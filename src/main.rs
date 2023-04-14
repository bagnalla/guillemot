use array2d::{Array2D};
use bevy::{
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    input::{keyboard::KeyCode, Input},
    input::mouse::{MouseMotion, MouseWheel},
    prelude::*,
};
use rand::prelude::*;
use rand::distributions::Uniform;
use rand_distr::{Normal};

const CELL_SIZE: usize = 24;
const GRID_SIZE: usize = 128;
const MOVE_SPEED: f32 = 10.0;
const TARGET_POP: usize = (GRID_SIZE * GRID_SIZE / 2) as usize;
const CAM_SPEED: f32 = 200.0;
const INCUBATION_PERIOD: u32 = 15;
const EGG_EXPIRATION: u32 = 20;
const DEATH_RATE: f32 = 1.0 / 500.0;
const MUTATION_RATE: f32 = 0.02;

#[derive(Component, Clone, Copy)]
enum Strategy {
    Altruistic,
    Selfish,
    Cheater
}

#[derive(Component, Clone, Copy)]
struct Age(f32);

#[derive(Component, Clone, Copy)]
struct Genes(Vec3);

#[derive(Component, Clone, Copy, Debug)]
struct Position { x: usize, y: usize }

// #[derive(Component, Clone, Copy)]
// struct Alive(bool);

#[derive(Bundle)]
struct Bird {
    age: Age,
    strategy: Strategy,
    genes: Genes,
    position: Position,
    // alive: Alive,
    sprite: SpriteBundle
}

#[derive(Bundle)]
struct Egg {
    age: Age,
    genes: Genes,
    position: Position,
    sprite: SpriteBundle
}

#[derive(Debug, Clone)]
enum Cell {
    Bird(Entity),
    Egg(Entity),
    BirdOnEgg { bird: Entity, egg: Entity },
    Empty
}

impl Cell {
    fn is_bird(&self) -> bool {
	match self {
	    Cell::Bird(_) => true,
	    _ => false
	}
    }
    fn is_egg(&self) -> bool {
	match self {
	    Cell::Egg(_) => true,
	    _ => false
	}
    }
    fn is_bird_on_egg(&self) -> bool {
	match self {
	    Cell::BirdOnEgg { .. } => true,
	    _ => false
	}
    }
    fn is_empty(&self) -> bool {
	match self {
	    Cell::Empty => true,
	    _ => false
	}
    }
    fn bird_id(&self) -> Option<Entity> {
	match self {
	    Cell::Bird(id) => Some(*id),
	    Cell::BirdOnEgg { bird: id, .. } => Some(*id),
	    _ => None
	}
    }
}

#[derive(Component)]
struct Camera;

#[derive(Resource)]
struct World {
    cam: Option<Entity>,
    grid: Array2D<Cell>,
    pop: usize,
    frame_counter: u32
}

impl Default for World {
    fn default() -> World {
	World {
	    cam: None,
	    grid: Array2D::filled_with(Cell::Empty, GRID_SIZE, GRID_SIZE),
	    pop: 0,
	    frame_counter: 0
	}
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
enum Sets {
    Move,
    Sim
}

fn main() {
    App::new()
	.add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "Guillemot".into(),
                ..default()
            }),
            ..default()
        }))
        // .add_plugins(DefaultPlugins)
	.add_plugin(LogDiagnosticsPlugin::default())
	.add_plugin(FrameTimeDiagnosticsPlugin::default())
	.init_resource::<World>()
        .add_startup_system(setup)
	.add_system(update_camera)
	.add_system(move_sprites)
	.add_system(update_birds.in_set(Sets::Sim))
	// .add_system(update_eggs.in_set(Sets::Sim))
	.add_system(update_age.after(Sets::Sim))
	.add_system(update_frame_counter.after(Sets::Sim))
        .run();
}

fn nearest_empty_cell(grid: &Array2D<Cell>, pos: &Position, rng: &mut ThreadRng)
		      -> Option<Position> {
    let mut ps = [(pos.x-1, pos.y-1), (pos.x, pos.y-1), (pos.x+1, pos.y-1),
		  (pos.x-1, pos.y), (pos.x, pos.y), (pos.x+1, pos.y),
		  (pos.x-1, pos.y+1), (pos.x, pos.y+1), (pos.x+1, pos.y+1)];
    ps.shuffle(rng);
    for (x, y) in ps {
	match grid.get(x, y) {
	    Some(Cell::Empty) => return Some(Position { x: x, y: y }),
	    _ => ()
	}
    }
    None
}

fn setup(mut commands: Commands,
	 mut world: ResMut<World>,
	 asset_server: Res<AssetServer>) {
    commands.spawn(Camera2dBundle::default());

    let cam = commands.spawn((Camera, SpatialBundle::default())).id();
    let mut rng = rand::thread_rng();
    let unif = Uniform::from(0..GRID_SIZE);
    
    while world.pop < TARGET_POP {
	let i = unif.sample(&mut rng);
	let j = unif.sample(&mut rng);
	if world.grid.get(i, j).unwrap().is_empty() {
	    let bird_id = spawn_bird(Strategy::Altruistic,
				     // Genes(Vec3::new(1.0/3.0, 1.0/3.0, 1.0/3.0)),
				     Genes(Vec3::new(1.0, 0.0, 0.0)),
				     Position { x: i, y: j },
				     cam, &mut commands, &mut world, &asset_server);
	    *world.grid.get_mut(i, j).unwrap() = Cell::Bird(bird_id)
	}
    }

    world.cam = Some(cam)
}

fn update_camera(time: Res<Time>,
		 keyboard_input: Res<Input<KeyCode>>,
		 mut transforms: Query<(&mut Transform, With<Camera>)>,
		 mut mouse_motion_events: EventReader<MouseMotion>,
		 mut scroll_evr: EventReader<MouseWheel>,
		 buttons: Res<Input<MouseButton>>,
		 mut motion_evr: EventReader<MouseMotion>) {
    // Get mutable ref to camera transform so we can change it.
    let (mut transform, _) = transforms.iter_mut().next().unwrap();
    
    let mut translation = Vec3::ZERO;
    if keyboard_input.pressed(KeyCode::W) || keyboard_input.pressed(KeyCode::Up) {
    	translation -= Vec3::Y
    }
    if keyboard_input.pressed(KeyCode::A) || keyboard_input.pressed(KeyCode::Left) {
    	translation += Vec3::X
    }
    if keyboard_input.pressed(KeyCode::S) || keyboard_input.pressed(KeyCode::Down) {
    	translation += Vec3::Y
    }
    if keyboard_input.pressed(KeyCode::D) || keyboard_input.pressed(KeyCode::Right) {
    	translation -= Vec3::X
    }
    transform.translation += translation * CAM_SPEED * time.delta_seconds();
    
    if keyboard_input.pressed(KeyCode::PageUp) {
    	transform.scale *= 1.1;
    }
    if keyboard_input.pressed(KeyCode::PageDown) {
    	transform.scale *= 0.9;
    }

    // use bevy::input::mouse::MouseScrollUnit;
    for ev in scroll_evr.iter() {
	transform.scale *= 1.0 + ev.y * 0.1;
    }

    // This doesn't work for some reason in the Virtualbox Ubuntu VM
    // (delta should be the difference in mouse position but currently
    // just gives the coordinates of the mouse in screen space.
    // for ev in motion_evr.iter() {
    // 	if buttons.pressed(MouseButton::Left) {
    // 	    // println!("Mouse moved: X: {} px, Y: {} px", ev.delta.x, ev.delta.y);
    // 	    transform.translation += Vec3::new(ev.delta.x, -ev.delta.y, 0.0)
    // 	}
    // }
}

fn move_sprites(time: Res<Time>,
		mut query: Query<(&mut Transform, &Position)>) {
    for (mut transform, Position { x, y }) in query.iter_mut() {
	let v = Vec3::new((CELL_SIZE * x) as f32,
			  (CELL_SIZE * y) as f32,
			  transform.translation.z) - transform.translation;
	transform.translation += v * MOVE_SPEED * time.delta_seconds();
    }
}

fn update_age(mut eggs: Query<(&mut Age, Without<Strategy>)>,
	      mut birds: Query<(&mut Age, &Strategy)>) {
    for (mut age, _) in eggs.iter_mut() {
	age.0 += 1.0
    }
    for (mut age, strat) in birds.iter_mut() {
	match strat {
	    Strategy::Selfish => age.0 += 1.5,
	    _ => age.0 += 1.0
	}
    }
}

fn birth_rate(current_pop: usize) -> f32 {
    // 0.0 // calculate based on current_pop, TARGET_POP, and DEATH_RATE
    // (TARGET_POP - current_pop + current_pop * DEATH_RATE) / (current_pop - )
    let br = (TARGET_POP as f32 + current_pop as f32 * (DEATH_RATE - 1.0)) /
	(current_pop as f32 * (1.0 - DEATH_RATE));
    // info!("current_pop: {}", current_pop);
    // info!("birth rate: {}", br);
    br
}

fn mutate(genes: &Genes,
	  gauss: rand_distr::Normal<f32>, rng: &mut ThreadRng) -> Genes {
    let Genes(Vec3 { x, y, z }) = genes;
    let x2 = f32::max(0.0, x + gauss.sample(rng));
    let y2 = f32::max(0.0, y + gauss.sample(rng));
    let z2 = f32::max(0.0, z + gauss.sample(rng));
    let c = x2 + y2 + z2;
    Genes(Vec3::new(x2 / c, y2 / c, z2 / c))
}

fn hatch(pos: &Position, genes: &Genes, rng: &mut ThreadRng) -> Strategy {
    let Genes(Vec3 { x, y, z }) = genes;
    let p: f32 = rng.gen();
    if p <= *x {
	Strategy::Altruistic
    } else if p <= *x + *y {
	Strategy::Selfish
    } else {
	Strategy::Cheater
    }
}

fn spawn_bird(strat: Strategy, genes: Genes, pos: Position, cam: Entity,
	      commands: &mut Commands, world: &mut ResMut<World>,
	      asset_server: &Res<AssetServer>) -> Entity {
    world.pop += 1;
    commands.spawn(Bird {
	age: Age(0.0),
	strategy: strat,
	genes: genes,
	position: pos,
	// alive: Alive(true),
	sprite: SpriteBundle {
	    texture: asset_server.load(match strat {
		Strategy::Altruistic => "altruist.png",
		Strategy::Selfish => "selfish.png",
		Strategy::Cheater => "cheater.png"
	    }),
	    transform: Transform {
		translation: Vec3::new((pos.x * CELL_SIZE) as f32,
				       (pos.y * CELL_SIZE) as f32,
				       0.0),
		..default()
	    },
	    sprite: Sprite {
		custom_size: Some(Vec2::new(CELL_SIZE as f32,
					    CELL_SIZE as f32)),
		..default()
	    },
	    ..default()
	}}).set_parent(cam).id()
}

fn spawn_egg(genes: Genes, pos: Position, cam: Entity,
	     commands: &mut Commands, world: &mut World,
	     asset_server: &Res<AssetServer>) -> Entity {
    world.pop += 1;
    commands.spawn(Egg {
	age: Age(0.0),
	genes: genes,
	position: pos,
	sprite: SpriteBundle {
	    texture: asset_server.load("egg.png"),
	    transform: Transform {
		// translation: Vec3::new((pos.x * CELL_SIZE - CELL_SIZE / 2) as f32,
		// 		       (pos.y * CELL_SIZE - CELL_SIZE / 2) as f32,
		translation: Vec3::new((pos.x * CELL_SIZE) as f32,
				       (pos.y * CELL_SIZE) as f32,
				       1.0),
		..default()
	    },
	    sprite: Sprite {
		custom_size: Some(Vec2::new(CELL_SIZE as f32 / 3.0,
					    CELL_SIZE as f32 / 3.0)),
		..default()
	    },
	    ..default()
	}}).set_parent(cam).id()
}

fn update_frame_counter(mut world: ResMut<World>) {
    world.frame_counter += 1;
}

fn bird_death(id: Entity, pos: &Position,
	      commands: &mut Commands, world: &mut ResMut<World>) {
    // info!("bird {:?} died at {:?}", id, pos);
    commands.entity(id).despawn();
    *world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Empty;
    world.pop -= 1;
}

fn egg_death(id: Entity, pos: &Position,
	      commands: &mut Commands, world: &mut ResMut<World>) {
    // info!("egg {:?} died at {:?}", id, pos);
    commands.entity(id).despawn();
    *world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Empty;
    world.pop -= 1;
}

fn update_birds(time: Res<Time>,
		mut commands: Commands,
		mut world: ResMut<World>,
		asset_server: Res<AssetServer>,
		mut birds: Query<(Entity, &Age, &mut Position, &Strategy,
				  &Genes, With<Sprite>)>,
		mut eggs: Query<(Entity, &Age, &Position, &Genes,
				 With<Sprite>, Without<Strategy>)>) {
    if world.frame_counter % 1 != 0 {
	return
    }

    // info!("world.pop: {:?}", world.pop);
    
    let mut rng = rand::thread_rng();
    let unif = Uniform::from(0..3);
    let gauss = Normal::new(0.0, MUTATION_RATE).unwrap();
    let cam = world.cam.unwrap();

    for (bird_id, Age(bird_age), mut bird_pos, strat, genes, _) in birds.iter_mut() {
	// if !alive.0 {
	//     continue
	// }
	let cell = world.grid.get(bird_pos.x, bird_pos.y).unwrap();
	if cell.is_bird() {
	    if rng.gen::<f32>() <= DEATH_RATE * *bird_age as f32 {
		bird_death(bird_id, &bird_pos, &mut commands, &mut world)
	    } else if rng.gen::<f32>() <= birth_rate(world.pop) {
		match nearest_empty_cell(&world.grid, &bird_pos, &mut rng) {
		    Some(pos) => {
			let mutated_genes = mutate(genes, gauss, &mut rng);
			let egg_id = spawn_egg(mutated_genes, pos, cam,
					       &mut commands, &mut world, &asset_server);
			match strat {
			    Strategy::Selfish => {
				*world.grid.get_mut(bird_pos.x, bird_pos.y).unwrap() =
				    Cell::Empty;
				*bird_pos = pos;
				*world.grid.get_mut(pos.x, pos.y).unwrap() =
				    Cell::BirdOnEgg { bird: bird_id, egg: egg_id }
			    },
			    _  =>
				*world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Egg(egg_id)
			}
		    },
		    None => ()
		}
	    } 
	    else {
		let x2 = (bird_pos.x as i32 + unif.sample(&mut rng) - 1)
		    .clamp(0, GRID_SIZE as i32 - 1) as usize;
		let y2 = (bird_pos.y as i32 + unif.sample(&mut rng) - 1)
		    .clamp(0, GRID_SIZE as i32 - 1) as usize;
		match world.grid.get(x2, y2).unwrap() {
		    Cell::Empty => {
			*world.grid.get_mut(bird_pos.x, bird_pos.y).unwrap() = Cell::Empty;
			*world.grid.get_mut(x2, y2).unwrap() = Cell::Bird(bird_id);
			bird_pos.x = x2;
			bird_pos.y = y2
		    },
		    Cell::Egg(egg_id) => {
			let egg_id = *egg_id;
			match strat {
			    Strategy::Altruistic => {
				*world.grid.get_mut(bird_pos.x, bird_pos.y).unwrap() =
				    Cell::Empty;
				*world.grid.get_mut(x2, y2).unwrap() =
				    Cell::BirdOnEgg { bird: bird_id, egg: egg_id };
				bird_pos.x = x2;
				bird_pos.y = y2
			    },
			    _ => ()
			}
		    },
		    _ => ()
		}
	    }
	} else if cell.is_bird_on_egg() {
	    ()
	} else {
	    panic!("expected bird {:?} at location {:?}, got {:?}", bird_id, bird_pos, cell)
	}
    }

    for (egg_id, Age(egg_age), egg_pos, genes, _, _) in eggs.iter_mut() {
	let cell = world.grid.get(egg_pos.x, egg_pos.y).unwrap();
	if cell.is_egg() {
	    if *egg_age >= EGG_EXPIRATION as f32 {
		egg_death(egg_id, egg_pos, &mut commands, &mut world)
		// commands.entity(egg_id).despawn();
		// *world.grid.get_mut(egg_pos.x, egg_pos.y).unwrap() = Cell::Empty
	    }
	} else if cell.is_bird_on_egg() {
	    let bird_id = cell.bird_id().unwrap();
	    if *egg_age >= INCUBATION_PERIOD as f32 {
		commands.entity(egg_id).despawn();
		world.pop -= 1;
		match nearest_empty_cell(&world.grid, egg_pos, &mut rng) {
		    Some(pos) => {
			let strat = hatch(&pos, genes, &mut rng);
			let chick_id = spawn_bird(strat, *genes, pos,
						  cam, &mut commands, &mut world,
						  &asset_server);
			*world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Bird(chick_id)
		    },
		    None => () // No space, chick dies
		}
		// if rng.gen::<f32>() <= DEATH_RATE * INCUBATION_PERIOD as f32 {
		//     bird_death(bird_id, egg_pos, &mut birds.get_mut(bird_id).unwrap(),
		// 	       &mut commands, &mut world)
		// } else {
		*world.grid.get_mut(egg_pos.x, egg_pos.y).unwrap() = Cell::Bird(bird_id)
		// }
	    }
	} else {
	    panic!("expected egg at location {:?}", egg_pos)
	}
    }
}

// fn update_eggs(time: Res<Time>,
// 	       mut commands: Commands,
// 	       mut world: ResMut<World>,
// 	       asset_server: Res<AssetServer>,
// 	       mut eggs: Query<(Entity, &Age, &Position, &Genes,
// 				With<Sprite>, Without<Strategy>)>,
// 	       // mut birds: Query<&mut Alive>
// ) {
//     if world.frame_counter % 1 != 0 {
// 	return
//     }
    
//     let mut rng = rand::thread_rng();
//     let unif = Uniform::from(0..3);
//     let gauss = Normal::new(0.0, 0.01).unwrap();
//     let cam = world.cam.unwrap();
    
//     for (egg_id, Age(egg_age), egg_pos, genes, _, _) in eggs.iter_mut() {
// 	let cell = world.grid.get(egg_pos.x, egg_pos.y).unwrap();
// 	if cell.is_egg() {
// 	    if *egg_age >= EGG_EXPIRATION {
// 		commands.entity(egg_id).despawn();
// 		*world.grid.get_mut(egg_pos.x, egg_pos.y).unwrap() = Cell::Empty
// 	    }
// 	} else if cell.is_bird_on_egg() {
// 	    let bird_id = cell.bird_id().unwrap();
// 	    if *egg_age >= INCUBATION_PERIOD {
// 		commands.entity(egg_id).despawn();
// 		match nearest_empty_cell(&world.grid, egg_pos) {
// 		    Some(pos) => {
// 			let strat = hatch(&pos, genes, &mut rng);
// 			let chick_id = spawn_bird(strat, *genes, pos,
// 						  cam, &mut commands, &mut world,
// 						  &asset_server);
// 			*world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Bird(chick_id)
// 		    },
// 		    None => () // No space, chick dies
// 		}
// 		// if rng.gen::<f32>() <= DEATH_RATE * INCUBATION_PERIOD as f32 {
// 		//     bird_death(bird_id, egg_pos, &mut birds.get_mut(bird_id).unwrap(),
// 		// 	       &mut commands, &mut world)
// 		// } else {
// 		*world.grid.get_mut(egg_pos.x, egg_pos.y).unwrap() = Cell::Bird(bird_id)
// 		// }
// 	    }
// 	} else {
// 	    panic!("expected egg at location {:?}", egg_pos)
// 	}
//     }
// }
