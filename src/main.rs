use array2d::{Array2D};
use bevy::{
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    input::{keyboard::KeyCode, Input},
    input::mouse::{MouseMotion, MouseWheel},
    prelude::*,
    window::PresentMode
};
use rand::prelude::*;
use rand::distributions::Uniform;
use rand_distr::{Normal};
// use std::time::{Duration, Instant};

const CELL_SIZE: usize = 24;
const GRID_SIZE: usize = 32;
const MOVE_SPEED: f32 = 10.0;
const TARGET_POP: usize = (GRID_SIZE * GRID_SIZE / 3) as usize;
const CAM_SPEED: f32 = 400.0;
const INCUBATION_PERIOD: u32 = 15;
const EGG_EXPIRATION: u32 = 20;
const DEATH_RATE: f32 = 1.0 / 500.0;
const MUTATION_RATE: f32 = 0.01;
const FRAME_DELAY: u32 = 1;

#[derive(Component, Clone, Copy, Debug)]
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

#[derive(Bundle)]
struct Bird {
    age: Age,
    strategy: Strategy,
    genes: Genes,
    position: Position,
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
    BirdOnEgg(Entity), // bird entity
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
	    Cell::BirdOnEgg(_) => true,
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
	    Cell::BirdOnEgg(id) => Some(*id),
	    _ => None
	}
    }
}

#[derive(Resource)]
struct World {
    grid: Array2D<Cell>,
    pop: usize,
    frame_counter: u32,
    unif: Uniform<i32>,
    gauss: Normal<f32>
}

impl Default for World {
    fn default() -> World {
	World {
	    grid: Array2D::filled_with(Cell::Empty, GRID_SIZE, GRID_SIZE),
	    pop: 0,
	    frame_counter: 0,
	    unif: Uniform::from(0..3),
	    gauss: Normal::new(0.0, MUTATION_RATE).unwrap()
	}
    }
}

#[derive(Debug, Hash, PartialEq, Eq, Clone, SystemSet)]
enum Sets {
    Sim
}

fn main() {
    App::new()
	.add_plugins(DefaultPlugins.set(WindowPlugin {
            primary_window: Some(Window {
                title: "Guillemot".into(),
		present_mode: PresentMode::Immediate,
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
	.add_system(update_sim.in_set(Sets::Sim))
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
    use bevy::core_pipeline::clear_color::ClearColorConfig;
    commands.spawn(Camera2dBundle {
	camera_2d: Camera2d { clear_color:
			      ClearColorConfig::Custom(Color::rgba(0.0, 0.0, 0.0, 1.0)) },
	..default()
    });
    
    let mut rng = rand::thread_rng();
    let unif = Uniform::from(0..GRID_SIZE);

    let dim = (GRID_SIZE * CELL_SIZE) as f32;
    let border_color = Color::rgba(2.0 / 256.0,
				   48.0 / 256.0,
				   32.0 / 256.0,
				   0.75);
    commands.spawn(SpriteBundle {
        sprite: Sprite {
            // color: Color::rgba(0.0, 0.0, 0.0, 0.25),
	    color: Color::rgba(1.0, 1.0, 1.0, 0.075),
            custom_size: Some(Vec2::new(dim, dim)),
            ..default()
        },
        transform: Transform::from_translation(
	    Vec3::new(dim / 2.0 - CELL_SIZE as f32 / 2.0,
		      dim / 2.0 - CELL_SIZE as f32 / 2.0, 0.0)),
        ..default()
    });

    // commands.spawn(SpriteBundle {
    //     sprite: Sprite {
    //         color: border_color,
    //         custom_size: Some(Vec2::new(dim, CELL_SIZE as f32)),
    //         ..default()
    //     },
    //     transform: Transform::from_translation(
    // 	    Vec3::new(dim / 2.0 - CELL_SIZE as f32 / 2.0,
    // 		      dim, 0.0)),
    //     ..default()
    // });

    // commands.spawn(SpriteBundle {
    //     sprite: Sprite {
    //         color: border_color,
    //         custom_size: Some(Vec2::new(dim, CELL_SIZE as f32)),
    //         ..default()
    //     },
    //     transform: Transform::from_translation(
    // 	    Vec3::new(dim / 2.0 - CELL_SIZE as f32 / 2.0,
    // 		      - (CELL_SIZE as f32), 0.0)),
    //     ..default()
    // });
    
    // commands.spawn(SpriteBundle {
    //     sprite: Sprite {
    //         color: border_color,
    //         custom_size: Some(Vec2::new(CELL_SIZE as f32,
    // 					dim + (CELL_SIZE as f32) * 2.0)),
    //         ..default()
    //     },
    //     transform: Transform::from_translation(
    // 	    Vec3::new(- (CELL_SIZE as f32),
    // 		      dim / 2.0 - (CELL_SIZE as f32) / 2.0, 0.0)),
    //     ..default()
    // });

    // commands.spawn(SpriteBundle {
    //     sprite: Sprite {
    //         color: border_color,
    //         custom_size: Some(Vec2::new(CELL_SIZE as f32,
    // 					dim + (CELL_SIZE as f32) * 2.0)),
    //         ..default()
    //     },
    //     transform: Transform::from_translation(
    // 	    Vec3::new(dim,
    // 		      dim / 2.0 - (CELL_SIZE as f32) / 2.0, 0.0)),
    //     ..default()
    // });
    
    while world.pop < TARGET_POP {
	let i = unif.sample(&mut rng);
	let j = unif.sample(&mut rng);
	if world.grid.get(i, j).unwrap().is_empty() {
	    let bird_id = spawn_bird(Strategy::Altruistic,
				     // Genes(Vec3::new(1.0/3.0, 1.0/3.0, 1.0/3.0)),
				     Genes(Vec3::new(1.0, 0.0, 0.0)),
				     Position { x: i, y: j },
				     &mut commands, &mut world, &asset_server);
	    *world.grid.get_mut(i, j).unwrap() = Cell::Bird(bird_id)
	}
    }
}

fn update_camera(time: Res<Time>,
		 keyboard_input: Res<Input<KeyCode>>,
		 mut transforms: Query<(&mut Transform,
					&mut OrthographicProjection,
					With<Camera>)>,
		 // mut mouse_motion_events: EventReader<MouseMotion>,
		 mut scroll_evr: EventReader<MouseWheel>,
		 buttons: Res<Input<MouseButton>>,
		 mut motion_evr: EventReader<MouseMotion>) {
    // Get mutable ref to camera transform so we can change it.
    let (mut transform, mut projection, _) = transforms.iter_mut().next().unwrap();
    
    let mut translation = Vec3::ZERO;
    if keyboard_input.pressed(KeyCode::W) || keyboard_input.pressed(KeyCode::Up) {
    	translation += Vec3::Y
    }
    if keyboard_input.pressed(KeyCode::A) || keyboard_input.pressed(KeyCode::Left) {
    	translation -= Vec3::X
    }
    if keyboard_input.pressed(KeyCode::S) || keyboard_input.pressed(KeyCode::Down) {
    	translation -= Vec3::Y
    }
    if keyboard_input.pressed(KeyCode::D) || keyboard_input.pressed(KeyCode::Right) {
    	translation += Vec3::X
    }
    transform.translation += translation * CAM_SPEED * time.delta_seconds();
    
    if keyboard_input.pressed(KeyCode::PageUp) {
    	// transform.scale *= 1.0 + (0.1 * time.delta_seconds());
	projection.scale *= 1.0 - (1.0 * time.delta_seconds());
    }
    if keyboard_input.pressed(KeyCode::PageDown) {
    	// transform.scale *= 1.0 - (0.1 * time.delta_seconds());
	projection.scale *= 1.0 + (1.0 * time.delta_seconds());
    }

    // use bevy::input::mouse::MouseScrollUnit;
    for ev in scroll_evr.iter() {
	projection.scale *= 1.0 - ev.y * 0.1;
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

fn update_age(world: ResMut<World>,
	      mut eggs: Query<(&mut Age, Without<Strategy>)>,
	      mut birds: Query<(&mut Age, &Strategy)>) {
    if world.frame_counter % FRAME_DELAY != 0 {
    	return
    }
    for (mut age, _) in eggs.iter_mut() {
	age.0 += 1.0
    }
    for (mut age, strat) in birds.iter_mut() {
	match strat {
	    Strategy::Selfish => age.0 += 1.10,
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
    // let y2 = y;
    let z2 = f32::max(0.0, z + gauss.sample(rng));
    // let z2 = z;
    let c = x2 + y2 + z2;
    Genes(Vec3::new(x2 / c, y2 / c, z2 / c))
}

fn hatch(genes: &Genes, rng: &mut ThreadRng) -> Strategy {
    let Genes(Vec3 { x, y, .. }) = genes;
    let p: f32 = rng.gen();
    if p <= *x {
	Strategy::Altruistic
    } else if p <= *x + *y {
	Strategy::Selfish
    } else {
	Strategy::Cheater
    }
}

fn spawn_bird(strat: Strategy, genes: Genes, pos: Position,
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
	}}).id()
}

fn spawn_egg(genes: Genes, bird_pos: Position, pos: Position,
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
		translation: Vec3::new((bird_pos.x * CELL_SIZE) as f32,
				       (bird_pos.y * CELL_SIZE) as f32,
				       1.0),
		..default()
	    },
	    sprite: Sprite {
		custom_size: Some(Vec2::new(CELL_SIZE as f32 / 3.0,
					    CELL_SIZE as f32 / 3.0)),
		..default()
	    },
	    ..default()
	}}).id()
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

fn update_sim(mut commands: Commands,
	      mut world: ResMut<World>,
	      asset_server: Res<AssetServer>,
	      mut birds: Query<(Entity, &Age, &mut Position, &Strategy,
				&Genes, With<Sprite>)>,
	      mut eggs: Query<(Entity, &Age, &Position, &Genes,
	      		 With<Sprite>, Without<Strategy>)>) {
    if world.frame_counter % FRAME_DELAY != 0 {
    	return
    }
    
    let mut rng = rand::thread_rng();

    for (bird_id, Age(bird_age), mut bird_pos, strat, genes, _) in birds.iter_mut() {
	let cell = world.grid.get(bird_pos.x, bird_pos.y).unwrap();
	if cell.is_bird() {
	    if rng.gen::<f32>() <= DEATH_RATE * *bird_age as f32 {
		bird_death(bird_id, &bird_pos, &mut commands, &mut world)
	    } else if rng.gen::<f32>() <= birth_rate(world.pop) {
		match nearest_empty_cell(&world.grid, &bird_pos, &mut rng) {
		    Some(pos) => {
			let mutated_genes = mutate(genes, world.gauss, &mut rng);
			let egg_id = spawn_egg(mutated_genes, *bird_pos, pos,
					       &mut commands, &mut world, &asset_server);
			match strat {
			    Strategy::Selfish => {
				*world.grid.get_mut(bird_pos.x, bird_pos.y).unwrap() =
				    Cell::Empty;
				*bird_pos = pos;
				*world.grid.get_mut(pos.x, pos.y).unwrap() =
				    Cell::BirdOnEgg(bird_id)
			    },
			    _  =>
				*world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Egg(egg_id)
			}
		    },
		    None => ()
		}
	    } 
	    else {
		let x2 = (bird_pos.x as i32 + world.unif.sample(&mut rng) - 1)
		    .clamp(0, GRID_SIZE as i32 - 1) as usize;
		let y2 = (bird_pos.y as i32 + world.unif.sample(&mut rng) - 1)
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
				    Cell::BirdOnEgg(bird_id);
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
	    }
	} else if cell.is_bird_on_egg() {
	    let bird_id = cell.bird_id().unwrap();
	    if *egg_age >= INCUBATION_PERIOD as f32 {
		commands.entity(egg_id).despawn();
		world.pop -= 1;
		match nearest_empty_cell(&world.grid, egg_pos, &mut rng) {
		    Some(pos) => {
			let strat = hatch(genes, &mut rng);
			let chick_id = spawn_bird(strat, *genes, pos,
						  &mut commands, &mut world,
						  &asset_server);
			*world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Bird(chick_id)
		    },
		    None => () // No space, chick dies
		}
		*world.grid.get_mut(egg_pos.x, egg_pos.y).unwrap() = Cell::Bird(bird_id)
	    }
	} else {
	    panic!("expected egg at location {:?}", egg_pos)
	}
    }
}
