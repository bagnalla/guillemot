use array2d::{Array2D};
use bevy::{
    // app::CoreSchedule::FixedUpdate,
    diagnostic::{FrameTimeDiagnosticsPlugin, LogDiagnosticsPlugin},
    input::{keyboard::KeyCode, Input},
    input::mouse::{MouseMotion, MouseWheel},
    prelude::*,
    // time::fixed_timestep::FixedTime
};
// use bevy_framepace::*;
use rand::prelude::*;
use rand::distributions::Uniform;

const CELL_SIZE: usize = 24;
const GRID_SIZE: usize = 64;
const INCUBATION_PERIOD: u32 = 30;
const MOVE_SPEED: f32 = 5.0;
const TARGET_POP: u32 = (GRID_SIZE * GRID_SIZE / 4) as u32;
const CAM_SPEED: f32 = 200.0;

#[derive(Component)]
enum Strategy {
    Altruistic,
    Selfish,
    Cheater
}

#[derive(Component)]
struct Age(u32);

#[derive(Component)]
struct Genes(Vec3);

#[derive(Component)]
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
    BirdOnEgg { bird: Entity, egg: Entity, time_elapsed: f64 },
    Empty
}

#[derive(Component)]
struct Camera;

#[derive(Resource)]
struct World {
    cam: Option<Entity>,
    grid: Array2D<Cell>,
    frame_counter: u32
}

impl Default for World {
    fn default() -> World {
	World {
	    cam: None,
	    grid: Array2D::filled_with(Cell::Empty, GRID_SIZE, GRID_SIZE),
	    frame_counter: 0
	}
    }
}

fn main() {
    // CoreSchedule::FixedUpdate.add_system(update_sim);
    
    App::new()
        .add_plugins(DefaultPlugins)
	// .add_plugin(bevy_framepace::FramepacePlugin)
	.add_plugin(LogDiagnosticsPlugin::default())
	.add_plugin(FrameTimeDiagnosticsPlugin::default())
	.init_resource::<World>()
        .add_startup_system(setup)
	.add_system(update_camera)
	.add_system(move_sprites)
	// .add_system_set(
        //     SystemSet::new()
        //         .with_run_criteria(FixedTimestep::step(1.0 / 5.0))
        //         .with_system(update_sim)
    // )
	// .add_system(FixedUpdate, update_sim)
    // .insert_resource(FixedTime::new_from_secs(1.0 / 5.0))
	.add_system(update_sim)
	// .add_system(run_fixed_timestep)
        .run();
}

fn setup(mut commands: Commands,
	 mut world: ResMut<World>,
	 // mut fs: ResMut<FramepaceSettings>,
	 asset_server: Res<AssetServer>) {
    // fs.limiter = Limiter::from_framerate(10.0);
    
    commands.spawn(Camera2dBundle::default());
    
    world.cam = Some(commands.spawn((Camera, SpatialBundle::default())
    ).with_children(|u| {
	// u.spawn(SpriteBundle {
	//     texture: asset_server.load("cheater.png"),
	//     ..default()
	// });

	let mut rng = rand::thread_rng();
	let unif = Uniform::from(0..GRID_SIZE);
	
	let mut birds: u32 = 0;
	while birds < TARGET_POP {
	    let i = unif.sample(&mut rng);
	    let j = unif.sample(&mut rng);
	    let mut cell = world.grid.get_mut(i, j).unwrap();
	    match cell {
		Cell::Empty => {
		    let bird_id = u.spawn(Bird {
			age: Age(0),
			strategy: Strategy::Altruistic,
			genes: Genes(Vec3::new(1.0/3.0,
					       1.0/3.0,
					       1.0/3.0)),
			position: Position { x:i, y:j },
			sprite: SpriteBundle {
			    texture: asset_server.load("altruist.png"),
			    transform: Transform {
				translation: Vec3::new((i * CELL_SIZE) as f32,
						       (j * CELL_SIZE) as f32,
						       0.0),
				..default()
			    },
			    sprite: Sprite {
				custom_size: Some(Vec2::new(CELL_SIZE as f32,
							    CELL_SIZE as f32)),
				..default()
			    },
			    ..default()
			}}).id();
		    *cell = Cell::Bird(bird_id);
		    birds += 1
		}
		_ => continue
	    }
	}
    }).id());
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

    use bevy::input::mouse::MouseScrollUnit;
    for ev in scroll_evr.iter() {
	transform.scale *= 1.0 + ev.y * 0.1;
    }

    // This doesn't work for some reason (delta should be the
    // difference in mouse position but currently just gives the
    // coordinates of the mouse in screen space.
    // for ev in motion_evr.iter() {
    // 	if buttons.pressed(MouseButton::Left) {
    // 	    // println!("Mouse moved: X: {} px, Y: {} px", ev.delta.x, ev.delta.y);
    // 	    transform.translation += Vec3::new(ev.delta.x, ev.delta.y, 0.0)
    // 	}
    // }
}

fn move_sprites(time: Res<Time>,
	       mut query: Query<(&mut Transform, &Position)>) {
    for (mut transform, Position { x, y }) in query.iter_mut() {
	// info!("moving {:?}", transform);
	let v = Vec3::new((CELL_SIZE * x) as f32,
			  (CELL_SIZE * y) as f32,
			  transform.translation.z) - transform.translation;
	transform.translation += v * MOVE_SPEED * time.delta_seconds();
    }
}

// fn usize_minus(n: usize, m: usize) -> usize {
//     if n <= m {
// 	0
//     } else {
// 	n - m
//     }
// }

fn update_sim(time: Res<Time>,
	      mut world: ResMut<World>,
	      mut birds: Query<(Entity, &mut Position, &Strategy, With<Sprite>)>,
	      mut eggs: Query<(&mut Transform, With<Sprite>, Without<Strategy>)>) {
    world.frame_counter += 1;
    if world.frame_counter < 1 {
	return
    }
    world.frame_counter = 0;
    
    let mut rng = rand::thread_rng();
    let unif = Uniform::from(0..3);
    
    for (id, mut pos, strat, _) in birds.iter_mut() {
	// let x2 = (pos.x + usize_minus(unif.sample(&mut rng), 1))
	//     .clamp(0, GRID_SIZE - 1);
	// let y2 = (pos.y + usize_minus(unif.sample(&mut rng), 1))
	//     .clamp(0, GRID_SIZE - 1);
	let x2 = (pos.x as i32 + unif.sample(&mut rng) - 1)
	    .clamp(0, GRID_SIZE as i32 - 1) as usize;
	let y2 = (pos.y as i32 + unif.sample(&mut rng) - 1)
	    .clamp(0, GRID_SIZE as i32 - 1) as usize;
	match world.grid.get(x2, y2).unwrap() {
	    Cell::Empty => {
		*world.grid.get_mut(pos.x, pos.y).unwrap() = Cell::Empty;
		*world.grid.get_mut(x2, y2).unwrap() = Cell::Bird(id);
		pos.x = x2;
		pos.y = y2
	    },
	    Cell::Bird(_) => (),
	    _ => todo!()
	}
    }
}
    
// fn update_sim(time: Res<Time>,
// 	      mut world: ResMut<World>,
// 	      mut query: Query<(&mut Transform, With<Sprite>)>) {
//     let mut rng = rand::thread_rng();
//     let unif = Uniform::from(0..3);
    
//     for i in 0..GRID_SIZE {
// 	for j in 0..GRID_SIZE {
// 	    // info!("{} {}", world.grid.num_rows(), world.grid.num_columns());
// 	    // info!("{} {}", i, j);
// 	    match world.grid.get(i, j).unwrap() {
// 	    	Cell::Bird(id) => {
// 	    	    let x = (i + usize_minus(unif.sample(&mut rng), 1))
// 			.clamp(0, GRID_SIZE - 1);
// 		    let y = (j + usize_minus(unif.sample(&mut rng), 1))
// 			.clamp(0, GRID_SIZE - 1);
// 	    	    // info!("bakow");
// 	    	    match world.grid.get(x, y).unwrap() {
// 	    	    	Cell::Empty => {
// 			    *world.grid.get_mut(x, y).unwrap() = Cell::Empty,
// 			    *world.grid.get_mut(x, y).unwrap() = Cell::Empty
// 			},
// 	    	    	_ => ()
// 	    	    }
// 	    	},
// 	    	Cell::Egg(id) =>
// 	    	    todo!(),
// 	    	Cell::BirdOnEgg { bird: bird_id,
// 	    			  egg: egg_id,
// 	    			  time_elapsed: time } =>
// 	    	    todo!(),
// 	    	Cell::Empty => ()
// 	    }
// 	}
//     }
// }
