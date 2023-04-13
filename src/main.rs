use array2d::{Array2D};
use bevy::prelude::*;
use rand::prelude::*;
use rand::distributions::Uniform;

const CELL_SIZE: usize = 24;
const GRID_SIZE: usize = 1024;
const INCUBATION_PERIOD: u32 = 30;
const MOVE_SPEED: f32 = 100.0;
const TARGET_POP: u32 = 250000;

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

#[derive(Resource)]
struct World {
    cam: Option<Entity>,
    grid: Array2D<Cell>
}

impl Default for World {
    fn default() -> World {
	World {
	    cam: None,
	    grid: Array2D::filled_with(Cell::Empty, GRID_SIZE, GRID_SIZE)
	}
    }
}

fn main() {
    App::new()
        .add_plugins(DefaultPlugins)
	.init_resource::<World>()
        .add_startup_system(setup)
	.add_system(update_system)
	.add_system(move_system)
        .run();
}

fn setup(mut commands: Commands,
	 mut world: ResMut<World>,
	 asset_server: Res<AssetServer>) {
    commands.spawn(Camera2dBundle::default());
    
    world.cam = Some(commands.spawn(SpatialBundle::default()
    ).with_children(|u| {
	u.spawn(SpriteBundle {
	    texture: asset_server.load("cheater.png"),
	    ..default()
	});

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
			position: Position { x: i, y: j },
			sprite: SpriteBundle {
			    texture: asset_server.load("altruistic.png"),
			    ..default()
			}}).id();
		    *cell = Cell::Bird(bird_id);
		    birds += 1
		}
		_ => continue
	    }
	}
    }).id());

    // info!();
}

fn move_system(time: Res<Time>,
	       mut query: Query<(&mut Transform, &Position)>) {
    for (mut transform, Position { x:x, y:y }) in query.iter_mut() {
	// info!("moving {:?}", transform);
	let v = Vec3::new((CELL_SIZE * x) as f32,
			  (CELL_SIZE * y) as f32,
			  transform.translation.z) - transform.translation;
	transform.translation += v * MOVE_SPEED * time.delta_seconds();
    }
}

fn update_system(time: Res<Time>,
	  mut world: ResMut<World>,
	  mut query: Query<(&mut Transform, With<Sprite>)>) {
    for i in 0..GRID_SIZE {
	for j in 0..GRID_SIZE {
	    match world.grid.get_mut(i, j).unwrap() {
		Cell::Bird(id) =>
		    todo!(),
		Cell::Egg(id) =>
		    todo!(),
		Cell::BirdOnEgg { bird: bird_id,
				  egg: egg_id,
				  time_elapsed: time } =>
		    todo!(),
		Cell::Empty => ()
	    }
	}
    }
}
