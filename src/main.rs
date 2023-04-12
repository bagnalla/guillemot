use bevy::prelude::*;
use array2d::{Array2D};

const GRID_SIZE: usize = 1024;

#[derive(Component)]
enum BirdStrategy {
    Altruistic,
    Selfish,
    Cheater
}

#[derive(Component)]
struct Genes(Vec3);

#[derive(Component)]
struct Coordinates { x: usize, y: usize }

#[derive(Bundle)]
struct Bird {
    strategy: BirdStrategy,
    genes: Genes,
    position: Coordinates
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
	    texture: asset_server.load("guillemot.png"),
	    ..default()
	});
    }).id());

    // info!();
}

fn move_system(time: Res<Time>,
	       mut query: Query<(&mut Transform, With<Sprite>)>) {
    for (mut transform, _) in query.iter_mut() {
	info!("moving {:?}", transform);
	transform.translation += Vec3::new(10.0, 10.0, 0.0) * time.delta_seconds();
	// transform.scale += Vec3::new(0.1, 0.1, 0.0)
    }
}
