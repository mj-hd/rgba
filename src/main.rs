use env_logger::{Builder, Target};
use log::debug;
use pixels::{Pixels, SurfaceTexture};
use rgba::rom::Rom;
use std::{
    env,
    fs::File,
    io::BufReader,
    time::{Duration, Instant},
};
use winit::{
    dpi::LogicalSize,
    event::{Event, VirtualKeyCode, WindowEvent},
    event_loop::{ControlFlow, EventLoop},
    window::WindowBuilder,
};
use winit_input_helper::WinitInputHelper;

fn main() {
    let mut builder = Builder::from_default_env();
    builder.target(Target::Stdout);

    builder.init();

    let event_loop = EventLoop::new();
    let mut input = WinitInputHelper::new();

    let size = LogicalSize::new(240, 160);
    let window = WindowBuilder::new()
        .with_title("rgba")
        .with_inner_size(size)
        .with_min_inner_size(size)
        .build(&event_loop)
        .unwrap();

    let window_size = window.inner_size();
    let surface_texture = SurfaceTexture::new(window_size.width, window_size.height, &window);
    let mut pixels = Pixels::new(240, 160, surface_texture).unwrap();

    let args = env::args().collect::<Vec<String>>();

    let mut reader = BufReader::new(File::open(args[1].clone()).unwrap());
    let rom = Rom::new(&mut reader).unwrap();

    debug!("ROM: {:?}", rom);

    let mut time = Instant::now();

    event_loop.run(move |event, _, control_flow| {
        match event {
            Event::WindowEvent {
                event: WindowEvent::CloseRequested,
                ..
            } => {
                *control_flow = ControlFlow::Exit;
            }
            Event::RedrawRequested(_) => {
                pixels.render().unwrap();
            }
            _ => {}
        }

        match *control_flow {
            ControlFlow::Exit => {}
            _ => {
                if time.elapsed() >= Duration::from_millis(1000 / 60) {
                    time = Instant::now();

                    window.request_redraw();
                }

                if input.update(&event) {
                    if input.key_pressed(VirtualKeyCode::Escape) || input.quit() {
                        *control_flow = ControlFlow::Exit;
                        return;
                    }

                    if let Some(size) = input.window_resized() {
                        pixels.resize_surface(size.width, size.height)
                    }
                }

                *control_flow = ControlFlow::Poll;
            }
        }
    });
}
