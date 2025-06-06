use std::{
    mem,
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
    time::Instant,
};

use clap::Parser;
use fabricator as fab;
use fabricator_desktop::{
    geometry::{Geometry, GpuGeometry},
    pipeline,
};
use fabricator_math::{Affine2, Box2, Vec2, cast};
use fabricator_util::typed_id_map::SecondaryMap;
use winit::{
    application::ApplicationHandler,
    event::{MouseButton, WindowEvent},
    event_loop::{ActiveEventLoop, ControlFlow, EventLoop},
    window::{Window, WindowId},
};

struct AppState {
    window: Arc<Window>,
    device: wgpu::Device,
    queue: wgpu::Queue,
    size: winit::dpi::PhysicalSize<u32>,
    surface: wgpu::Surface<'static>,
    surface_format: wgpu::TextureFormat,

    input: fab::InputState,
    game: fab::Game,
    render: fab::Render,

    pipeline: pipeline::Pipeline,
    texture_page_bind_groups: SecondaryMap<fab::TexturePageId, wgpu::BindGroup>,
    parameters_buffer: wgpu::Buffer,
    parameters_bind_group: wgpu::BindGroup,
    textures: SecondaryMap<fab::TextureId, (fab::TexturePageId, Vec2<f32>, Box2<f32>)>,
    geometry: Geometry<pipeline::Vertex, u32>,
    batches: Vec<(Range<u32>, fab::TexturePageId)>,
    last_render: Instant,
    frames_behind: f64,
}

impl AppState {
    async fn new(window: Arc<Window>, project_file: &Path) -> AppState {
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor::default());
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions::default())
            .await
            .unwrap();
        let (device, queue) = adapter
            .request_device(&wgpu::DeviceDescriptor::default())
            .await
            .unwrap();

        let size = window.inner_size();

        let surface = instance.create_surface(window.clone()).unwrap();
        let cap = surface.get_capabilities(&adapter);
        let surface_format = cap.formats[0];

        let game = fab::Game::new(fab::Project::load(&project_file).unwrap()).unwrap();

        let pipeline = pipeline::Pipeline::new(&device, surface_format.add_srgb_suffix());

        let mut texture_page_bind_groups =
            SecondaryMap::<fab::TexturePageId, wgpu::BindGroup>::new();

        let parameters_buffer = device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("UI element parameters buffer"),
            usage: wgpu::BufferUsages::UNIFORM | wgpu::BufferUsages::COPY_DST,
            size: mem::size_of::<pipeline::ParametersUniform>() as u64,
            mapped_at_creation: false,
        });

        let parameters_bind_group = pipeline
            .create_parameters_bind_group(&device, parameters_buffer.as_entire_buffer_binding());

        let mut textures =
            SecondaryMap::<fab::TextureId, (fab::TexturePageId, Vec2<f32>, Box2<f32>)>::new();

        for (page_id, page) in game.texture_pages() {
            let page_texture = device.create_texture(&wgpu::TextureDescriptor {
                label: Some("page texture"),
                size: wgpu::Extent3d {
                    width: page.size[0],
                    height: page.size[1],
                    depth_or_array_layers: 1,
                },
                mip_level_count: 1,
                sample_count: 1,
                dimension: wgpu::TextureDimension::D2,
                format: wgpu::TextureFormat::Rgba8UnormSrgb,
                usage: wgpu::TextureUsages::TEXTURE_BINDING | wgpu::TextureUsages::COPY_DST,
                view_formats: &[],
            });

            texture_page_bind_groups.insert(
                page_id,
                pipeline.create_texture_bind_group(
                    &device,
                    &page_texture.create_view(&wgpu::TextureViewDescriptor::default()),
                    &device.create_sampler(&wgpu::SamplerDescriptor {
                        label: None,
                        mag_filter: wgpu::FilterMode::Nearest,
                        min_filter: wgpu::FilterMode::Nearest,
                        ..Default::default()
                    }),
                ),
            );

            for (texture_id, &position) in page.textures.iter() {
                let texture_desc = game.texture(texture_id);

                let image = image::ImageReader::open(&texture_desc.image_path)
                    .unwrap()
                    .decode()
                    .unwrap()
                    .into_rgba8()
                    .into_flat_samples();

                assert_eq!(
                    texture_desc.size,
                    Vec2::new(image.layout.width, image.layout.height)
                );

                queue.write_texture(
                    wgpu::TexelCopyTextureInfo {
                        texture: &page_texture,
                        mip_level: 0,
                        origin: wgpu::Origin3d {
                            x: position[0],
                            y: position[1],
                            z: 0,
                        },
                        aspect: wgpu::TextureAspect::All,
                    },
                    &image.samples,
                    wgpu::TexelCopyBufferLayout {
                        offset: 0,
                        bytes_per_row: Some(image.layout.height_stride.try_into().unwrap()),
                        rows_per_image: None,
                    },
                    wgpu::Extent3d {
                        width: texture_desc.size[0],
                        height: texture_desc.size[1],
                        depth_or_array_layers: 1,
                    },
                );

                textures.insert(
                    texture_id,
                    (
                        page_id,
                        texture_desc.size.cast(),
                        Box2::with_size(position, texture_desc.size)
                            .cast::<f32>()
                            .scale(Vec2::splat(1.0) / page.size.cast()),
                    ),
                );
            }
        }

        let state = AppState {
            window,
            device,
            queue,
            size,
            surface,
            surface_format,
            render: Default::default(),
            input: Default::default(),
            game,
            pipeline,
            texture_page_bind_groups,
            parameters_buffer,
            parameters_bind_group,
            textures,
            geometry: Default::default(),
            batches: Default::default(),
            last_render: Instant::now(),
            frames_behind: 1.0,
        };

        state.configure_surface();

        state
    }

    fn get_window(&self) -> &Window {
        &self.window
    }

    fn configure_surface(&self) {
        let surface_config = wgpu::SurfaceConfiguration {
            usage: wgpu::TextureUsages::RENDER_ATTACHMENT,
            format: self.surface_format,
            view_formats: vec![self.surface_format.add_srgb_suffix()],
            alpha_mode: wgpu::CompositeAlphaMode::Auto,
            width: self.size.width,
            height: self.size.height,
            desired_maximum_frame_latency: 2,
            present_mode: wgpu::PresentMode::AutoVsync,
        };
        self.surface.configure(&self.device, &surface_config);
    }

    fn resize(&mut self, new_size: winit::dpi::PhysicalSize<u32>) {
        self.size = new_size;
        self.configure_surface();
    }

    fn render(&mut self) {
        const MAX_FRAMES_BEHIND: u8 = 4;

        let now = Instant::now();
        let since_last_render = now.duration_since(self.last_render);
        self.last_render = now;
        self.frames_behind += since_last_render.as_secs_f64() * self.game.tick_rate();
        self.frames_behind = self.frames_behind.min(MAX_FRAMES_BEHIND as f64);

        while self.frames_behind >= 1.0 {
            self.game.tick(&self.input, &mut self.render).unwrap();
            self.frames_behind -= 1.0;
        }

        let surface_texture = self
            .surface
            .get_current_texture()
            .expect("failed to acquire next swapchain texture");
        let texture_view = surface_texture
            .texture
            .create_view(&wgpu::TextureViewDescriptor {
                format: Some(self.surface_format.add_srgb_suffix()),
                ..Default::default()
            });

        let logical_size = self.size.to_logical(self.window.scale_factor());
        let ndc_transform = Affine2::new()
            .scale(Vec2::splat(2.0) / Vec2::new(logical_size.width, logical_size.height))
            .translate(Vec2::splat(-1.0))
            .scale(Vec2::new(1.0, -1.0));

        self.queue.write_buffer(
            &self.parameters_buffer,
            0,
            bytemuck::bytes_of(&pipeline::ParametersUniform::new(ndc_transform)),
        );

        self.geometry.clear();
        self.batches.clear();

        for quad in &self.render.quads {
            let index_start = self.geometry.indices.len();

            let (texture_page_id, tex_size, tex_coords) = self.textures[quad.texture];
            let vertexes = [[0.0, 0.0], [0.0, 1.0], [1.0, 1.0], [1.0, 0.0]].map(|[x, y]| {
                pipeline::Vertex::new(
                    quad.transform
                        .transform_point(Box2::with_size(Vec2::zero(), tex_size).eval([x, y])),
                    tex_coords.eval([x, y]),
                )
            });
            self.geometry.draw_quad(vertexes);

            self.batches.push((
                cast::cast(index_start)..cast::cast(self.geometry.indices.len()),
                texture_page_id,
            ));
        }

        let gpu_geometry = GpuGeometry::create(&self.device, &self.geometry);

        let mut encoder = self.device.create_command_encoder(&Default::default());
        let mut rpass = encoder.begin_render_pass(&wgpu::RenderPassDescriptor {
            label: None,
            color_attachments: &[Some(wgpu::RenderPassColorAttachment {
                view: &texture_view,
                resolve_target: None,
                ops: wgpu::Operations {
                    load: wgpu::LoadOp::Clear(wgpu::Color::BLACK),
                    store: wgpu::StoreOp::Store,
                },
            })],
            depth_stencil_attachment: None,
            timestamp_writes: None,
            occlusion_query_set: None,
        });

        rpass.set_pipeline(&self.pipeline.pipeline);
        rpass.set_bind_group(
            pipeline::Pipeline::PARAMETERS_BIND_GROUP,
            &self.parameters_bind_group,
            &[],
        );

        rpass.set_vertex_buffer(0, gpu_geometry.vertex_buffer.slice(..));
        rpass.set_index_buffer(
            gpu_geometry.index_buffer.slice(..),
            gpu_geometry.index_format,
        );

        for (range, page_id) in self.batches.iter().cloned() {
            rpass.set_bind_group(
                pipeline::Pipeline::TEXTURE_BIND_GROUP,
                &self.texture_page_bind_groups[page_id],
                &[],
            );

            rpass.draw_indexed(range, 0, 0..1);
        }

        drop(rpass);

        self.queue.submit([encoder.finish()]);
        self.window.pre_present_notify();
        surface_texture.present();
    }
}

struct App {
    project_file: PathBuf,
    app_state: Option<AppState>,
}

impl ApplicationHandler for App {
    fn resumed(&mut self, event_loop: &ActiveEventLoop) {
        let window = Arc::new(
            event_loop
                .create_window(
                    Window::default_attributes()
                        .with_inner_size(winit::dpi::LogicalSize::new(1366, 768)),
                )
                .unwrap(),
        );

        let state = pollster::block_on(AppState::new(window.clone(), &self.project_file));
        self.app_state = Some(state);

        window.request_redraw();
    }

    fn window_event(&mut self, event_loop: &ActiveEventLoop, _id: WindowId, event: WindowEvent) {
        let app_state = self.app_state.as_mut().unwrap();
        match event {
            WindowEvent::CloseRequested => {
                event_loop.exit();
            }
            WindowEvent::RedrawRequested => {
                app_state.render();
                app_state.get_window().request_redraw();
            }
            WindowEvent::Resized(size) => {
                app_state.resize(size);
            }
            WindowEvent::MouseInput { state, button, .. } => match button {
                MouseButton::Left => {
                    app_state
                        .input
                        .mouse_pressed
                        .set(fab::MouseButtons::Left, state.is_pressed());
                }
                MouseButton::Right => {
                    app_state
                        .input
                        .mouse_pressed
                        .set(fab::MouseButtons::Right, state.is_pressed());
                }
                MouseButton::Middle => {
                    app_state
                        .input
                        .mouse_pressed
                        .set(fab::MouseButtons::Middle, state.is_pressed());
                }
                _ => {}
            },
            WindowEvent::CursorMoved { position, .. } => {
                let logical = position.to_logical(app_state.window.scale_factor());
                app_state.input.mouse_position = Vec2::new(logical.x, logical.y);
            }
            _ => (),
        }
    }
}

#[derive(Parser)]
struct Cli {
    project_file: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("info")).init();

    let event_loop = EventLoop::new().unwrap();
    event_loop.set_control_flow(ControlFlow::Poll);

    let mut app = App {
        project_file: cli.project_file,
        app_state: None,
    };
    event_loop.run_app(&mut app).unwrap();
}
