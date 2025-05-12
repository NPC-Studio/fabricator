use std::mem;

use bytemuck::{Pod, Zeroable};
use fabricator_math::{Affine2, Vec2};

#[derive(Debug, Copy, Clone, Default, Pod, Zeroable)]
#[repr(C)]
pub struct ParametersUniform {
    transform: [[f32; 4]; 3],
}

impl ParametersUniform {
    pub fn new(transform: Affine2<f32>) -> Self {
        let m = transform.into_mat3();
        Self {
            transform: [
                m[0].extend(0.0).into_array(),
                m[1].extend(0.0).into_array(),
                m[2].extend(0.0).into_array(),
            ],
        }
    }
}

#[derive(Debug, Copy, Clone, Pod, Zeroable)]
#[repr(C)]
pub struct Vertex {
    position: [f32; 2],
    uv: [f32; 2],
}

impl Vertex {
    pub fn new(pos: Vec2<f32>, uv: Vec2<f32>) -> Self {
        Self {
            position: pos.into_array(),
            uv: uv.into_array(),
        }
    }
}

pub struct Pipeline {
    pub texture_bind_group_layout: wgpu::BindGroupLayout,
    pub parameters_bind_group_layout: wgpu::BindGroupLayout,
    pub pipeline: wgpu::RenderPipeline,
}

impl Pipeline {
    pub const TEXTURE_BIND_GROUP: u32 = 0;
    pub const PARAMETERS_BIND_GROUP: u32 = 1;

    pub fn new(device: &wgpu::Device, output_format: wgpu::TextureFormat) -> Self {
        let texture_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[
                    wgpu::BindGroupLayoutEntry {
                        binding: 0,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Texture {
                            sample_type: wgpu::TextureSampleType::Float { filterable: true },
                            view_dimension: wgpu::TextureViewDimension::D2,
                            multisampled: false,
                        },
                        count: None,
                    },
                    wgpu::BindGroupLayoutEntry {
                        binding: 1,
                        visibility: wgpu::ShaderStages::FRAGMENT,
                        ty: wgpu::BindingType::Sampler(wgpu::SamplerBindingType::Filtering),
                        count: None,
                    },
                ],
            });

        let parameters_bind_group_layout =
            device.create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                label: None,
                entries: &[wgpu::BindGroupLayoutEntry {
                    binding: 0,
                    visibility: wgpu::ShaderStages::VERTEX_FRAGMENT,
                    ty: wgpu::BindingType::Buffer {
                        ty: wgpu::BufferBindingType::Uniform,
                        has_dynamic_offset: false,
                        min_binding_size: None,
                    },
                    count: None,
                }],
            });

        let shader_module = device.create_shader_module(wgpu::ShaderModuleDescriptor {
            label: None,
            source: wgpu::ShaderSource::Wgsl(include_str!("shader.wgsl").into()),
        });

        let pipeline = device.create_render_pipeline(&wgpu::RenderPipelineDescriptor {
            label: None,
            layout: Some(
                &device.create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                    label: None,
                    bind_group_layouts: &[
                        &texture_bind_group_layout,
                        &parameters_bind_group_layout,
                    ],
                    push_constant_ranges: &[],
                }),
            ),
            vertex: wgpu::VertexState {
                module: &shader_module,
                entry_point: Some("vs_main"),
                buffers: &[wgpu::VertexBufferLayout {
                    array_stride: mem::size_of::<Vertex>() as wgpu::BufferAddress,
                    step_mode: wgpu::VertexStepMode::Vertex,
                    attributes: &wgpu::vertex_attr_array![
                        0 => Float32x2,
                        1 => Float32x2,
                    ],
                }],
                compilation_options: Default::default(),
            },
            fragment: Some(wgpu::FragmentState {
                module: &shader_module,
                entry_point: Some("fs_main"),
                targets: &[Some(wgpu::ColorTargetState {
                    format: output_format,
                    blend: Some(wgpu::BlendState::PREMULTIPLIED_ALPHA_BLENDING),
                    write_mask: wgpu::ColorWrites::ALL,
                })],
                compilation_options: Default::default(),
            }),
            primitive: wgpu::PrimitiveState {
                topology: wgpu::PrimitiveTopology::TriangleList,
                cull_mode: None,
                ..Default::default()
            },
            depth_stencil: None,
            multisample: Default::default(),
            multiview: None,
            cache: None,
        });

        Self {
            texture_bind_group_layout,
            parameters_bind_group_layout,
            pipeline,
        }
    }

    pub fn create_texture_bind_group(
        &self,
        device: &wgpu::Device,
        texture: &wgpu::TextureView,
        sampler: &wgpu::Sampler,
    ) -> wgpu::BindGroup {
        device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &self.texture_bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: wgpu::BindingResource::TextureView(texture),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: wgpu::BindingResource::Sampler(sampler),
                },
            ],
        })
    }

    pub fn create_parameters_bind_group(
        &self,
        device: &wgpu::Device,
        parameters_buffer: wgpu::BufferBinding,
    ) -> wgpu::BindGroup {
        device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: None,
            layout: &self.parameters_bind_group_layout,
            entries: &[wgpu::BindGroupEntry {
                binding: 0,
                resource: wgpu::BindingResource::Buffer(parameters_buffer),
            }],
        })
    }
}
