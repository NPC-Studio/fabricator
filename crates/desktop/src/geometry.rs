use std::marker::PhantomData;

use bytemuck::Pod;
use wgpu::util::DeviceExt;

pub trait IndexType: bytemuck::Pod {
    const INDEX_FORMAT: wgpu::IndexFormat;
}

impl IndexType for u16 {
    const INDEX_FORMAT: wgpu::IndexFormat = wgpu::IndexFormat::Uint16;
}

impl IndexType for u32 {
    const INDEX_FORMAT: wgpu::IndexFormat = wgpu::IndexFormat::Uint32;
}

pub struct Geometry<V, I> {
    pub vertices: Vec<V>,
    pub indices: Vec<I>,
}

impl<V, I> Default for Geometry<V, I> {
    fn default() -> Self {
        Self {
            vertices: Default::default(),
            indices: Default::default(),
        }
    }
}

impl<V, I: IndexType> Geometry<V, I> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn draw_triangle_list(
        &mut self,
        vertices: impl IntoIterator<Item = V>,
        indices: impl IntoIterator<Item = I>,
    ) where
        I: TryFrom<usize> + num::CheckedAdd,
    {
        let base_index = I::try_from(self.vertices.len())
            .ok()
            .expect("cannot convert usize to index");
        self.vertices.extend(vertices);
        self.indices.extend(
            indices
                .into_iter()
                .map(|i| base_index.checked_add(&i).expect("index overflow")),
        );
    }

    pub fn draw_quad(&mut self, quad: [V; 4])
    where
        I: TryFrom<usize> + num::CheckedAdd + From<u8>,
    {
        self.draw_triangle_list(quad, [0, 1, 2, 2, 3, 0].map(Into::into));
    }

    pub fn clear(&mut self) {
        self.vertices.clear();
        self.indices.clear();
    }

    pub fn is_empty(&self) -> bool {
        self.vertices.is_empty() && self.indices.is_empty()
    }
}

pub struct GpuGeometry<V> {
    pub vertex_buffer: wgpu::Buffer,
    pub index_buffer: wgpu::Buffer,
    pub index_format: wgpu::IndexFormat,
    pub index_count: u32,
    pub _marker: PhantomData<V>,
}

impl<V: Pod> GpuGeometry<V> {
    pub fn create<I: IndexType>(device: &wgpu::Device, geometry: &Geometry<V, I>) -> Self {
        let vertex_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("vertex buffer"),
            contents: bytemuck::cast_slice(&geometry.vertices),
            usage: wgpu::BufferUsages::VERTEX,
        });

        let index_buffer = device.create_buffer_init(&wgpu::util::BufferInitDescriptor {
            label: Some("index buffer"),
            contents: bytemuck::cast_slice(&geometry.indices),
            usage: wgpu::BufferUsages::INDEX,
        });

        Self {
            vertex_buffer,
            index_buffer,
            index_format: I::INDEX_FORMAT,
            index_count: geometry.indices.len().try_into().unwrap(),
            _marker: PhantomData,
        }
    }
}
