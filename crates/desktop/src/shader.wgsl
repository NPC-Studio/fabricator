struct ParametersUniform {
    transform: mat3x3<f32>,
};

struct VertexInput {
    @location(0) position: vec2<f32>,
    @location(1) uv: vec2<f32>,
};

struct VertexOutput {
    @builtin(position) position: vec4<f32>,
    @location(0) uv: vec2<f32>,
};

@group(0) @binding(0) var texture: texture_2d<f32>;
@group(0) @binding(1) var texture_sampler: sampler;

@group(1) @binding(0) var<uniform> parameters: ParametersUniform;

@vertex
fn vs_main(vin: VertexInput) -> VertexOutput {
    var vout: VertexOutput;
    vout.position = vec4<f32>((parameters.transform * vec3<f32>(vin.position, 1.0)).xy, 0.0, 1.0);
    vout.uv = vin.uv;
    return vout;
}

@fragment
fn fs_main(fin: VertexOutput) -> @location(0) vec4<f32> {
    return textureSample(texture, texture_sampler, fin.uv);
}
