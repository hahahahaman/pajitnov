#version 330
layout (location = 0) in vec3 position;
layout (location = 0) in vec4 color;
//layout (location = 0) in vec2 texcoord;

out vec4 Color;
//out vec2 Texcoord;

uniform mat4 model;
uniform mat4 view;
uniform mat4 proj;

void main() {
  Color = color;
  gl_Position = proj * view * model * vec4(position, 1.0f);
}
