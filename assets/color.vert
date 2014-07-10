#version 330

in vec3 position;
uniform mat4x4 modelView;

in vec3 color;
out vec3 vert_color;

void main()
{
    gl_Position = modelView * vec4(position, 1);
    vert_color = color;
}