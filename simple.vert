#version 330

layout(location = 0) in vec4 position;

uniform mat4x4 modelView;

void main()
{
    gl_Position = modelView * position;
}