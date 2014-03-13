#version 330

in vec4 position;
in vec2 texCoord;
in vec3 normal;

out vec2 texCoordFrag;
out vec3 normalFrag;

uniform mat4x4 modelView;

void main()
{
    gl_Position = modelView * position;
    texCoordFrag = texCoord;
    normalFrag = normal;
}