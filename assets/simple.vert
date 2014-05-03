#version 330

in vec3 position;
in vec2 texCoord;
in vec3 normal;

out vec2 texCoordFrag;
out vec3 normalFrag;

uniform mat4x4 modelView;

void main()
{
    gl_Position = modelView * vec4(position, 1);
    texCoordFrag = texCoord;
    normalFrag = normal;
}