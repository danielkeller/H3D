#version 330

out vec4 outputColor;

in vec2 texCoordFrag;
in vec3 normalFrag;
uniform sampler2D tex;

void main()
{
    outputColor = texture(tex, texCoordFrag);
}