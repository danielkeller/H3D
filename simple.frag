#version 330

out vec4 outputColor;
uniform vec4 Color;

void main()
{
   outputColor = Color; //vec4(1.0f, 1.0f, 1.0f, 1.0f);
}