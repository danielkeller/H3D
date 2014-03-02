#version 330

out vec4 outputColor;
uniform vec4 color;

void main()
{
   outputColor = color; //vec4(1.0f, 1.0f, 1.0f, 1.0f);
}