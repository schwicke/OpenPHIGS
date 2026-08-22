#version 420 compatibility
uniform mat4 ModelViewMatrix;
uniform mat4 ProjectionMatrix;
uniform vec4 sPlane;
uniform vec4 tPlane;
in vec4 vColor;

out vec4 Color;
out vec4 Normal;
out vec4 VertexPosEye;
out vec2 TexCoord;

void main()
{
  VertexPosEye = gl_ModelViewMatrix * gl_Vertex;
  gl_ClipVertex = VertexPosEye;
  Color = vColor;
  Normal = normalize(ModelViewMatrix * vec4(gl_Normal, 1));
  gl_Position = ProjectionMatrix * ModelViewMatrix * gl_Vertex;
  //TexCoord =  gl_MultiTexCoord0.st;
  TexCoord = vec2(dot(gl_Vertex, sPlane), dot(gl_Vertex, tPlane));
}
