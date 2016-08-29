varying vec3 ecPos,normal;
varying vec4 diffuse,specular,ambientGlobal,ambient;
	
void main() {
		
	normal = normalize(gl_NormalMatrix * gl_Normal);
		
	ecPos = vec3(gl_ModelViewMatrix * gl_Vertex);
		
	diffuse = gl_FrontMaterial.diffuse * gl_LightSource[0].diffuse;	
	specular = gl_FrontMaterial.specular * gl_LightSource[0].specular;	
	ambient = gl_FrontMaterial.ambient * gl_LightSource[0].ambient;
	ambientGlobal = gl_LightModel.ambient * gl_FrontMaterial.ambient;

			
	gl_TexCoord[0] = gl_MultiTexCoord0;
	gl_TexCoord[1] = gl_TextureMatrix[0]*vec4(ecPos.xyz,1);
	gl_Position = ftransform();
}
