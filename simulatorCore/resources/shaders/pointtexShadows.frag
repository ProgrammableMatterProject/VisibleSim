varying vec4 diffuse,specular,ambientGlobal,ambient;
varying vec3 ecPos,normal;
uniform sampler2D tex;
uniform sampler2DShadow shadowMap;
uniform bool textureEnable;

void main() {
	float shade = clamp(shadow2DProj(shadowMap, gl_TexCoord[1]).r, 0.5, 1.0);

	vec4 texel;
	vec4 color = ambientGlobal;
	if (textureEnable) {
		vec4 texColor = texture2D(tex,gl_TexCoord[0].st);
		if (texColor.a<0.1) discard;
		color+= ambient*texColor;
		texel = diffuse*texColor;
	} else {
		texel=diffuse;
		color+=ambient;
	}
	vec3 D = normalize(gl_LightSource[0].spotDirection);
	vec3 L = normalize(gl_LightSource[0].position.xyz - ecPos);

	if (dot(-L, D) >= gl_LightSource[0].spotCosCutoff ) {
		vec3 n = normalize(normal);
		vec3 R = normalize(reflect(L,n));
		vec3 E = normalize(ecPos); // we are in Eye Coordinates, so EyePos is (0,0,0)

		float NdotL = max(dot(n,L),0.0);
		float Ispec = min(pow(max(dot(R,E),0.0),0.3*gl_FrontMaterial.shininess),1.0);
		color += shade*(texel * NdotL + Ispec * specular);
	}
	gl_FragColor = vec4(color.rgb,texel.a);
}
