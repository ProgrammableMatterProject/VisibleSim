#include "shaders.h"
#include "trace.h"
#include "color.h"

GLuint depth_tex,id_fb,color_rb;
bool useShaders=true;
GLhandleARB shadersProgram;
GLint locTex,locShadowMap,locTextureEnable;

void enableTexture(bool enable) {
	glUniform1iARB(locTextureEnable,enable);
}

GLcharARB *lectureCodeShader(const char* titre)
{ GLint tailleFichier;
  ifstream fin(titre);
  if (!fin.is_open()) return NULL;
  fin.seekg(0, ios_base::end);
  tailleFichier = GLint(fin.tellg());
  fin.close();

  // Memory allocation
  GLcharARB *code = new GLcharARB[tailleFichier+1];

  // Load shader file
  fin.open(titre);
  fin.read((char*)code, tailleFichier);
  code[tailleFichier] = '\0';
  fin.close();

  return code;
}

GLhandleARB loadShader(const char *titreVP, const char *titreFP) {
	GLhandleARB VShader,FShader;
	GLcharARB* code;
	GLhandleARB prog;

// create an Object Shader for the Vertex Program
	VShader = glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);
// create an Object Shader for the Fragment Program
	FShader = glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
// loading the Vertex Program source
	code = lectureCodeShader(titreVP);
	if (!code) {
		ERRPUT << "error: " << titreVP << " not found."<< endl;
		exit(-1);
	}
	glShaderSourceARB(VShader, 1, (const GLcharARB**) &code, NULL);
// Compile the Vertex program
	OUTPUT << "Compilation of the Vertex Program" << endl;
	glCompileShaderARB(VShader);

	shaderCompilationStatus(VShader);

	delete [] code;
// loading the Fragment Program source
	code = lectureCodeShader(titreFP);
	if (!code) {
		ERRPUT << "error: " << titreFP << " not found."<< endl;
		exit(-1);
	}
	glShaderSourceARB(FShader, 1, (const GLcharARB**) &code, NULL);
// Compile the Fragment program
	OUTPUT << "Compilation of the Fragment Program" << endl;
	glCompileShaderARB(FShader);

	shaderCompilationStatus(FShader);

	delete [] code;

// Create the object program
	prog = glCreateProgramObjectARB();
// links
	glAttachObjectARB(prog, VShader);
	glAttachObjectARB(prog, FShader);

// linking
	glLinkProgramARB(prog);

	return prog;
}

void initShaders() {
  OUTPUT << "initShaders" << endl;
  glewInit();

  glClearColor (0.6f, 0.6f, 0.6f, 1.0f);	// Black Background
  glClearDepth (1.0f);						// Depth Buffer Setup
  glDepthFunc (GL_LEQUAL);					// The Type Of Depth Testing (Less Or Equal)
  glEnable (GL_DEPTH_TEST);					// Enable Depth Testing
  glShadeModel (GL_SMOOTH);					// Select Smooth Shading
  glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);			// Set Perspective Calculations To Most Accurate

  shadersProgram = loadShader("../../simulatorCore/resources/shaders/pointtex.vert",
							  "../../simulatorCore/resources/shaders/pointtex.frag");
  glEnable(GL_CULL_FACE);
  glCullFace(GL_BACK);

  locTex = glGetUniformLocationARB(shadersProgram, "tex");
  if (locTex==-1) {
	  ERRPUT << "erreur affectation : tex\n";
  }
  locShadowMap = glGetUniformLocationARB(shadersProgram, "shadowMap");
  if (locShadowMap ==-1) {
	  ERRPUT << "erreur affectation : shadowMap\n";
  }
  locTextureEnable = glGetUniformLocationARB(shadersProgram, "textureEnable");
  if (locTextureEnable  ==-1) {
	  ERRPUT << "erreur affectation : textureEnable\n";
  }

  // texture pour le shadow mapping
  glGenFramebuffersEXT(1, &id_fb);	// identifiant pour la texture
  glGenTextures(1, &depth_tex);													// and a new texture used as a color buffer
  glGenRenderbuffersEXT(1, &color_rb);											// And finaly a new depthbuffer

  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT,id_fb);	// switch to the new framebuffer
  // initialize color texture
  glBindTexture(GL_TEXTURE_2D, depth_tex);										// Bind the colorbuffer texture
  glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);				// make it linear filterd
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
  glTexImage2D( GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT24, 2048, 2048, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_INT, NULL );
  glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT,GL_DEPTH_ATTACHMENT_EXT,GL_TEXTURE_2D, depth_tex, 0); // attach it to the framebuffer

  // initialize depth renderbuffer
  glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, color_rb);							// bind the depth renderbuffer
  glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT,GL_RGBA,2048, 2048);	// get the data space for it
  glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT,GL_COLOR_ATTACHMENT0_EXT,GL_RENDERBUFFER_EXT, color_rb); // bind it to the renderbuffer

  glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

  OUTPUT << "Shaders initialized." << endl;
}

void shadowedRenderingStep1(Camera *camera) {
	// first pass : create the shadow map
	glViewport (0, 0, 2048, 2048); // viewport at the texture size
	glClearDepth (1.0f);
	glBindTexture(GL_TEXTURE_2D, 0);						// disable texture
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, id_fb);			// rendering in the fb texture

	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);	// clear the texture
	glMatrixMode(GL_PROJECTION);
    glLoadMatrixf(camera->ls.matP);
	glMatrixMode(GL_MODELVIEW);
	glLoadMatrixf(camera->ls.matMV);

 	glColorMask(GL_FALSE, GL_FALSE, GL_FALSE, GL_FALSE);
  // Eliminate artifacts caused by shadow mapping
	glPolygonOffset(8.0f, 4.0f);
	glEnable(GL_POLYGON_OFFSET_FILL);
}

void shadowedRenderingStep2(int w,int h) {
	 glFlush ();	// Flush The GL Rendering Pipeline
  // second pass : rendering of the final image
	glViewport(0,0,w,h);

	glColorMask(GL_TRUE, GL_TRUE, GL_TRUE, GL_TRUE);
	glDisable(GL_POLYGON_OFFSET_FILL);

	glBindTexture(GL_TEXTURE_2D, 0);
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0); // d�sactive le rendu en texture

	glClearDepth (1.0f);
	glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

}

void shadowedRenderingStep3(Camera *camera) {
	// second pass render the shadow map
	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	camera->glProjection();
	const float mBias[] = {0.5, 0.0, 0.0, 0.0,
						   0.0, 0.5, 0.0, 0.0,
						   0.0, 0.0, 0.5, 0.0,
						   0.5, 0.5, 0.5, 1.0};

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	camera->glLookAt();

	float camViewMatrix[16];
	glGetFloatv(GL_MODELVIEW_MATRIX, camViewMatrix);
	Matrix mat(camViewMatrix),mat_1;
	mat.inverse(mat_1);

	glMatrixMode(GL_TEXTURE);

	glLoadMatrixf(mBias);			// The bias matrix to convert to a 0 to 1 ratio

	glMultMatrixf(camera->ls.matP);
	glMultMatrixf(camera->ls.matMV);
	glMultMatrixd(mat_1.m);

	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	camera->glLookAt();
	glColor3f(1,1,1);												// set the color to white

 // placement de la source de lumière
    glLightfv(GL_LIGHT0, GL_POSITION, camera->ls.pos);
	glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, camera->ls.dir );
	glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, camera->ls.falloffAngle );

    glLightfv(GL_LIGHT0, GL_DIFFUSE, WHITE.rgba);
    glLightfv(GL_LIGHT0, GL_AMBIENT, WHITE.rgba);
    glLightfv(GL_LIGHT0, GL_SPECULAR, WHITE.rgba);

// activation du programme de shader
    if(useShaders && shadersProgram)
    glUseProgramObjectARB(shadersProgram);
    else{
    glEnable(GL_LIGHTING);
    }

    glUniform1iARB(locTex, 0);

    glUniform1iARB(locShadowMap , 1);

    glActiveTextureARB(GL_TEXTURE1_ARB);
    glEnable(GL_TEXTURE_2D);
    glBindTexture(GL_TEXTURE_2D,depth_tex);

    // Here is where we set the mode and function for shadow mapping with shadow2DProj().
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_MODE_ARB, GL_COMPARE_R_TO_TEXTURE_ARB);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_COMPARE_FUNC_ARB, GL_LEQUAL);

    glActiveTextureARB(GL_TEXTURE0_ARB);
    glEnable(GL_TEXTURE_2D);
}

void shadowedRenderingStep4() {
	glActiveTextureARB(GL_TEXTURE1);
	glDisable(GL_TEXTURE_2D);
	glActiveTextureARB(GL_TEXTURE0);
	glDisable(GL_TEXTURE_2D);

	if(useShaders) glUseProgramObjectARB(0);

	glMatrixMode(GL_TEXTURE);
	glLoadIdentity();
	glFlush ();	// Flush The GL Rendering Pipeline
}

GLint shaderCompilationStatus(GLhandleARB shader) {
/* verification du succes de la compilation */
	GLint compile_status = GL_TRUE;
	GLsizei logsize = 0;
	char *log=NULL;
	glGetObjectParameterivARB(shader, GL_COMPILE_STATUS, &compile_status);
	if (compile_status != GL_TRUE) {
/* erreur a la compilation recuperation du log d'erreur */
/* on recupere la taille du message d'erreur */
		glGetObjectParameterivARB(shader, GL_INFO_LOG_LENGTH, &logsize);
/* on alloue un espace memoire dans lequel OpenGL ecrira le message */
		log = new char [logsize + 1];
/* initialisation du contenu */
		memset(log, '\0', logsize + 1);
		glGetInfoLogARB(shader, logsize, &logsize, log);
		OUTPUT << "Impossible de compiler le program :\n" << log  <<endl;
	} else {
		OUTPUT << "compilation OK" << endl;
	}
	return compile_status;
}
