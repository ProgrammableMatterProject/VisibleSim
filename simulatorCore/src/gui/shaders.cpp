#include "../gui/shaders.h"
#include "../utils/color.h"
#ifdef DEBUG_GRAPHICS
    #include "../utils/trace.h"
#endif

GLuint depth_tex,id_fb,color_rb;
bool useShaders=true;
GLhandleARB shadersProgram;
GLint locTex,locShadowMap,locTextureEnable;

static const float ambientLightColor[4] = {0.25,0.25,0.25,1.0};
static const float diffuseLightColor[4] = {0.75,0.75,0.75,1.0};
static const float specularLightColor[4] = {0.75,0.75,0.75,1.0};

void enableTexture(bool enable) {
    glUniform1iARB(locTextureEnable,enable);
}

string lectureCodeShader(const char* title) {
    ifstream fin(title);
    if (!fin.is_open()) return string("");

#ifdef DEBUG_GRAPHICS
    fin.seekg(0, ifstream::end);
    int tailleFichier = fin.tellg();
    cout << title << " file size=" << tailleFichier << endl;
    fin.seekg(0, ifstream::beg);
#endif
    // Memory allocation
    string code,line;
    while (!fin.eof()) {
        getline(fin,line);
        code+=line+'\n';
    }
    fin.close();

    return code;
}

GLhandleARB loadShader(const char *titreVP, const char *titreFP) {
    GLhandleARB VShader,FShader;
    string code;
    GLhandleARB prog;

// create an Object Shader for the Vertex Program
    VShader = glCreateShaderObjectARB(GL_VERTEX_SHADER_ARB);
// create an Object Shader for the Fragment Program
    FShader = glCreateShaderObjectARB(GL_FRAGMENT_SHADER_ARB);
// loading the Vertex Program source
    code = lectureCodeShader(titreVP);

    if (code.empty()) {
#ifdef DEBUG_GRAPHICS
        ERRPUT << "error: " << titreVP << " not found."<< endl;
#endif
        exit(-1);
    }
    glShaderSourceARB(VShader, 1, (const GLcharARB**) &code, NULL);
// Compile the Vertex program
#ifdef DEBUG_GRAPHICS
    OUTPUT << "Compilation of the Vertex Program" << endl;
#endif
    glCompileShaderARB(VShader);

    shaderCompilationStatus(VShader);

// loading the Fragment Program source
    code = lectureCodeShader(titreFP);
    if (code.empty()) {
#ifdef DEBUG_GRAPHICS
        ERRPUT << "error: " << titreFP << " not found."<< endl;
        exit(-1);
#endif
    }
    glShaderSourceARB(FShader, 1, (const GLcharARB**) &code, NULL);
// Compile the Fragment program
#ifdef DEBUG_GRAPHICS
    OUTPUT << "Compilation of the Fragment Program" << endl;
#endif
    glCompileShaderARB(FShader);

    shaderCompilationStatus(FShader);

// Create the object program
    prog = glCreateProgramObjectARB();
// links
    glAttachObjectARB(prog, VShader);
    glAttachObjectARB(prog, FShader);

// linking
    glLinkProgramARB(prog);

    return prog;
}

void initShaders(bool activateShadows) {
#ifdef DEBUG_GRAPHICS
    OUTPUT << "initShaders" << endl;
#endif

    glewInit();

    glClearDepth (1.0f);						// Depth Buffer Setup
    glDepthFunc (GL_LEQUAL);					// The Type Of Depth Testing (Less Or Equal)
    glEnable (GL_DEPTH_TEST);					// Enable Depth Testing
    glShadeModel (GL_SMOOTH);					// Select Smooth Shading
    glHint (GL_PERSPECTIVE_CORRECTION_HINT, GL_NICEST);			// Set Perspective Calculations To Most Accurate

#ifdef WIN32
    if (activateShadows) {
        string vertFile = ROOT_DIR;
        vertFile+="/simulatorCore/resources/shaders/pointtexShadows.vert";
        cout << vertFile << endl;
        string fragFile = ROOT_DIR;
        fragFile+="/simulatorCore/resources/shaders/pointtexShadows.frag";
        shadersProgram = loadShader(vertFile.c_str(),fragFile.c_str());
    } else {
        string vertFile = ROOT_DIR;
        vertFile+="/simulatorCore/resources/shaders/pointtex.vert";
        cout << vertFile << endl;
        string fragFile = ROOT_DIR;
        fragFile+="/simulatorCore/resources/shaders/pointtex.frag";
        shadersProgram = loadShader(vertFile.c_str(),fragFile.c_str());
    }
#else
    if (activateShadows) {
      shadersProgram = loadShader("../../simulatorCore/resources/shaders/pointtexShadows.vert",
                                  "../../simulatorCore/resources/shaders/pointtexShadows.frag");
    } else {
      shadersProgram = loadShader("../../simulatorCore/resources/shaders/pointtex.vert",
                                  "../../simulatorCore/resources/shaders/pointtex.frag");
    }
#endif
    glEnable(GL_CULL_FACE);
    glCullFace(GL_BACK);

    locTex = glGetUniformLocationARB(shadersProgram, "tex");
#ifdef DEBUG_GRAPHICS
    if (locTex==-1) {
      ERRPUT << "erreur affectation : tex\n";
    }
#endif
    locTextureEnable = glGetUniformLocationARB(shadersProgram, "textureEnable");
#ifdef DEBUG_GRAPHICS
    if (locTextureEnable  ==-1) {
        ERRPUT << "erreur affectation : textureEnable\n";
    }
#endif

    if (activateShadows) {
        locShadowMap = glGetUniformLocationARB(shadersProgram, "shadowMap");
#ifdef DEBUG_GRAPHICS
        if (locShadowMap == -1) {
            ERRPUT << "erreur affectation : shadowMap\n";
        }
#endif
        // texture pour le shadow mapping
        glGenFramebuffersEXT(1, &id_fb);	// id for the frameBuffer
        glGenTextures(1, &depth_tex);		// and a new texture used as a color buffer
        glGenRenderbuffersEXT(1, &color_rb); // And finaly a new depthbuffer
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT,id_fb);	// switch to the new framebuffer
        // initialize color texture
        glBindTexture(GL_TEXTURE_2D, depth_tex);										// Bind the colorbuffer texture
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);				// make it linear filterd
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
        glTexImage2D( GL_TEXTURE_2D, 0, GL_DEPTH_COMPONENT, 2048, 2048, 0, GL_DEPTH_COMPONENT, GL_UNSIGNED_INT, NULL );
        glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT,GL_DEPTH_ATTACHMENT_EXT,GL_TEXTURE_2D, depth_tex, 0); // attach it to the framebuffer
        // initialize depth renderbuffer
        glBindRenderbufferEXT(GL_RENDERBUFFER_EXT, color_rb);							// bind the depth renderbuffer
        glRenderbufferStorageEXT(GL_RENDERBUFFER_EXT,GL_RGBA,2048, 2048);	// get the data space for it
        glFramebufferRenderbufferEXT(GL_FRAMEBUFFER_EXT,GL_COLOR_ATTACHMENT0_EXT,GL_RENDERBUFFER_EXT, color_rb); // bind it to the renderbuffer
        glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
    } else {
        glTexParameterf(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);				// make it linear filterd
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    }

#ifdef DEBUG_GRAPHICS
  OUTPUT << "Shaders initialized." << endl;
#endif
    cout << "Shaders initialized." << endl;
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
    //glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT); // done by background management
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
    glMultMatrixf(mat_1.m);

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    camera->glLookAt();
    glColor3f(1,1,1);												// set the color to white

 // placement de la source de lumière
    glLightfv(GL_LIGHT0, GL_POSITION, camera->ls.pos);
    glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, camera->ls.dir );
    glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, camera->ls.falloffAngle);

    glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLightColor);
    glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLightColor);
    glLightfv(GL_LIGHT0, GL_SPECULAR, specularLightColor);

// activation du programme de shader
    if(useShaders && shadersProgram) {
        glUseProgramObjectARB(shadersProgram);
    } else {
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

    if (camera->ls.showCone) {
        camera->ls.glDraw();
    }

}

void shadowedRenderingStep4() {
    glActiveTextureARB(GL_TEXTURE1_ARB);
    glDisable(GL_TEXTURE_2D);
    glActiveTextureARB(GL_TEXTURE0_ARB);
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
#ifdef DEBUG_GRAPHICS
        OUTPUT << "warning: Impossible de compiler le program :\n" << log  <<endl;
#endif
        cout << "warning: Impossible de compiler le program :\n" << log  <<endl;
    } else {
#ifdef DEBUG_GRAPHICS
        OUTPUT << "compilation OK" << endl;
#endif
        cout << "compilation OK" << endl;
    }
    return compile_status;
}

void noshadowRenderingStart(Camera *camera) {
    glClearDepth (1.0f);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    camera->glProjection();
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    camera->glLookAt();
    glColor3f(1,1,1);												// set the color to white

    // placement de la source de lumière
    glLightfv(GL_LIGHT0, GL_POSITION, camera->ls.pos);
    glLightfv(GL_LIGHT0, GL_SPOT_DIRECTION, camera->ls.dir );
    glLightf(GL_LIGHT0, GL_SPOT_CUTOFF, camera->ls.falloffAngle);

    glLightfv(GL_LIGHT0, GL_DIFFUSE, diffuseLightColor);
    glLightfv(GL_LIGHT0, GL_AMBIENT, ambientLightColor);
    glLightfv(GL_LIGHT0, GL_SPECULAR, specularLightColor);
// activation du programme de shader
    if(useShaders && shadersProgram) {
        glUseProgramObjectARB(shadersProgram);
    } else {
        glEnable(GL_LIGHTING);
    }

    glUniform1iARB(locTex, 0);
    glBindTexture(GL_TEXTURE_2D,0);
    enableTexture(true);
    glActiveTexture(GL_TEXTURE0);
    glEnable(GL_TEXTURE_2D);
}

void noshadowRenderingStop() {
    glActiveTexture(GL_TEXTURE0);
    glDisable(GL_TEXTURE_2D);

    if(useShaders) glUseProgramObjectARB(0);
    glFlush ();	// Flush The GL Rendering Pipeline
}

/////////////////////////////////////////////////////////////////////////////
// loadTextures
// lecture de l'identifiant de texture
GLuint loadTexture(const char *titre, int &tw, int &th) {
    unsigned char *image;
    GLuint id = 0;
#ifdef DEBUG_GRAPHICS
    OUTPUT << "loading " << titre << endl;
#endif
    if (!(image = lectureTarga(titre, tw, th))) {
#ifdef DEBUG_GRAPHICS
        ERRPUT << "Error : can't open " << titre << endl;
#endif
    } else {
        glGenTextures(1, &id);
        glBindTexture(GL_TEXTURE_2D, id);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
        glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
        gluBuild2DMipmaps(GL_TEXTURE_2D, GL_RGBA, tw, th, GL_RGBA, GL_UNSIGNED_BYTE, image);
        glTexEnvf(GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_REPLACE);
        delete[] image;
    }
    return id;
}

unsigned char *lectureTarga(const char *titre, int &width, int &height, bool turn) {
#define DEF_targaHeaderLength            12
#define DEF_targaHeaderContent        "\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00"

    ifstream fin;
    char *pData;
    streampos maxLen = 0;

    fin.open(titre, ios::binary);
    if (!fin.is_open()) return NULL;

// calcul la longueur du fichier
    fin.seekg(0, ios::end);
    maxLen = fin.tellg();
    fin.seekg(0, ios::beg);

    // allocation de la mémoire pour le fichier
    pData = new char[int(maxLen)];

    // lecture des données du fichier
    fin.read(pData, maxLen);

    fin.close();

    int commentOffset = int((unsigned char) *pData);
    if (memcmp(pData + 1, DEF_targaHeaderContent, DEF_targaHeaderLength - 1) != 0) {
#ifdef DEBUG_GRAPHICS
        ERRPUT << "Format non reconnu : " << titre << endl;
#endif
        return 0;
    }
    unsigned char smallArray[2];

    memcpy(smallArray, pData + DEF_targaHeaderLength + 0, 2);
    width = smallArray[0] + smallArray[1] * 0x0100;

    memcpy(smallArray, pData + DEF_targaHeaderLength + 2, 2);
    height = smallArray[0] + smallArray[1] * 0x0100;

    memcpy(smallArray, pData + DEF_targaHeaderLength + 4, 2);
    int depth = smallArray[0];
//	int pixelBitFlags = smallArray[ 1 ];

    if ((width <= 0) || (height <= 0))
        return 0;

    // Only allow 24-bit and 32-bit!
    bool is24Bit(depth == 24);
    bool is32Bit(depth == 32);
    if (!(is24Bit || is32Bit))
        return 0;

    // Make it a BGRA array for now.
    int bodySize(width * height * 4);
    unsigned char *pBuffer = new unsigned char[bodySize];
    if (is32Bit) {
        // Easy, just copy it.
        memcpy(pBuffer, pData + DEF_targaHeaderLength + 6 + commentOffset, bodySize);
    } else if (is24Bit) {
        int bytesRead = DEF_targaHeaderLength + 6 + commentOffset;
        for (int loop = 0; loop < bodySize; loop += 4, bytesRead += 3) {
            memcpy(pBuffer + loop, pData + bytesRead, 3);
            pBuffer[loop + 3] = 255;            // Force alpha to max.
        }
    } else return NULL;

    // Swap R & B (convert to RGBA).
    for (int loop = 0; loop < bodySize; loop += 4) {
        unsigned char tempC = pBuffer[loop + 0];
        pBuffer[loop + 0] = pBuffer[loop + 2];
        pBuffer[loop + 2] = tempC;
    }

    delete[] pData;


    if (turn) {
        unsigned char *pBufferRet = new unsigned char[bodySize],
                *ptr1 = pBuffer + width * (height - 1) * 4, *ptr2 = pBufferRet;
        for (int loop = 0; loop < height; loop++) {
            memcpy(ptr2, ptr1, width * 4);
            ptr2 += width * 4;
            ptr1 -= width * 4;
        }
        delete[] pBuffer;
        return pBufferRet;
    }
    // Ownership moves out.
    return pBuffer;
}

