#ifndef _shaders_h
#define _shaders_h

#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <fstream>

#include <GL/glew.h>
#include <GL/freeglut.h>

#if defined(__APPLE__)
#include <OpenGL/gl.h>
#include <OpenGL/glu.h>
#else
#include <GL/gl.h>
#include <GL/glu.h>
#endif

#include "math/matrix44.h"
#include "gui/camera.h"

using namespace std;

GLint shaderCompilationStatus(GLhandleARB shader);
void initShaders();
void enableTexture(bool enable);
//void drawShadowedScene();
void shadowedRenderingStep1(Camera *camera);
void shadowedRenderingStep2(int w,int h);
void shadowedRenderingStep3(Camera *camera);
void shadowedRenderingStep4();
#endif
