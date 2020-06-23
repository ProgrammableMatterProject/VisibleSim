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

#include "../math/matrix44.h"
#include "../gui/camera.h"

using namespace std;

GLint shaderCompilationStatus(GLhandleARB shader);
void initShaders(bool enableShadows);
void enableTexture(bool enable);
void shadowedRenderingStep1(Camera *camera);
void shadowedRenderingStep2(int w,int h);
void shadowedRenderingStep3(Camera *camera);
void shadowedRenderingStep4();
void noshadowRenderingStart(Camera *camera);
void noshadowRenderingStop();

unsigned char *lectureTarga(const char *titre, int &width, int &height, bool turn=false);
GLuint loadTexture(const char *titre, int &tw, int &th);
#endif
