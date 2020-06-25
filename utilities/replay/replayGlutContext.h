/**
 * @file   ReplayGlutContext.h
 * @author Matteo Daluz
 * @date   Tue Jun  9 11:13:33 2020
 *
 * @brief  Glut components for replay player application
 *
 *
 */

#ifndef REPLAYGLUTCONTEXT
#define REPLAYGLUTCONTEXT
#pragma once

#include <fstream>
#include <map>
#include "../../simulatorCore/src/gui/objLoader.h"
#include "replay.hpp"
#include "replayWorld.h"
#include "../../simulatorCore/src/gui/camera.h"

namespace GlutContext {



    //Cottes pour la fenetre de commandes en fonction des dimensions


    class ReplayGlutContext {

        /*********************************************************/
/* global variables                                      */
    public:


        static inline int width, height, toolHeight, separ; // initial size of the screen
        static inline float offsetX, offsetY;
        static inline float timelineHeight, timelineOffset;
        static inline float toolsSeparationY;
        static inline float toolsButtonSize;
        static inline float toolbarOffsetX, buttonSeparation;
        static inline float recButtonOffset;
        static inline float timelineX , timelineY;
        static inline ReplayWorld *world;


        static inline constexpr GLfloat red[4] = {1.0f, 0.0f, 0.0f, 1.0f}; // red color material
        static inline constexpr GLfloat white[4]= {1.0f, 1.0f, 1.0f, 1.0f}; // White color material
        static inline constexpr  GLfloat green[4]= {0.0f, 1.0f, 0.0f, 1.0f}; // green color material
        static inline constexpr GLfloat blue[4] = {0.0f, 0.0f, 1.0f, 1.0f}; // blue color material
        static inline constexpr GLfloat lightgrey[4]= {0.8f, 0.8f, 0.8f, 1.0f}; // lightgrey color material
        static inline constexpr GLfloat grey[4] = {0.4f, 0.4f, 0.4f, 1.0f}; // grey color material
        static inline constexpr GLfloat black[4] = {0.0f, 0.0f, 0.0f, 1.0f}; // black color material

        static bool isIn(int x, int y, int x0, int y0, int w, int h);

        //static inline constexpr float cameraPos[] = {5.0f, 2.0f, 8.0f};;
        //static inline float cameraTheta, cameraPhi, cameraDist; // spherical coordinates of the point of view
        static inline Camera *camera = new Camera(30,30,50.0f);
        static inline float rotationAngle; // rotation angle of the teapot /z
        //static inline int mouseX0, mouseY0;
        static inline GLint topWindow;
        static inline GLint mainWindow;
        static inline GLint toolsWindow;
        static inline bool toolsWinOpened;
        static inline bool enableShadows; // BPi todo for true
        static inline int keyboardModifier=0;

        static void init(int argc, char *argv[]);

        static void initGL();

        static void reshapeFunc(int, int);

        static void reshapeFuncMW(int, int);

        static void reshapeFuncTW(int, int);

        static void drawFunc();

        static void mainLoop();

        static void setWorld(ReplayWorld* replayWorld);

        static void drawFuncMW();

        static void drawFuncTW();

        static void kbdFunc(unsigned char, int, int);

        static void mouseFunc(int button, int state, int x, int y);

        static void mouseFuncMW(int button, int state, int x, int y);

        static void motionFuncMW(int x, int y);

        static void motionFuncTW(int x, int y);

        static void idleFunc();

        static void quit();

        static void updateSubWindows();

        static void drawNextButtonSquare();

        static void drawTimeline();

        static void drawString(int ix,int iy,char* str);

        static void mouseFuncTW(int button, int state, int x, int y);


    };

}

#endif