/*!
 * @file replayGlutContext.h
 * @brief Contains all graphical elements, draw interface and manage user controls
 * @author Mattéo Daluz
 */

#ifndef REPLAYGLUTCONTEXT
#define REPLAYGLUTCONTEXT
#pragma once

#include <fstream>
#include <map>
#include "../../simulatorCore/src/gui/objLoader.h"
#include "replay.hpp"
#include "replayPlayer.h"
#include "../../simulatorCore/src/gui/camera.h"
#include "../../simulatorCore/src/replay/replayTags.h"
#include "replayInterface.h"
class ReplayWorld;
using namespace ReplayTags;
class Button;

namespace GlutContext {
    //Cottes pour la fenetre de commandes en fonction des dimensions


    class ReplayGlutContext {

        /*********************************************************/
/* global variables                                      */
    public:


        static inline int width = 1024, height = 600, toolHeight = 120, separ = 24; // initial size of the screen
        static inline float offsetX=0.01f, offsetY=0.1f;
        static inline float timelineHeight=0.5f, timelineOffset=0.05f;
        static inline float toolsSeparationY=0.05f;
        static inline float toolsButtonSize=0.3f;
        static inline float toolbarOffsetX=0.3f, buttonSeparation=0.005f;
        static inline float recButtonOffset=0.1f;
        static inline float timelineX = width * (1 - 2 * offsetX), timelineY = timelineHeight * toolHeight;
        static inline float stepDuration = 0.0f;
        static inline ReplayWorld *world= nullptr;

        //Buttons
        static inline vector<Button*> buttons  = {};

        //Keyframes show time
        static inline vector<float> keyframesTime = {};
        static inline bool showKeyframesTime = false;


        static inline u1 replayMode = REPLAY_MODE_PAUSE;


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
        static inline float rotationAngle = 0.0f; // rotation angle of the teapot /z
        //static inline int mouseX0, mouseY0;
        static inline GLint topWindow;
        static inline GLint mainWindow;
        static inline GLint toolsWindow;
        static inline bool toolsWinOpened=true;
        static inline bool enableShadows=false; // BPi todo for true
        static inline int keyboardModifier=0;

        /**
         * Initialize the windows and bind functions to actions
         * @param argc
         * @param argv
         */
        static void init(int argc, char *argv[]);

        /**
         * Initialize the Camera
         */
        static void initGL();

        /**
         * Called when the window size is changed by the user and updates window size variables
         */
        static void reshapeFunc(int, int);
        static void reshapeFuncMW(int, int);
        static void reshapeFuncTW(int, int);

        /**
         * Called to draw frames
         */
        static void drawFunc();
        static void drawFuncMW();
        static void drawFuncTW();

        /**
         * Starts glut mainloop
         */
        static void mainLoop();

        static void setWorld(ReplayWorld* replayWorld);

        /**
         * Bind keyboard actions
         */
        static void kbdFunc(unsigned char, int, int);

        /**
         * Called on click
         * @param button
         * @param state
         * @param x
         * @param y
         */
        static void mouseFunc(int button, int state, int x, int y);
        static void mouseFuncMW(int button, int state, int x, int y);
        static void mouseFuncTW(int button, int state, int x, int y);

        /**
         * Called when the user drags his mouse
         * @param x
         * @param y
         */
        static void motionFuncMW(int x, int y);
        static void motionFuncTW(int x, int y);

        /**
         * Animation function
         */
        static void idleFunc();


        static void quit();

        /**
         * Manage Subwindows placement
         */
        static void updateSubWindows();

        /**
         * Draws a white square for a button
         */
        static void drawNextButtonSquare();

        /**
         * Draws the timeline in the tool window
         */
        static void drawTimeline();

        /**
         * Draw the string str in the tool window
         * @param ix
         * @param iy
         * @param str
         */
        static void drawString(int ix,int iy,char* str);

        /**
         * @brief Starts Play mode
         */
        static void play();
    };

}

#endif
