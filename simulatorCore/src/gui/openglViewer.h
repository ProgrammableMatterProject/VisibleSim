/*
 * openglViewer.h
 *
 *  Created on: 21/02/2012
 *      Author: ben
 */

#ifndef OPENGLVIEWER_H_
#define OPENGLVIEWER_H_

#include <pthread.h>

#include "gui/shaders.h"
#include "math/matrix44.h"
#include "gui/camera.h"
#include "base/glBlock.h"
#include "interface.h"

#ifndef GLUT
#define GLUT
#endif

class GlutContext;
//===========================================================================================================
//
//          GlutContext  (class)
//
//===========================================================================================================
class GlutContext {
    static int keyboardModifier;
public :
    static bool GUIisEnabled; //!< Enable / Disable GLUT graphical simulation, enabled by default
    static GlutSlidingMainWindow *mainWindow;
    // static GlutSlidingDebugWindow *debugWindow;
    static GlutPopupWindow *popup;
    static GlutPopupMenuWindow *popupMenu;
    static GlutPopupMenuWindow *popupSubMenu;
    static GlutHelpWindow *helpWindow;
    static int screenWidth, screenHeight;
    static int initialScreenWidth, initialScreenHeight;
    static bool fullScreenMode;
    static bool saveScreenMode;
    static int lastMotionTime;
    static int lastMousePos[2];
    static bool mustSaveImage;
// FPS counter
    static int frameCount;
    static int previousTime;
    static float fps;
    static unsigned int nbModules;
    static long unsigned int timestep;
//	bool showLinks;

    static void init(int argc, char **argv);
    static void deleteContext();
    static void mainLoop(void);
    static void addTrace(const string &str,int id,const Color &color);
    static void reshapeFunc(int w,int h);
    static void setFullScreenMode(bool b);
private :
    static void passiveMotionFunc(int x,int y);
    static void motionFunc(int x,int y);
    static void mouseFunc(int button,int state,int x,int y);
    static void keyboardFunc(unsigned char c, int x, int y);
    static void specialFunc(int key, int x, int y);
    static void drawFunc(void);
    static void idleFunc(void);
    static int selectFunc(int x,int y);
    static int selectFaceFunc(int x,int y);
    static int processHits(GLint hits, GLuint *buffer);
    static bool saveScreen(const char *title);
    static void *lanceScheduler(void *param);
    static void calculateFPS(void);
    static void calculateSimulationInfo(void);
    static void showFPS(void);
    static void showSimulationInfo(void);
};
#endif
