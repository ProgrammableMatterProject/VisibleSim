/*
 * openglViewer.cpp
 *
 *  Created on: 21/02/2012
 *      Author: Ben
 */


#include <string>
#include <chrono>
#include <cerrno>
#include <sys/stat.h>
#include <future>

#include "openglViewer.h"
#include "../base/world.h"
#include "../events/scheduler.h"
#include "../base/simulator.h"
#include "../events/events.h"
#include "../utils/trace.h"
#include "../utils/utils.h"
#include "../utils/global.h"

// #define showStatsFPS  0

//===========================================================================================================
//
//          GlutContext  (class)
//
//===========================================================================================================
static const int DELAY_FOR_POPUP = 200; // ms
static const int FPS_DELAY = 200; // ms

bool GlutContext::GUIisEnabled = true;

int GlutContext::screenWidth = 1024;
int GlutContext::screenHeight = 800;
int GlutContext::initialScreenWidth = 1024;
int GlutContext::initialScreenHeight = 800;
int GlutContext::keyboardModifier = 0;
int GlutContext::lastMotionTime = 0;
int GlutContext::lastMousePos[2];
//bool GlutContext::showLinks=false;
bool GlutContext::fullScreenMode = false;
bool GlutContext::enableShadows = true;
bool GlutContext::saveScreenMode = false;
GlutSlidingMainWindow *GlutContext::mainWindow = nullptr;
// GlutSlidingDebugWindow *GlutContext::debugWindow=nullptr;
GlutPopupWindow *GlutContext::popup = nullptr;
GlutPopupMenuWindow *GlutContext::popupMenu = nullptr;
GlutPopupMenuWindow *GlutContext::popupSubMenu = nullptr;
GlutHelpWindow *GlutContext::helpWindow = nullptr;
int GlutContext::frameCount = 0;
int GlutContext::previousTime = 0;
float GlutContext::fps = 0;
bool GlutContext::enableShowFPS = false;
bool GlutContext::showGrid = true;
bool GlutContext::enablePopup = false;
bool GlutContext::hasGradientBackground = false;
float GlutContext::bgColor[3] = {0.3, 0.3, 0.8};
float GlutContext::bgColor2[3] = {0.8, 0.8, 0.8};
//GLint GlutContext::mainWinId=0; // TODO new interface with subWindows
//GLint GlutContext::consoleWinId=0; // TODO new interface with subWindows
unsigned int GlutContext::nbModules = 0;
long unsigned int GlutContext::timestep = 0;
bool GlutContext::editMode = false;

std::string animationDirName;

void GlutContext::init(int argc, char **argv) {
    if (GUIisEnabled) {
        cout << "initGlut" << endl;
        glutInit(&argc, argv);

        glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE, GLUT_ACTION_CONTINUE_EXECUTION);
        glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);

        // creation of a new graphic window
        glutInitWindowPosition(0, 0);
        glutInitWindowSize(screenWidth, screenHeight);
        if (glutCreateWindow("VisibleSim [RUN Mode]") == GL_FALSE) {
            puts("ERREUR : echec à la création de la fenêtre graphique");
            exit(EXIT_FAILURE);
        }
        if (fullScreenMode) {
            glutFullScreen();
        }
        cout << "initShaders...";
        initShaders(enableShadows);
        cout << "... ok" << endl;
        ////// GL parameters /////////////////////////////////////
        glEnable(GL_DEPTH_TEST);
        glShadeModel(GL_SMOOTH);
        glEnable(GL_CULL_FACE);
        glCullFace(GL_BACK);
        glEnable(GL_NORMALIZE);

        glClearColor(bgColor[0], bgColor[1], bgColor[2], 1.0f);

        glEnable(GL_LIGHTING);
        glEnable(GL_LIGHT0);
        glEnable(GL_NORMALIZE);

        glutReshapeFunc(reshapeFunc);
        if (enableShadows) {
            glutDisplayFunc(drawFunc);
        } else {
            glutDisplayFunc(drawFuncNoShadows);
        }
        glutMouseFunc(mouseFunc);
        glutMotionFunc(motionFunc);
        glutPassiveMotionFunc(passiveMotionFunc);
        glutKeyboardFunc(keyboardFunc);
        glutSpecialFunc(specialFunc);
        glutIdleFunc(idleFunc);

#ifdef WIN32
        mainWindow = new GlutSlidingMainWindow(screenWidth - 40, 60, 40, screenHeight - 60,
                                               (string(ROOT_DIR) +
                                                "/simulatorCore/resources/textures/UITextures/fenetre_onglet.tga").c_str());
#else
        mainWindow = new GlutSlidingMainWindow(screenWidth-40,60,40,screenHeight-60,
                                               "../../simulatorCore/resources/textures/UITextures/fenetre_onglet.tga");
#endif
        // debugWindow = new GlutSlidingDebugWindow(screenWidth-40,60,40,screenHeight-60,
        //                                       "../../simulatorCore/resources/textures/UITextures/fenetre_ongletDBG.tga");
        popup = new GlutPopupWindow(nullptr, 0, 0, 180, 30);
    }
}

void GlutContext::deleteContext() {
    if (GUIisEnabled) {
        delete mainWindow;
        // delete debugWindow;
        delete popup;
        //delete popupMenu;
    }
}

void *GlutContext::lanceScheduler(void *param) {
    int mode = *(int *) param;
    BaseSimulator::getScheduler()->start(mode);
    return (nullptr);
}

//////////////////////////////////////////////////////////////////////////////
// fonction de changement de dimensions de la fenetre,
// - width  : largeur (x) de la zone de visualisation
// - height : hauteur (y) de la zone de visualisation
void GlutContext::reshapeFunc(int w, int h) {
    screenWidth = w;
    screenHeight = h;
    Camera *camera = World::getWorld()->getCamera();
    camera->setW_H(double(w) / double(h));
    // size of the OpenGL drawing area
    glViewport(0, 0, w, h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    camera->glProjection();
    // camera intrinsic parameters
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    mainWindow->reshapeFunc(w - 40, 60, 40, h - 60);
    // debugWindow->reshapeFunc(w-40,60,40,h-60);
}

//////////////////////////////////////////////////////////////////////////////
// fonction associée aux interruptions générées par la souris bouton pressé
// - x,y : coordonnées du curseur dans la fenêtre
void GlutContext::motionFunc(int x, int y) {
    if (popup->isShown()) {
        glutPostRedisplay();
        popup->show(false);
    }
    if (mainWindow->mouseFunc(-1, GLUT_DOWN, x, screenHeight - y) > 0) return;
    // if (debugWindow->mouseFunc(-1,GLUT_DOWN,x,screenHeight - y)>0) return;
    if (keyboardModifier != GLUT_ACTIVE_CTRL) { // rotation du point de vue
        Camera *camera = World::getWorld()->getCamera();
        camera->mouseMove(x, y);
        glutPostRedisplay();
    }
}

void GlutContext::passiveMotionFunc(int x, int y) {
    if (popup->isShown()) {
        glutPostRedisplay();
        popup->show(false);
    }
    if (helpWindow && helpWindow->passiveMotionFunc(x, screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    if (popupMenu && popupMenu->passiveMotionFunc(x, screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    if (popupMenu && popupSubMenu && popupSubMenu->passiveMotionFunc(x, screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    if (mainWindow->passiveMotionFunc(x, screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    // if (debugWindow->passiveMotionFunc(x,screenHeight - y)) {
    //     glutPostRedisplay();
    //     return;
    // }
    lastMotionTime = glutGet(GLUT_ELAPSED_TIME);
    lastMousePos[0] = x;
    lastMousePos[1] = y;
}

void GlutContext::passiveMotionEditFunc(int x, int y) {
    if (popup->isShown()) {
        glutPostRedisplay();
        popup->show(false);
        GlBlock *slct = BaseSimulator::getWorld()->getselectedGlBlock();
        // unselect current if exists
        if (slct) slct->toggleHighlight();
        BaseSimulator::getWorld()->setselectedGlBlock(-1);
    }
    lastMotionTime = glutGet(GLUT_ELAPSED_TIME);
    lastMousePos[0] = x;
    lastMousePos[1] = y;
}

//////////////////////////////////////////////////////////////////////////////
// fonction associée aux interruptions générées par le clic de souris
// - bouton : code du bouton
// - state : état des touches du clavier
// - x,y : coordonnée du curseur dans la fenêtre
void GlutContext::mouseFunc(int button, int state, int x, int y) {
    if (mainWindow->mouseFunc(button, state, x, screenHeight - y) > 0) {
        glutPostRedisplay();
        return;
    }
    // if (debugWindow->mouseFunc(button,state,x,screenHeight - y)>0) {
    //     glutPostRedisplay();
    //     return;
    // }
    if (popupMenu && popupMenu->isVisible) {
        int n = popupMenu->mouseFunc(button, state, x, screenHeight - y);
        if (n) {
            popupMenu->show(false);
            World::getWorld()->menuChoice(n);
        }
    }
    if (popupSubMenu && popupSubMenu->isVisible) {
        int n = popupSubMenu->mouseFunc(button, state, x, screenHeight - y);
        if (n) {
            popupSubMenu->show(false);
            World::getWorld()->menuChoice(n);
        }
    }
    if (helpWindow) helpWindow->mouseFunc(button, state, x, screenHeight - y);

    keyboardModifier = glutGetModifiers();
    if (keyboardModifier != GLUT_ACTIVE_CTRL) { // rotation du point de vue
        Camera *camera = World::getWorld()->getCamera();
        switch (button) {
            case GLUT_LEFT_BUTTON:
                if (state == GLUT_DOWN) {
                    camera->mouseDown(x, y);
                } else if (state == GLUT_UP) {
                    camera->mouseUp(x, y);
                }
                break;
            case GLUT_RIGHT_BUTTON:
                if (state == GLUT_DOWN) {
                    camera->mouseDown(x, y, true);
                } else if (state == GLUT_UP) {
                    camera->mouseUp(x, y);
                }
                break;
            case 3 :
                camera->mouseZoom(-10);
                break;
            case 4 :
                camera->mouseZoom(10);
                break;
        }
    } else { // selection of the clicked block
        if (state == GLUT_UP) {
            int n = selectFunc(x, y);
            GlBlock *slct = BaseSimulator::getWorld()->getselectedGlBlock();
            // unselect current if exists
            if (slct) slct->toggleHighlight();
            // set n-1 block selected block (no selected block if n=0
            if (n) {
                GlBlock *glB = BaseSimulator::getWorld()->setselectedGlBlock(n);
                glB->toggleHighlight();
                glB->fireSelectedTrigger();
            } else BaseSimulator::getWorld()->setselectedGlBlock(-1);
            mainWindow->select(BaseSimulator::getWorld()->getselectedGlBlock());
            if (button == GLUT_RIGHT_BUTTON && n) {
                int n = selectFaceFunc(x, y);
                cout << "selectFaceFunc=" << n << endl;
                if (n > 0) {
                    BaseSimulator::getWorld()->setSelectedFace(n);
                    BaseSimulator::getWorld()->createPopupMenu(x, y);
                }
            }
        }
    }
    glutPostRedisplay();
}

/////////////////////////////////////run/////////////////////////////////////////
// fonction associée aux interruptions clavier
// - c : caractère saisi
// - x,y : coordonnée du curseur dans la fenètre
void GlutContext::keyboardFunc(unsigned char c, int x, int y) {
    //  static int modeScheduler;
    Camera *camera = World::getWorld()->getCamera();
    // si une interface a le focus
    // if (debugWindow->keyFunc(c)) {

    // } else {
    switch (c) {
        case 27 :
        case 'q' :
        case 'Q' : // quit
            glutLeaveMainLoop();
            break;
        case '+' :
            camera->mouseZoom(10);
            break;
        case '-' :
            camera->mouseZoom(-10);
            break;
        case 'T' :
        case 't' :
            if (mainWindow->getTextSize() == TextSize::TEXTSIZE_STANDARD) {
                mainWindow->setTextSize(TextSize::TEXTSIZE_LARGE);
                popup->setTextSize(TextSize::TEXTSIZE_LARGE);
            } else {
                mainWindow->setTextSize(TextSize::TEXTSIZE_STANDARD);
                popup->setTextSize(TextSize::TEXTSIZE_STANDARD);
            }
            break;
            //  case 'l' : showLinks = !showLinks; break;
        case 'r' :
            getScheduler()->start(SCHEDULER_MODE_REALTIME);
            break;
//          case 'p' : getScheduler()->pauseSimulation(getScheduler()->now()); break;
//          case 'p' : BlinkyBlocks::getDebugger()->handlePauseRequest(); break;
            // case 'd' : getScheduler()->stop(getScheduler()->now()); break;
        case 'R' :
            getScheduler()->start(SCHEDULER_MODE_FASTEST);
            break;
            //case 'u' : BlinkyBlocks::getDebugger()->unPauseSim(); break;
        case 'z' : {
            World *world = BaseSimulator::getWorld();
            GlBlock *slct = world->getselectedGlBlock();
            if (slct) {
                world->getCamera()->setTarget(slct->getPosition());
            }
        }
            break;
        case 'w' :
            fullScreenMode = !fullScreenMode;
            if (fullScreenMode) {
                glutFullScreen();
            } else {
                glutReshapeWindow(initialScreenWidth, initialScreenHeight);
                glutPositionWindow(0, 0);
            }
            break;
        case 'g' : // enable/disable shadows
            enableShadows = !enableShadows;
            initShaders(enableShadows);
            if (enableShadows) {
                glutDisplayFunc(drawFunc);
            } else {
                glutDisplayFunc(drawFuncNoShadows);
            }
            break;
        case 'h' :
            if (!helpWindow) {
                //TODO : IFDEF REPLAY-> Changer l'aide
                BaseSimulator::getWorld()->createHelpWindow();
            }
            helpWindow->showHide();
            break;
        case 'i' :
        case 'I' :
            mainWindow->openClose();
            //glutPostRedisplay();
            break;
        case 'S' : {
            if (not saveScreenMode) {
                // Will start animation capture,
                //  make sure animation directory exists
                int err; //extern int errno;
                struct stat sb;
                animationDirName = generateTimestampedDirName("animation");
                static const char *animationDirNameC = animationDirName.c_str();
                err = stat(animationDirNameC, &sb);
                if (err and errno == ENOENT) {
                    // Create directory
#ifdef WIN32
                    err = mkdir(animationDirNameC);
#else
                    err = mkdir(animationDirNameC, S_IRWXU);
#endif
                    if (err != 0) {
                        cerr << "An error occured when creating the directory for animation export" << endl;
                        break;
                    }
                }
                // else: directory exists, all good
                cerr << "Recording animation frames in directory: "
                     << animationDirName << endl;
            } else {
                cerr << "Recording of " << animationDirName.c_str()
                     << " has ended, attempting conversion" << endl;
                // Add a script for converting into a video, asynchronously
#ifndef WIN32
                (void)std::async([](const std::string& animDir){
                                     const string& bsname = myBasename(Simulator::configFileName);
                                     const string& vidName =
                                         generateTimestampedFilename("video_" + bsname.substr(0, bsname.size()-4), "mkv");
                                     // cout << vidName << endl;
                                     cerr << TermColor::BWhite << "running:"
                                          << TermColor::BYellow << "`ffmpeg -pattern_type glob -framerate 30 -i \""
                                         + animationDirName + "/*.jpg\" " + vidName << "`"
                                          << TermColor::Reset << endl;
                                     int r = system(
                                         string("ffmpeg -pattern_type glob -framerate 30 -i \""
                                                + animationDirName + "/*.jpg\" " + vidName
                                                + ">/dev/null 2>/dev/null").c_str());
                                     if (r == 0) {
                                         system(string("rm -rf " + animationDirName).c_str());
                                         cerr << "Animation video exported to "
                                              << vidName << endl;
                                     } else {
                                         cerr << animationDirName.c_str()
                                              << " conversion failure. Make sure that package ffmpeg is installed on your system (`sudo apt-get install ffmpeg` under Debian/Ubuntu)" << endl;
                                     }
                                 }, animationDirName);
#endif
            }
            saveScreenMode = !saveScreenMode;
        }
            break;
        case 's' :
            if (editMode) {
                getWorld()->exportConfiguration();
            } else {
                const string &bsname = myBasename(Simulator::configFileName);
                const string &ssName = generateTimestampedFilename("capture_" + bsname.substr(0, bsname.size() - 4),
                                                                   "ppm");
                string ssNameJpg = ssName;
                ssNameJpg.replace(ssName.length() - 3, 3, "jpg");
                saveScreen(ssName.c_str());
#ifndef WIN32
                (void)std::async([ssNameJpg, ssName](){
                                              int r = system(string("convert " + ssName + " " + ssNameJpg
                                                                    + " >/dev/null 2>/dev/null").c_str());
                                              if (r == 0)
                                                  system(string("rm -rf " + ssName
                                                                + " >/dev/null 2>/dev/null").c_str());
                                          });
#endif
                cout << "Screenshot saved to files: " << ssName
                     << " and " << ssNameJpg << endl;
            }
            break;
        case 'm' : {
            const string &bsname = myBasename(Simulator::configFileName);
            const string &ssName = generateTimestampedFilename("capture_" + bsname.substr(0, bsname.size() - 4), "ppm");
            string ssNamePNG = ssName;
            ssNamePNG.replace(ssName.length() - 3, 3, "png");
            saveScreen(ssName.c_str());
#ifndef WIN32
            (void)std::async([ssNamePNG, ssName](){
                int r = system(string("convert " + ssName + " PNG:" + ssNamePNG
                                      + " >/dev/null 2>/dev/null").c_str());
                if (r == 0)
                    system(string("rm -rf " + ssName
                                  + " >/dev/null 2>/dev/null").c_str());
            });
#endif
            cout << "Screenshot saved to files: " << ssName
                 << " and " << ssNamePNG << endl;
        }
            break;

        case 'B' :
        case 'b' : {
            /*World *world = BaseSimulator::getWorld();
            world->toggleBackground();*/
            showGrid = !showGrid;
        }
            break;
        case 32: { // SPACE
            Scheduler *scheduler = getScheduler();
            scheduler->toggle_pause();
            if (scheduler->state == Scheduler::State::PAUSED) {
                cout << "[t-" << scheduler->now()
                     << "] Simulation Paused. Press <space> again to resume..." << endl;
            } else {
                cout << "[t-" << scheduler->now()
                     << "] Simulation Resumed." << endl;
            }
        }
            break;
        case '!':
            BaseSimulator::getWorld()->exportSTLModel("model.stl");
            cout << "Exported STL model to file: model.stl" << endl;
            break;
        case '0' :
        case '1' :
        case '2' :
        case '3' :
        case '4' :
        case '5' :
        case '6' :
        case '7' :
        case '8' :
        case '9' : {
            BuildingBlock *bb = BaseSimulator::getWorld()->getSelectedBuildingBlock();
            if (bb) {
                int colorNum = c - '0';
                cout << "Changed color of building block #" << bb->blockId << "to " << ColorNames[colorNum] << endl;
                bb->setColor(colorNum);
            } else {
                cout << "Cannot change color: No selected block" << endl;
            }
        }
            break;
        case ',':
            enableShowFPS = !enableShowFPS;
            break;
        case GLUT_KEY_DELETE :
        case 127 :
        case 'd' : {
            auto wrld = BaseSimulator::getWorld();
            auto bb = wrld->getSelectedBuildingBlock();
            if (editMode && bb) {
                wrld->deleteBlock(bb);
            }
        }
        break;
        case 'l' : {
            BaseSimulator::getWorld()->camera->setLightFromCamera();
        }
        break;
        case 'p' : {
            enablePopup=!enablePopup;
        }
        break;
        default: { // Pass on key press to user blockcode handler
            // NOTE: Since C++ does not handle static virtual functions, we need
            //  to get a pointer to a blockcode and call onUserKeyPressed from
            //  this instance
            BuildingBlock *bb = BaseSimulator::getWorld()->getSelectedBuildingBlock()
                                ? BaseSimulator::getWorld()->getSelectedBuildingBlock()
                                : BaseSimulator::getWorld()->getMap().begin()->second;
            if (bb) bb->blockCode->onUserKeyPressed(c, x, y);
            break;
        }
    }
    glutPostRedisplay();
}

/////////////////////////////////////run/////////////////////////////////////////
// fonction associée aux interruptions clavier de caractères spéciaux
// - key : keycode of the pressed key
// - x,y : window cursor coordinates
void GlutContext::specialFunc(int key, int x, int y) {
    switch (key) {
        case GLUT_KEY_PAGE_UP: {
            BaseSimulator::motionDelayMultiplier /= 1.5f;
            const float minRotationDelayMultiplier = 0.001f;
            if (BaseSimulator::motionDelayMultiplier < minRotationDelayMultiplier) {
                BaseSimulator::motionDelayMultiplier = minRotationDelayMultiplier;
                cout << "Max rotation speed reached: "
                     << BaseSimulator::motionDelayMultiplier << endl;
            } else {
                cout << "Increased rotation speed: "
                     << BaseSimulator::motionDelayMultiplier << endl;
            }
        }
            break;

        case GLUT_KEY_PAGE_DOWN: {
            BaseSimulator::motionDelayMultiplier *= 1.5f;
            const float maxRotationDelayMultiplier = 10.0f;
            if (BaseSimulator::motionDelayMultiplier > maxRotationDelayMultiplier) {
                BaseSimulator::motionDelayMultiplier = maxRotationDelayMultiplier;
                cout << "Min rotation speed reached: "
                     << BaseSimulator::motionDelayMultiplier << endl;
            } else {
                cout << "Decreased rotation speed: "
                     << BaseSimulator::motionDelayMultiplier << endl;
            }
        }
            break;
        case GLUT_KEY_LEFT:
        case GLUT_KEY_RIGHT:
        case GLUT_KEY_UP:
        case GLUT_KEY_DOWN: {
            auto map = BaseSimulator::getWorld()->getMap();
            auto b = map.begin();
            while (b != map.end()) {
                (*b).second->blockCode->onUserArrowKeyPressed(key, x, y);
                b++;
            }
        }
            break;
        case GLUT_KEY_INSERT: {
            auto wrld = BaseSimulator::getWorld();
            auto bb = wrld->getSelectedBuildingBlock();
            if (editMode && bb) {
                auto nsf = wrld->getNumSelectedFace();
                if (nsf != -1) {
                    Cell3DPosition nPos;
                    if (bb->getNeighborPos(nsf, nPos)) {
                        wrld->addBlock(0, bb->buildNewBlockCode, nPos, bb->color);
                        wrld->linkBlock(nPos);
                        wrld->linkNeighbors(nPos);
                        GlBlock *slct = BaseSimulator::getWorld()->getselectedGlBlock();
                        if (slct) slct->toggleHighlight();
                        wrld->setselectedGlBlock(-1);
                    }
                }
            }
        }
            break;
        case GLUT_KEY_F12 : {
            editMode = !editMode;
            if (editMode) {
                glutPassiveMotionFunc(passiveMotionEditFunc);
                glutSetWindowTitle("VisibleSim [EDIT Mode]");
            } else {
                glutPassiveMotionFunc(passiveMotionFunc);
                glutSetWindowTitle("VisibleSim [RUN Mode]");
            }
        }
            break;

    }

    glutPostRedisplay();
}


//////////////////////////////////////////////////////////////////////////////
// fonction de mise à jour des données pour l'animation
void GlutContext::idleFunc(void) {
    std::chrono::milliseconds timespan(20);
    std::this_thread::sleep_for(timespan);

    if (enableShowFPS) calculateFPS();

    if (saveScreenMode) {
        static int num = 0;
        char title[32], titleFormat[32];
        strncpy(titleFormat, animationDirName.c_str(), sizeof(titleFormat));
        strncat(titleFormat, "/save%04d.ppm", sizeof(titleFormat) - strlen(titleFormat) - 1);

        sprintf(title, titleFormat, num++);

        string titleStr = string(title);
        string titleJpg = titleStr;
        titleJpg.replace(titleStr.length() - 3, 3, "jpg");

        saveScreen(title);

        (void) std::async([titleJpg, titleStr]() {
            int r = system(string("convert " + titleStr + " " + titleJpg
                                  + " >/dev/null 2>/dev/null").c_str());
            if (r == 0)
                system(string("rm -rf " + titleStr
                              + " >/dev/null 2>/dev/null").c_str());
        });
    }

    if (lastMotionTime && (GlutContext::enablePopup||editMode)) {
        int tm = glutGet(GLUT_ELAPSED_TIME);
        if (tm - lastMotionTime > DELAY_FOR_POPUP) {
            int n = selectFunc(lastMousePos[0], lastMousePos[1]);
            if (n) {
                //cout << "#" << n << endl;
                World *wrl = BaseSimulator::getWorld();
                GlBlock *slct = wrl->getGlBlock(n);
                if (editMode) {
                    GlBlock *slct = BaseSimulator::getWorld()->getselectedGlBlock();
                    if (slct) slct->toggleHighlight();
                    slct = BaseSimulator::getWorld()->setselectedGlBlock(n);
                    // unselect current if exists
                    slct->toggleHighlight();
                    slct->fireSelectedTrigger();
                    //GlBlock *slct = BaseSimulator::getWorld()->getselectedGlBlock();
                    mainWindow->select(BaseSimulator::getWorld()->getselectedGlBlock());
                    int nf = selectFaceFunc(lastMousePos[0], lastMousePos[1]);
                    //cout << "selectFaceFunc=" << nf << endl;
                    BaseSimulator::getWorld()->setSelectedFace(nf);
                } else {
                    popup->setCenterPosition(lastMousePos[0], screenHeight - lastMousePos[1]);
                    popup->setInfo(slct->getPopupInfo());
                    popup->show(true);
                }
            } else {
                popup->show(false);
                if (editMode) {
                    GlBlock *slct = BaseSimulator::getWorld()->getselectedGlBlock();
                    if (slct) slct->toggleHighlight();
                    BaseSimulator::getWorld()->setselectedGlBlock(-1);
                }
            }
            lastMotionTime = 0;
            glutPostRedisplay();
        }
    }
    if (mainWindow->hasselectedGlBlock() || getScheduler()->state == Scheduler::RUNNING ||
        BaseSimulator::getWorld()->hasBlinkingBlocks()) {
        glutPostRedisplay(); // for blinking
    }
}

void GlutContext::calculateFPS(void) {
    frameCount++;
    int currentTime = glutGet(GLUT_ELAPSED_TIME);

    //  Calculate time passed
    int timeInterval = currentTime - previousTime;
    if (timeInterval > FPS_DELAY) {
        fps = frameCount / (timeInterval / 1000.0f);
        previousTime = currentTime;
        frameCount = 0;
    }
}

void GlutContext::showFPS(void) {
    auto font = GLUT_BITMAP_HELVETICA_18;
    char str[32];

    sprintf(str, "FPS: %4.2f", fps);
    glColor4f(1.0, 1.0, 0.0, 0.75);
    GlutWindow::drawString(30, screenHeight - 40, str, font);
}

void GlutContext::showSimulationInfo(void) {
    auto font = GLUT_BITMAP_HELVETICA_18;
    glColor4f(1.0, 1.0, 1.0, 0.75);

    World *wrl = getWorld();
    //BuildingBlock *bb = wrl->getSelectedBuildingBlock() ? : wrl->getMap().begin()->second;
    BuildingBlock *bb = wrl->getSelectedBuildingBlock();
    if (bb == nullptr && !wrl->getMap().empty()) bb = wrl->getMap().begin()->second;
    if (bb != nullptr) {
        string info = bb->blockCode->onInterfaceDraw();
        int n = count(info.begin(), info.end(), '\n');
        GlutWindow::drawString(40, 25 + n * 20, info.c_str(), font, 20);
    }
}

void GlutContext::drawFunc(void) {
    World *wrl = BaseSimulator::getWorld();
    Camera *camera = wrl->getCamera();

    shadowedRenderingStep1(camera);
    glPushMatrix();
    wrl->glDrawShadows();
    glPopMatrix();

    shadowedRenderingStep2(screenWidth, screenHeight);

    /* Clear and background */
    if (hasGradientBackground) {
        glDisable(GL_LIGHTING);
        glClear(GL_DEPTH_BUFFER_BIT);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        glDisable(GL_DEPTH_TEST);
        glBegin(GL_QUADS);
        glColor3fv(bgColor);
        glVertex2f(-1.0, -1.0);
        glVertex2f(1.0, -1.0);
        glColor3fv(bgColor2);
        glVertex2f(1.0, 1.0);
        glVertex2f(-1.0, 1.0);
        glEnd();
        glEnable(GL_DEPTH_TEST);
    } else {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }

    shadowedRenderingStep3(camera);
    glPushMatrix();
    if (showGrid) {
        wrl->glDrawBackground();
    }
    wrl->glDraw();
    glPopMatrix();

    shadowedRenderingStep4();

    // drawing of the interface
    glEnable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glMatrixMode(GL_PROJECTION);
    //glPushMatrix(); // update BPI, no glPopMatrix without !
    glLoadIdentity();
    gluOrtho2D(0, screenWidth, 0, screenHeight);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    mainWindow->glDraw();
    // debugWindow->glDraw();
    popup->glDraw();
    if (popupMenu && popupMenu->isVisible) {
        popupMenu->glDraw();
        if (popupSubMenu && popupSubMenu->isVisible) popupSubMenu->glDraw();
    }
    if (helpWindow) helpWindow->glDraw();

    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);

    if (enableShowFPS) showFPS();
    showSimulationInfo();

    glEnable(GL_DEPTH_TEST);

    glFlush();
    glutSwapBuffers();
}

void GlutContext::drawFuncNoShadows() {
    World *wrl = BaseSimulator::getWorld();
    Camera *camera = wrl->getCamera();

    /* Clear and background */
    if (hasGradientBackground) {
        glDisable(GL_LIGHTING);
        glClear(GL_DEPTH_BUFFER_BIT);
        glMatrixMode(GL_PROJECTION);
        glLoadIdentity();
        glMatrixMode(GL_MODELVIEW);
        glLoadIdentity();
        glDisable(GL_DEPTH_TEST);
        glBegin(GL_QUADS);
        glColor3fv(bgColor);
        glVertex2f(-1.0, -1.0);
        glVertex2f(1.0, -1.0);
        glColor3fv(bgColor2);
        glVertex2f(1.0, 1.0);
        glVertex2f(-1.0, 1.0);
        glEnd();
        glEnable(GL_DEPTH_TEST);
    } else {
        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    }

    noshadowRenderingStart(camera);
    glPushMatrix();
    wrl->glDraw();

    if (showGrid) {
        wrl->glDrawBackground();
    }
    glPopMatrix();
    noshadowRenderingStop();

    // drawing of the interface
    glEnable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glMatrixMode(GL_PROJECTION);
    //glPushMatrix(); // update BPI, no glPopMatrix !
    glLoadIdentity();
    gluOrtho2D(0, screenWidth, 0, screenHeight);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    mainWindow->glDraw();
    // debugWindow->glDraw();
    popup->glDraw();
    if (popupMenu && popupMenu->isVisible) {
        popupMenu->glDraw();
        if (popupSubMenu && popupSubMenu->isVisible) popupSubMenu->glDraw();
    }
    if (helpWindow) helpWindow->glDraw();

    glDisable(GL_LIGHTING);
    glDisable(GL_TEXTURE_2D);

    if (enableShowFPS) showFPS();
    showSimulationInfo();

    glEnable(GL_DEPTH_TEST);

    glFlush();
    glutSwapBuffers();
}

//////////////////////////////////////////////////////////////////////////////
// fonction de détection d'un objet à l'écran en position x,y.
int GlutContext::selectFunc(int x, int y) {
    GLuint selectBuf[512];
    GLint hits;
    GLint viewport[4];
    Camera *camera = BaseSimulator::getWorld()->getCamera();

    glGetIntegerv(GL_VIEWPORT, viewport); // récupération de la position et de la taille de la fenêtre

    glSelectBuffer(512, selectBuf);
    glRenderMode(GL_SELECT);
    glInitNames();
    glPushName(0);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPickMatrix((float) x, (float) (screenHeight - y), 3.0, 3.0, viewport);
    camera->glProjection();
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    camera->glLookAt();

    glPushMatrix();
    BaseSimulator::getWorld()->glDrawId();
    glPopMatrix();

    glFlush();
    hits = glRenderMode(GL_RENDER);
    return processHits(hits, selectBuf);
}

//////////////////////////////////////////////////////////////////////////////
// fonction de détection d'un objet à l'écran en position x,y.
int GlutContext::selectFaceFunc(int x, int y) {
    GLuint selectBuf[512];
    GLint hits;
    GLint viewport[4];
    Camera *camera = BaseSimulator::getWorld()->getCamera();

    glGetIntegerv(GL_VIEWPORT, viewport); // récupération de la position et de la taille de la fenêtre

    glSelectBuffer(512, selectBuf);
    glRenderMode(GL_SELECT);
    glInitNames();
    glPushName(0);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPickMatrix((float) x, (float) (screenHeight - y), 3.0, 3.0, viewport);
    camera->glProjection();
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    camera->glLookAt();

    glPushMatrix();
    BaseSimulator::getWorld()->glDrawIdByMaterial();
    glPopMatrix();

    glFlush();
    hits = glRenderMode(GL_RENDER);
    return processHits(hits, selectBuf);
}

//////////////////////////////////////////////////////////////////////////////
// recherche du premier élément dans le tableau d'objet cliqués
// tableau d'entiers : { [name,zmin,zmax,n],[name,zmin,zmax,n]...}
int GlutContext::processHits(GLint hits, GLuint *buffer) {
    if (hits == 0) {
        return 0;
    }
    GLuint *ptr = buffer;
    GLuint nmini = ptr[3];
    GLuint zmini = ptr[1];
    ptr += 4;
    for (int i = 1; i < hits; i++) {
        if (ptr[1] < zmini) {
            zmini = ptr[1];
            nmini = ptr[3];
        }
        ptr += 4;
    }
    // traitement d'une selection
    // nmini contient le numéro de l'élément sélectionné
    // celui de z minimum
    return nmini;
}

void GlutContext::mainLoop() {
    Scheduler *s = getScheduler();
    if (GUIisEnabled) {
        glutMainLoop();
        deleteContext();
    } else {
//    cout << "r+[ENTER] to run simulation" << endl;
        std::chrono::milliseconds timespan(2);
        std::this_thread::sleep_for(timespan);
/*    char c='r';
      cin >> c;*/
//    Scheduler *s = getScheduler();
//    if (c=='r') {
        cout << "Run simulation..." << endl;
        cout.flush();

//        sleep(2);
        s->start(s->getSchedulerMode());
        s->waitForSchedulerEnd();
//    }
    }
    s->stop(s->now());

    deleteScheduler();
    std::chrono::milliseconds timespan(500);
    std::this_thread::sleep_for(timespan);

}

void GlutContext::addTrace(const string &message, int id, const Color &color) {
    if (GUIisEnabled && mainWindow)
        mainWindow->addTrace(id, message, color);
}

bool GlutContext::saveScreen(const char *title) {
#ifdef WIN32
    FILE *fichier;
    fopen_s(&fichier, title, "wb");
#else
    FILE *fichier = fopen(title,"wb");
#endif
    if (!fichier) return false;
    unsigned char *pixels;
    int w, h;

    w = glutGet(GLUT_WINDOW_WIDTH);
    h = glutGet(GLUT_WINDOW_HEIGHT);
    if (w % 4 != 0) w = (int(w / 4)) * 4;

    pixels = (unsigned char *) malloc(3 * w * h);
    glReadPixels(0, 0, w, h, GL_RGB, GL_UNSIGNED_BYTE, (GLvoid *) pixels);
    Time t = BaseSimulator::getScheduler()->now();
    fprintf(fichier, "P6\n# time: %d:%d\n%d %d\n255\n", int(t / 1000), int(t % 1000), w, h);
    unsigned char *ptr = pixels + (h - 1) * w * 3;
    while (h--) {
        fwrite(ptr, w * 3, 1, fichier);
        ptr -= w * 3;
    }
    fclose(fichier);
    free(pixels);
    return true;
}

void GlutContext::setFullScreenMode(bool b) {
    fullScreenMode = b;
}

void GlutContext::setShadowsMode(bool b) {
    enableShadows = b;
}
