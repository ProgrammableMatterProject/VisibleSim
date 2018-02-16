/*
 * openglViewer.cpp
 *
 *  Created on: 21/02/2012
 *      Author: Ben
 */

#include "openglViewer.h"

#include <chrono>

#include "world.h"
#include "scheduler.h"
#include "simulator.h"
#include "events.h"
#include "trace.h"

#ifdef ENABLE_MELDPROCESS
#include "meldProcessDebugger.h"
#endif

//===========================================================================================================
//
//          GlutContext  (class)
//
//===========================================================================================================

bool GlutContext::GUIisEnabled = true;

int GlutContext::screenWidth = 1024;
int GlutContext::screenHeight = 800;
int GlutContext::initialScreenWidth = 1024;
int GlutContext::initialScreenHeight = 800;
int GlutContext::keyboardModifier = 0;
int GlutContext::lastMotionTime=0;
int GlutContext::lastMousePos[2];
//bool GlutContext::showLinks=false;
bool GlutContext::fullScreenMode=false;
bool GlutContext::saveScreenMode=false;
bool GlutContext::mustSaveImage=false;
GlutSlidingMainWindow *GlutContext::mainWindow=NULL;
GlutSlidingDebugWindow *GlutContext::debugWindow=NULL;
GlutPopupWindow *GlutContext::popup=NULL;
GlutPopupMenuWindow *GlutContext::popupMenu=NULL;
GlutHelpWindow *GlutContext::helpWindow=NULL;
int GlutContext::frameCount = 0;
int GlutContext::previousTime = 0;
float GlutContext::fps = 0;

void GlutContext::init(int argc, char **argv) {
	if (GUIisEnabled) {
		glutInit(&argc,argv);
		glutSetOption(GLUT_ACTION_ON_WINDOW_CLOSE,GLUT_ACTION_CONTINUE_EXECUTION);
		glutInitDisplayMode(GLUT_RGBA | GLUT_DEPTH | GLUT_DOUBLE);

		// creation of a new graphic window
		glutInitWindowPosition(0, 0);
		glutInitWindowSize(screenWidth,screenHeight);
		if (glutCreateWindow("VisibleSim") == GL_FALSE) {
			puts("ERREUR : echec à la création de la fenêtre graphique");
			exit(EXIT_FAILURE);
		}

		if(fullScreenMode) {
			glutFullScreen();
		}

		initShaders();

		////// GL parameters /////////////////////////////////////
		glEnable(GL_DEPTH_TEST);
		glShadeModel(GL_SMOOTH);
		glEnable(GL_CULL_FACE);
		glCullFace(GL_BACK);
		glEnable(GL_NORMALIZE);

		glClearColor(0.3f,0.3f,0.8f,0.0f);

		glEnable(GL_LIGHTING);
		glEnable(GL_LIGHT0);
		glEnable(GL_NORMALIZE);

		glutReshapeFunc(reshapeFunc);
		glutDisplayFunc(drawFunc);
		glutMouseFunc(mouseFunc);
		glutMotionFunc(motionFunc);
		glutPassiveMotionFunc(passiveMotionFunc);
		glutKeyboardFunc(keyboardFunc);
		glutIdleFunc(idleFunc);

		mainWindow = new GlutSlidingMainWindow(screenWidth-40,60,40,screenHeight-60,
											   "../../simulatorCore/resources/textures/UITextures/fenetre_onglet.tga");
		debugWindow = new GlutSlidingDebugWindow(screenWidth-40,60,40,screenHeight-60,
												 "../../simulatorCore/resources/textures/UITextures/fenetre_ongletDBG.tga");
		popup = new GlutPopupWindow(NULL,0,0,140,30);
	}
}

void GlutContext::deleteContext() {
    if (GUIisEnabled) {
        delete mainWindow;
        delete debugWindow;
        delete popup;
        //delete popupMenu;
    }
}

void *GlutContext::lanceScheduler(void *param) {
    int mode = *(int*)param;
    BaseSimulator::getScheduler()->start(mode);
    return(NULL);
}

//////////////////////////////////////////////////////////////////////////////
// fonction de changement de dimensions de la fenetre,
// - width  : largeur (x) de la zone de visualisation
// - height : hauteur (y) de la zone de visualisation
void GlutContext::reshapeFunc(int w,int h) {
    screenWidth=w;
    screenHeight=h;
    Camera* camera=getWorld()->getCamera();
    camera->setW_H(double(w)/double(h));
    // size of the OpenGL drawing area
    glViewport(0,0,w,h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    camera->glProjection();
    // camera intrinsic parameters
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    mainWindow->reshapeFunc(w-40,60,40,h-60);
    debugWindow->reshapeFunc(w-40,60,40,h-60);
}

//////////////////////////////////////////////////////////////////////////////
// fonction associée aux interruptions générées par la souris bouton pressé
// - x,y : coordonnée du curseur dans la fenêtre
void GlutContext::motionFunc(int x,int y) {
    if (popup->isShown()) {
        glutPostRedisplay();
        popup->show(false);
    }
    if (mainWindow->mouseFunc(-1,GLUT_DOWN,x,screenHeight - y)>0) return;
    if (debugWindow->mouseFunc(-1,GLUT_DOWN,x,screenHeight - y)>0) return;
    if (keyboardModifier!=GLUT_ACTIVE_CTRL) { // rotation du point de vue
        Camera* camera=getWorld()->getCamera();
        camera->mouseMove(x,y);
        glutPostRedisplay();
    }
}

void GlutContext::passiveMotionFunc(int x,int y) {
    if (popup->isShown()) {
        glutPostRedisplay();
        popup->show(false);
    }
    if (helpWindow && helpWindow->passiveMotionFunc(x,screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    if (popupMenu && popupMenu->passiveMotionFunc(x,screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    if (mainWindow->passiveMotionFunc(x,screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    if (debugWindow->passiveMotionFunc(x,screenHeight - y)) {
        glutPostRedisplay();
        return;
    }
    lastMotionTime = glutGet(GLUT_ELAPSED_TIME);
    lastMousePos[0]=x;
    lastMousePos[1]=y;
}

//////////////////////////////////////////////////////////////////////////////
// fonction associée aux interruptions générées par le clic de souris
// - bouton : code du bouton
// - state : état des touches du clavier
// - x,y : coordonnée du curseur dans la fenêtre
void GlutContext::mouseFunc(int button,int state,int x,int y) {
    if (mainWindow->mouseFunc(button,state,x,screenHeight - y)>0) {
        glutPostRedisplay();
        return;
    }
    if (debugWindow->mouseFunc(button,state,x,screenHeight - y)>0) {
        glutPostRedisplay();
        return;
    }
    if (popupMenu) {
        int n=popupMenu->mouseFunc(button,state,x,screenHeight - y);
        if (n) {
            popupMenu->show(false);
            getWorld()->menuChoice(n);
        }
    }
    if (helpWindow) helpWindow->mouseFunc(button,state,x,screenHeight - y);

    keyboardModifier = glutGetModifiers();
    if (keyboardModifier!=GLUT_ACTIVE_CTRL) { // rotation du point de vue
        Camera* camera=getWorld()->getCamera();
        switch (button) {
        case GLUT_LEFT_BUTTON:
            if (state==GLUT_DOWN) {
                camera->mouseDown(x,y);
            } else
                if (state==GLUT_UP) {
                    camera->mouseUp(x,y);
                }
            break;
        case GLUT_RIGHT_BUTTON:
            if (state==GLUT_DOWN) {
                camera->mouseDown(x,y,true);
            } else
                if (state==GLUT_UP) {
                    camera->mouseUp(x,y);
                }
            break;
        case 3 :
            camera->mouseZoom(-10);
            break;
        case 4 :
            camera->mouseZoom(10);
            break;
        }
        //cout << *camera << endl;
    } else { // selection of the clicked block
        if (state==GLUT_UP) {
            int n=selectFunc(x,y);
            GlBlock *slct=BaseSimulator::getWorld()->getselectedGlBlock();
            // unselect current if exists
            if (slct) slct->toggleHighlight();
            // set n-1 block selected block (no selected block if n=0
            if (n) BaseSimulator::getWorld()->setselectedGlBlock(n-1)->toggleHighlight();
            else BaseSimulator::getWorld()->setselectedGlBlock(-1);
            mainWindow->select(BaseSimulator::getWorld()->getselectedGlBlock());
            if (button==GLUT_RIGHT_BUTTON && n) {
                int n=selectFaceFunc(x,y);
                cout << "selected " << n << endl;
                if (n>0) {
                    BaseSimulator::getWorld()->setSelectedFace(n-1);
                    BaseSimulator::getWorld()->createPopupMenu(x,y);
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
void GlutContext::keyboardFunc(unsigned char c, int x, int y)
{
    //  static int modeScheduler;
    Camera* camera=getWorld()->getCamera();
    // si une interface a le focus
    if (debugWindow->keyFunc(c)) {

    } else {
        switch(c) {
            case 27 : case 'q' : case 'Q' : // quit
                glutLeaveMainLoop();
                break;
            case 'f' : glPolygonMode(GL_FRONT_AND_BACK,GL_LINE); break;
            case 'F' : glPolygonMode(GL_FRONT_AND_BACK,GL_FILL); break;
            case '+' : camera->mouseZoom(0.5); break;
            case '-' : camera->mouseZoom(-0.5); break;
            case 'T' : case 't' :
                if (mainWindow->getTextSize()==TextSize::TEXTSIZE_STANDARD) {
                    mainWindow->setTextSize(TextSize::TEXTSIZE_LARGE);
                } else {
                    mainWindow->setTextSize(TextSize::TEXTSIZE_STANDARD);
                }
                break;
                //  case 'l' : showLinks = !showLinks; break;
            case 'r' : getScheduler()->start(SCHEDULER_MODE_REALTIME); break;
    //          case 'p' : getScheduler()->pauseSimulation(getScheduler()->now()); break;
                //case 'p' : BlinkyBlocks::getDebugger()->handlePauseRequest(); break;
            case 'd' : getScheduler()->stop(getScheduler()->now()); break;
            case 'R' : getScheduler()->start(SCHEDULER_MODE_FASTEST); break;
                //case 'u' : BlinkyBlocks::getDebugger()->unPauseSim(); break;
            case 'z' : {
                World *world = BaseSimulator::getWorld();
                GlBlock *slct=world->getselectedGlBlock();
                if (slct) {
                    world->getCamera()->setTarget(slct->getPosition());
                }
            }
                break;
            case 'w' : case 'W' :
                fullScreenMode = !fullScreenMode;
                if (fullScreenMode) {
                    glutFullScreen();
                } else {
                    glutReshapeWindow(initialScreenWidth,initialScreenHeight);
                    glutPositionWindow(0,0);
                }
                break;
            case 'h' :
                if (!helpWindow) {
                    BaseSimulator::getWorld()->createHelpWindow();
                }
                helpWindow->showHide();
                break;
            case 'i' : case 'I' :
                mainWindow->openClose();
                break;
            case 's' : saveScreenMode=!saveScreenMode; break;
            case 'S' : saveScreen((char *)("capture.ppm")); break;
            case 'B' : {
					World *world = BaseSimulator::getWorld();
					world->toggleBackground();
				}
                break;
			case 'p' : 
				BaseSimulator::getWorld()->simulatePolymer();
				break;

        }
    }
    glutPostRedisplay();
}


//////////////////////////////////////////////////////////////////////////////
// fonction de mise à jour des données pour l'animation
void GlutContext::idleFunc(void) {
    std::chrono::milliseconds timespan(20);
    std::this_thread::sleep_for(timespan);

    //calculateFPS();
    if (saveScreenMode && mustSaveImage) {
        static int num=0;
        char title[16];
        sprintf(title,"save%04d.ppm",num++);
        saveScreen(title);
        mustSaveImage=false;
    }
    if (lastMotionTime) {
        int tm = glutGet(GLUT_ELAPSED_TIME);
        if (tm-lastMotionTime>100) {
            int n=selectFunc(lastMousePos[0],lastMousePos[1]);
            if (n) {
                GlBlock *slct=BaseSimulator::getWorld()->getBlockByNum(n-1);
                popup->setCenterPosition(lastMousePos[0],screenHeight - lastMousePos[1]);
                popup->setInfo(slct->getPopupInfo());
                popup->show(true);
            } else {
                popup->show(false);
            }
            lastMotionTime=0;
            glutPostRedisplay();
        }
    }
    if (mainWindow->hasselectedGlBlock() || getScheduler()->state==Scheduler::RUNNING) {
        glutPostRedisplay(); // for blinking
    }
}

void GlutContext::calculateFPS(void) {
    frameCount++;
    int currentTime = glutGet(GLUT_ELAPSED_TIME);

    //  Calculate time passed
    int timeInterval = currentTime - previousTime;
    if(timeInterval > 1000)
    {
        fps = frameCount / (timeInterval / 1000.0f);
        previousTime = currentTime;
        frameCount = 0;
    }
}

void GlutContext::showFPS(void) {
    char fpsStr[50];
    sprintf(fpsStr, "FPS = %4.2f", fps);
    GlutWindow::drawString(50, 50, fpsStr);
}

void GlutContext::drawFunc(void) {
    World *wrl = getWorld();
    Camera*camera=wrl->getCamera();

    shadowedRenderingStep1(camera);
    glPushMatrix();
    wrl->glDraw();
    glPopMatrix();

    shadowedRenderingStep2(screenWidth,screenHeight);

    shadowedRenderingStep3(camera);
    glPushMatrix();
    wrl->glDraw();
    glPopMatrix();
    shadowedRenderingStep4();

    // drawing of the interface
    glEnable(GL_BLEND);
    glDisable(GL_DEPTH_TEST);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glMatrixMode(GL_PROJECTION);
    glPushMatrix();
    glLoadIdentity();
    gluOrtho2D(0,screenWidth,0,screenHeight);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    mainWindow->glDraw();
    debugWindow->glDraw();
    popup->glDraw();
    if (popupMenu) popupMenu->glDraw();
    if (helpWindow) helpWindow->glDraw();
    //showFPS();

    glEnable(GL_DEPTH_TEST);
    glutSwapBuffers();
}

//////////////////////////////////////////////////////////////////////////////
// fonction de détection d'un objet à l'écran en position x,y.
int GlutContext::selectFunc(int x,int y) {
    GLuint selectBuf[512];
    GLint hits;
    GLint viewport[4];
    Camera* camera=getWorld()->getCamera();

    glGetIntegerv(GL_VIEWPORT,viewport); // récupération de la position et de la taille de la fenêtre

    glSelectBuffer(512,selectBuf);
    glRenderMode(GL_SELECT);
    glInitNames();
    glPushName(0);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPickMatrix((float) x,(float)(screenHeight-y),3.0,3.0,viewport);
    camera->glProjection();
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    camera->glLookAt();

    glPushMatrix();
    BaseSimulator::getWorld()->glDrawId();
    glPopMatrix();

    glFlush();
    hits = glRenderMode(GL_RENDER);
    return processHits(hits,selectBuf);
}

//////////////////////////////////////////////////////////////////////////////
// fonction de détection d'un objet à l'écran en position x,y.
int GlutContext::selectFaceFunc(int x,int y) {
    GLuint selectBuf[512];
    GLint hits;
    GLint viewport[4];
    Camera* camera=getWorld()->getCamera();

    glGetIntegerv(GL_VIEWPORT,viewport); // récupération de la position et de la taille de la fenêtre

    glSelectBuffer(512,selectBuf);
    glRenderMode(GL_SELECT);
    glInitNames();
    glPushName(0);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPickMatrix((float) x,(float)(screenHeight-y),3.0,3.0,viewport);
    camera->glProjection();
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    camera->glLookAt();

    glPushMatrix();
    BaseSimulator::getWorld()->glDrawIdByMaterial();
    glPopMatrix();

    glFlush();
    hits = glRenderMode(GL_RENDER);
    return processHits(hits,selectBuf);
}

//////////////////////////////////////////////////////////////////////////////
// recherche du premier élément dans le tableau d'objet cliqués
// tableau d'entiers : { [name,zmin,zmax,n],[name,zmin,zmax,n]...}
int GlutContext::processHits(GLint hits, GLuint *buffer) {
    if (hits==0) {
        return 0;
    }
    GLuint *ptr=buffer;
    GLuint nmini = ptr[3];
    GLuint zmini = ptr[1];
    ptr+=4;
    for (int i=1; i<hits; i++)
    { if (ptr[1]<zmini)
        { zmini = ptr[1];
            nmini = ptr[3];
        }
        ptr+=4;
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
//		Scheduler *s = getScheduler();
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

void GlutContext::addTrace(const string &message,int id,const Color &color) {
    if (GUIisEnabled && mainWindow)
        mainWindow->addTrace(id,message,color);
}

bool GlutContext::saveScreen(char *title) {
#ifdef WIN32
    FILE *fichier;
    fopen_s(&fichier,title,"wb");
#else
    FILE *fichier = fopen(title,"wb");
#endif
    if (!fichier) return false;
    unsigned char *pixels;
    int w,h;

    w = glutGet(GLUT_WINDOW_WIDTH);
    h = glutGet(GLUT_WINDOW_HEIGHT);
    if (w%4!=0) w=(int(w/4))*4;

    pixels = (unsigned char*) malloc(3*w*h);
    glReadPixels(0,0,w,h,GL_RGB,GL_UNSIGNED_BYTE,(GLvoid*) pixels);
    Time t = BaseSimulator::getScheduler()->now();
    fprintf(fichier,"P6\n# time: %d:%d\n%d %d\n255\n",int(t/1000),int(t%1000),w,h);
    unsigned char *ptr = pixels+(h-1)*w*3;
    while (h--) {
        fwrite(ptr,w*3,1,fichier);
        ptr-=w*3;
    }
    fclose(fichier);
    free(pixels);
    return true;
}

void GlutContext::setFullScreenMode(bool b) {
    fullScreenMode = true;
}
