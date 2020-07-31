/*!
 * @file replayGlutContext.cpp
 * @brief Contains all graphical elements, draw interface and manage user controls
 * @author Mattéo Daluz
 */

#include <iostream>
#include "replayGlutContext.h"
#include <chrono>
#include "../../simulatorCore/src/replay/replayTags.h"

using namespace std;
using namespace Replay;
using namespace GlutContext;

using us = chrono::microseconds;
using get_time = chrono::steady_clock;

void ReplayGlutContext::initGL() {
    camera->setDirection(0,30);
    camera->setDistance(1000.0);
    camera->setNearFar(100.0,10000.0);
    camera->setTarget(Vector3D(0,0,0));
    camera->setAngle(10.0);
    camera->setLightParameters(Vector3D(0.0,0.0,0.0),80.0,50.0,500.0,50.0,1.0,1000.0);
    std::string versionString = std::string((const char*)glGetString(GL_VERSION));
    cout << "Opengl Version: " << versionString << endl;
}

void ReplayGlutContext::quit() {

}

/*********************************************************/
/* Window size update function                           */
/* width: width of the drawing area                      */
/* height: width of the drawing area                     */
void ReplayGlutContext::reshapeFunc(int w,int h) {
    cout << "reshapeFunc:" << w << "," << h << endl;
    width=w;
    height = toolsWinOpened ? h-toolHeight-separ:h-separ;

    glViewport(0,0,w,h);
    // initialize Projection matrix
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0,w,0,h,0.0,1.0);

    // initialize ModelView matrix
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    updateSubWindows();
}

void ReplayGlutContext::updateSubWindows() {

    // place and size mainWindow
    glutSetWindow(mainWindow);
    glutPositionWindow(0,0);
    glutReshapeWindow(width, height);

    glutSetWindow(toolsWindow);
    if (toolsWinOpened) {
        // place and size toolsWindow
        glutShowWindow();
        glutPositionWindow(0, height + separ);
        glutReshapeWindow(width, toolHeight);
    } else {
        glutReshapeWindow(width, 0);
        glutHideWindow();
    }
    for(auto &glButton : buttons)
    {
        glButton->reshapeFunc();
    }
    glutSetWindow(topWindow);
}



void ReplayGlutContext::reshapeFuncMW(int w,int h) {
    cout << "reshapeMW:" << w << "," << h << endl;
    glViewport(0,0,w,h);

    camera->setW_H(double(w)/double(h));

    // size of the OpenGL drawing area
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    camera->glProjection();

    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

void ReplayGlutContext::reshapeFuncTW(int w,int h) {
    cout << "reshapeTW:" << w << "," << h << endl;
    glViewport(0,0,w,h);

    // initialize Projection matrix
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0,w,0,h,0.0,1.0);

    // initialize ModelView matrix
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

/*********************************************************/
/* Animation function                                    */
void ReplayGlutContext::idleFunc(void) {
    static int initTime = glutGet(GLUT_ELAPSED_TIME); // ms
    int currentTime = glutGet(GLUT_ELAPSED_TIME);

    float dt= static_cast<float>(currentTime-initTime)/1000.0f;

    initTime = currentTime;
    //rotationAngle += dt*20.0f; // turn at 20° / s
    if(replayMode == REPLAY_MODE_PLAY)
    {
        if(world->getCurrentTime()+dt<= world->getEndZoom())
        {
            world->setCurrentTime(world->getCurrentTime()+dt);
        }
        else
        {
            world->setCurrentTime(world->getEndZoom());
            replayMode = REPLAY_MODE_PAUSE;
        }
        world->updateMap();
        updateSubWindows();
    }

    glutPostWindowRedisplay(mainWindow);
}

/*********************************************************/
/* Key pressed function                                  */
/* c: key pressed character                              */
/* x,y: mouse coordinates                                */
void ReplayGlutContext::kbdFunc(unsigned char c, int x, int y) {
    switch(c) {
        case 27: case 'q' : // quit
            glutLeaveMainLoop();
            break;
        case 'f' :
            glPolygonMode(GL_FRONT_AND_BACK,GL_LINE);
            break;
        case 'F' :
            glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
            break;
        case 'r' :
            replayMode = REPLAY_MODE_PLAY;
            break;
        case 's' :
            replayMode = REPLAY_MODE_PAUSE;
            break;
    }
    glutPostWindowRedisplay(mainWindow);
}

/*********************************************************/
/* Mouse clicked function                                */
/* button: sum of pressed buttons id                     */
/* state: action                                         */
/* x,y: mouse coordinates                                */
void ReplayGlutContext::mouseFuncMW(int button,int state,int x,int y) {
    keyboardModifier = glutGetModifiers();
    if (keyboardModifier != GLUT_ACTIVE_CTRL) { // rotation du point de vue
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
            case GLUT_KEY_F1 :
                play();
                break;
        }
    }

}

bool ReplayGlutContext::isIn(int x, int y, int x0, int y0, int w, int h) {
    return (x>x0 && y>y0 && x<x0+w && y<y0+h);
}

void ReplayGlutContext::mouseFunc(int button,int state,int x,int y) {
    int hy=toolsWinOpened?height+separ-y:height+separ-y;
    cout << x << ',' << y << "/" << hy << ":" << isIn(x,hy,width-1.5f*separ,0.1f*separ,0.8f*separ,0.8f*separ) << endl;
    if (state==GLUT_UP && isIn(x,hy,width-1.5f*separ,0.1f*separ,0.8f*separ,0.8f*separ)) {
        if (toolsWinOpened) {
            height+=toolHeight;
            toolsWinOpened = false;
        } else {
            height-=toolHeight;
            toolsWinOpened = true;
        }
        updateSubWindows();
        glutPostWindowRedisplay(topWindow);
    }

}

/*********************************************************/
/* Mouse move function (with button pressed)             */
/* x,y: mouse coordinates                                */
void ReplayGlutContext::motionFuncMW(int x,int y) {

    if (keyboardModifier!=GLUT_ACTIVE_CTRL) { // rotation du point de vue
        camera->mouseMove(x,y);
        glutPostRedisplay();
    }
    glutPostWindowRedisplay(mainWindow);
}
void ReplayGlutContext::motionFuncTW(int x,int y)
{
    if(x>=offsetX*width && x<= width*(1-offsetX))
    {
        if(y>=offsetY*toolHeight && y<= toolHeight*(timelineHeight+offsetY))
        {
            if(x<offsetX*width + timelineX*timelineOffset)
            {
                world->setCurrentTime(world->getStartZoom());
            }
            else if(x>width*(1-offsetX)-timelineX*timelineOffset)
            {
                world->setCurrentTime(world->getEndZoom());
            }
            else
            {
                world->setCurrentTime(world->getStartZoom()+
                (x - timelineOffset*timelineX-offsetX*width)*(world->getEndZoom()-world->getStartZoom())/
                                      (width*(1-2*offsetX)-2*timelineX*timelineOffset));
            }
            replayMode = REPLAY_MODE_PAUSE;
            world->updateMap();
        }
    }


    glutPostWindowRedisplay(toolsWindow);
}
void ReplayGlutContext::mouseFuncTW(int button,int state,int x,int y)
{
    if(x>=offsetX*width && x<= width*(1-offsetX))
    {
        if(y>=offsetY*toolHeight && y<= toolHeight*(timelineHeight+offsetY))
        {

            if(x<offsetX*width + timelineX*timelineOffset)
            {
                world->setCurrentTime(world->getStartZoom());
            }
            else if(x>width*(1-offsetX)-timelineX*timelineOffset)
            {
                world->setCurrentTime(world->getEndZoom());
            }
            else
            {
                 world->setCurrentTime(world->getStartZoom()+
                (x - timelineOffset*timelineX-offsetX*width)*(world->getEndZoom()-world->getStartZoom())/
                                     (width*(1-2*offsetX)-2*timelineX*timelineOffset));
            }
            world->updateMap();
            replayMode = REPLAY_MODE_PAUSE;
        }
    }
    if(state == GLUT_DOWN)
    {
        for(auto &glButton : buttons)
        {
            glButton->mouseFunc(button, state, x ,toolHeight-y);
        }
    }


    glutPostWindowRedisplay(toolsWindow);
}

void ReplayGlutContext::init(int argc, char *argv[]) {

    glutInit(&argc, argv);

    // create the window
    glutInitWindowPosition(0, 0);
    glutInitWindowSize(width,height+toolHeight+separ);
    glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE);
    topWindow=glutCreateWindow("VisibleSim Replayer");

    glutDisplayFunc(drawFunc);
    /* bind reshape function */
    glutReshapeFunc(reshapeFunc);
    /* bind drawing function */
    glutDisplayFunc(drawFunc);
    /* bind mouse click function */
    glutMouseFunc(mouseFunc);
    //glutPassiveMotionFunc(passiveMotionFunc);
    /* bind key pressed function */
    glutKeyboardFunc(kbdFunc);
    /* bind special key pressed function */
//  glutSpecialFunc(kbdSpecialFunc);
    /* bind idle function */
    glutIdleFunc(idleFunc);
    /* bind close function */
    glutCloseFunc(quit);

    // sub windows
    toolsWindow = glutCreateSubWindow(topWindow, 0,height+separ,width, toolHeight);
    glutDisplayFunc(drawFuncTW);
    glutReshapeFunc(reshapeFuncTW);
    glutKeyboardFunc(kbdFunc);
    glutMouseFunc(mouseFuncTW);
    glutMotionFunc(motionFuncTW);

    mainWindow = glutCreateSubWindow(topWindow,0,0,width,height);
    glutDisplayFunc(drawFuncMW);
    glutReshapeFunc(reshapeFuncMW);
    glutKeyboardFunc(kbdFunc);
    glutMouseFunc(mouseFuncMW);
    /* bind mouse motion function */
    glutMotionFunc(motionFuncMW); // drag

    initShaders(enableShadows);
    initGL();

    //Buttons initialization

    buttons.push_back(new PlayButton(width*(toolbarOffsetX+offsetX),3*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                     toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                     toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new PauseButton(width*(toolbarOffsetX+offsetX),2*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                      toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                      toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new GoToBeginningButton(width*(toolbarOffsetX+offsetX),0,
                                      toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                      toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new GoToEndButton(width*(toolbarOffsetX+offsetX),5*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                              toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                              toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new StepBackwardButton(width*(toolbarOffsetX+offsetX),1*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                        toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                        toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new StepForwardButton(width*(toolbarOffsetX+offsetX),4*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                        toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                        toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new ZoomInButton(width*(toolbarOffsetX+offsetX),9*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                            toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                            toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new ZoomOutButton(width*(toolbarOffsetX+offsetX),7*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                       toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                       toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new CenterZoomButton(width*(toolbarOffsetX+offsetX),8*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                        toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                        toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new SetStartZoomButton(width*(toolbarOffsetX+offsetX),10*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                           toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                           toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new SetEndZoomButton(width*(toolbarOffsetX+offsetX),11*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                           toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                           toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));
    buttons.push_back(new ShowKeyframesButton(width*(toolbarOffsetX+offsetX),13*(toolsButtonSize*toolHeight+buttonSeparation*width),
                                           toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0,
                                           toolHeight*toolsButtonSize, toolHeight*toolsButtonSize));

}

void ReplayGlutContext::setWorld(ReplayWorld* replayWorld) {
    world = replayWorld;
}

void ReplayGlutContext::mainLoop() {
    world->updateMap();

    glutMainLoop();
}
/*********************************************************/
/* frame drawing function                                */
void ReplayGlutContext::drawFunc(void) {
    //cout << "topdraw" << endl;
    glClearColor(0.8f,0.8f,0.8f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);

    glColor3fv(black);
    glPushMatrix();
    glTranslatef(width-1.5f*separ,0.1f*separ+(toolsWinOpened?toolHeight:0),0.0f);
    glBegin(GL_QUADS);
    glVertex2f(0,0);
    glVertex2f(separ*0.8f,0);
    glVertex2f(separ*0.8f,separ*0.8f);
    glVertex2f(0,separ*0.8f);
    glEnd();
    glPopMatrix();

    glColor3fv(red);
    if (toolsWinOpened) {
        glPushMatrix();
        glTranslatef(width - 1.5f * separ, 0.1f * separ+toolHeight, 0.0f);
        glBegin(GL_TRIANGLES);
        glVertex2f(0.4f * separ, 0.1f * separ);
        glVertex2f(separ * 0.7f, 0.7f * separ);
        glVertex2f(separ * 0.1f, 0.7f * separ);
        glEnd();
        glPopMatrix();
    } else {
        glPushMatrix();
        glTranslatef(width - 1.5f * separ, 0.1f * separ, 0.0f);
        glBegin(GL_TRIANGLES);
        glVertex2f(0.4f * separ, 0.7f * separ);
        glVertex2f(separ * 0.1f, 0.1f * separ);
        glVertex2f(separ * 0.7f, 0.1f * separ);
        glEnd();
        glPopMatrix();
    }

    glEnable(GL_DEPTH_TEST);

    glutSwapBuffers();

}

/*********************************************************/
/* frame drawing function                                */
void ReplayGlutContext::drawFuncMW(void) {

    glClearColor(0.2f,0.2f,0.2f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    noshadowRenderingStart(camera);

    glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,grey);
    glPushMatrix();
        world->glDraw();
    glPopMatrix();

    glPopMatrix();
    noshadowRenderingStop();
    glutSwapBuffers();

}

void ReplayGlutContext::drawString(int ix,int iy,char* str) {
    glRasterPos2i(ix,iy);
    while (*str) {
        glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18, *str);
        str++;
    }
}


/*********************************************************/
/* frame drawing function                                */
void ReplayGlutContext::drawFuncTW(void) {
    //cout << "draw TW" << endl;

    glClearColor(0.5f,0.5f,0.5f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);

    //Drawing timeline

    drawTimeline();

    //Drawing buttons
    for(auto &glButton : buttons)
    {
        glButton->drawFunc();
    }

    glEnable(GL_DEPTH_TEST);
    glutSwapBuffers();

}

void ReplayGlutContext::drawTimeline()
{
    glPushMatrix();
    glTranslatef(offsetX*width,toolHeight*(1-offsetY-timelineHeight),0);
    timelineX = width*(1-2*offsetX);
    glColor3fv(white);
    glBegin(GL_QUADS);
    glVertex2i(0,timelineY);
    glVertex2i(timelineX,timelineY);
    glVertex2i(timelineX,0);
    glVertex2i(0,0);
    glEnd();

    //calcul de la timeline
    float zoomDuration = world->getEndZoom()-world->getStartZoom();

    float power = floor(log10(zoomDuration));
    float firstDigit = zoomDuration/(pow(10,power));
    char str[50];
    float stepHeight;

    if(firstDigit>=5)
    {
        stepDuration = pow(10,power-1)*pow(2,ceil(log2(1024.0f/width)));
    }
    else if (firstDigit>=2)
    {
        stepDuration = 5*pow(10,power-2)*pow(2,ceil(log2(1024.0f/width)));
    }
    else
    {
        stepDuration = 2*pow(10,power-2)*pow(2,ceil(log2(1024.0f/width)));
    }
    int stepCount = zoomDuration/stepDuration;
    glColor3fv(black);
    for(float i=0;i<=stepCount;i++)
    {
        int xPosition = timelineOffset*timelineX+i/stepCount*(timelineX*(1-2*timelineOffset));
        if((int)i%10==0)
        {
            stepHeight = 1.0f;
            sprintf(str,"%2.1f",i*stepDuration+world->getStartZoom()) ;
            drawString(xPosition-18,0.05*timelineY,str);
        }
        else
        {
            stepHeight = 0.6f;
        }
        glBegin(GL_LINES);
        glVertex2i(xPosition,stepHeight*timelineY);
        glVertex2i(xPosition,0.4*timelineY);
        glEnd();
    }
    glBegin(GL_QUADS);
    glColor3fv(blue);
    if(showKeyframesTime)
    {
        for(float time : keyframesTime)
        {
            float xPosition = timelineOffset*timelineX+(time-world->getStartZoom())
                    /zoomDuration*timelineX*(1-2*timelineOffset);
            glVertex2i(xPosition-1,0.3f*timelineY);
            glVertex2i(xPosition+1,0.3f*timelineY);
            glVertex2i(xPosition+1,0.8f*timelineY);
            glVertex2i(xPosition-1,0.8f*timelineY);
//            cout<<"Checkpoint 3"<<endl;
        }
    }
    glColor3fv(black);
    glEnd();
    //cout << "DEBUG : "<<stepDuration<<" SUITE :"<<stepCount<<endl;

    //Curseur
    glTranslatef(timelineOffset*timelineX+timelineX*(1-2*timelineOffset)*
               (world->getCurrentTime()-world->getStartZoom())/zoomDuration,0.65*timelineY,0);
    glBegin(GL_TRIANGLES);
    glVertex2i(-9*3-10, 9);
    glVertex2i(-9*3, 0);
    glVertex2i(-9*3, 18);

    glVertex2i(9*3+10, 9);
    glVertex2i(9*3, 0);
    glVertex2i(9*3, 18);

    glVertex2i(9, 0);
    glVertex2i(-9, 0);
    glVertex2i(0, -10);
    glEnd();
    glBegin(GL_LINES);
    glVertex2i(-9*3, 1);
    glVertex2i(9*3, 1);
    glVertex2i(-9*3, 17);
    glVertex2i(9*3, 17);
    glEnd();

    glColor3fv(white);
    glBegin(GL_QUADS);
    {
        glVertex2i(-9*3+1, 2);
        glVertex2i(9*3-1, 2);
        glVertex2i(9*3-1, 16);
        glVertex2i(-9*3+1, 16);
    }
    glEnd();
    glColor3fv(black);
    sprintf(str,"%2.1f",world->getCurrentTime()) ;
    drawString(-12,2,str);

    glPopMatrix();

}

void ReplayGlutContext::drawNextButtonSquare()
{
    glTranslatef(width*buttonSeparation+toolHeight*toolsButtonSize,0,0);
    glColor3fv(black);
    glBegin(GL_QUADS);
    glVertex2i(0,0);
    glVertex2i(0,toolHeight*toolsButtonSize);
    glVertex2i(toolHeight*toolsButtonSize,toolHeight*toolsButtonSize);
    glVertex2i(toolHeight*toolsButtonSize,0);
    glEnd();

    glColor3fv(white);
    glBegin(GL_QUADS);
    glVertex2i(toolHeight*toolsButtonSize*0.1f,toolHeight*toolsButtonSize*0.1f);
    glVertex2i(toolHeight*toolsButtonSize*0.1f,toolHeight*toolsButtonSize*(1-0.1f));
    glVertex2i(toolHeight*toolsButtonSize*(1-0.1f),toolHeight*toolsButtonSize*(1-0.1f));
    glVertex2i(toolHeight*toolsButtonSize*(1-0.1f),toolHeight*toolsButtonSize*0.1f);
    glEnd();
}

void ReplayGlutContext::play()
{
    replayMode = REPLAY_MODE_PLAY;
}