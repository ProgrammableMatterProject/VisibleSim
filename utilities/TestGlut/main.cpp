/*********************************************************/
/* Benoit Piranda          Université de Franche-Comté   */
/* M1 IOT Copyright 2019                                 */
/*********************************************************/
#ifdef _WIN32
#include <windows.h>
#endif
#include <iostream>
#include <string>
#include <cmath>
#include "../../simulatorCore/src/gui/objLoader.h"
#include "../../simulatorCore/src/gui/camera.h"
using namespace std;

/*********************************************************/
/* prototypes                                            */
static void initGL();
static void reshapeFunc(int,int);
static void reshapeFuncMW(int,int);
static void reshapeFuncTW(int,int);
static void drawFunc();
static void drawFuncMW();
static void drawFuncTW();
static void kbdFunc(unsigned char,int,int);
static void mouseFunc(int button,int state,int x,int y);
static void mouseFuncMW(int button,int state,int x,int y);
static void motionFuncMW(int x,int y);
static void motionFuncTW(int x,int y);
static void idleFunc();
static void quit();
static void updateSubWindows();
static void drawNextButtonSquare();
static void drawTimeline();
static void drawCursor();
static void mouseFuncTW(int button,int state,int x,int y);
/*********************************************************/
/* global variables                                      */
static GLfloat red[4] = { 1.0f, 0.0f, 0.0f, 1.0f}; // red color material
static GLfloat white[4] = { 1.0f, 1.0f, 1.0f, 1.0f}; // White color material
static GLfloat green[4] = { 0.0f, 1.0f, 0.0f, 1.0f}; // green color material
static GLfloat blue[4] = { 0.0f, 0.0f, 1.0f, 1.0f}; // blue color material
static GLfloat lightgrey[4] = { 0.8f, 0.8f, 0.8f, 1.0f}; // lightgrey color material
static GLfloat grey[4] = { 0.4f, 0.4f, 0.4f, 1.0f}; // grey color material
static GLfloat black[4] = { 0.0f, 0.0f, 0.0f, 1.0f}; // black color material

int width=1024,height=600,toolHeight=120,separ=24; // initial size of the screen
int keyboardModifier=0;
//float cameraTheta=0.0f, cameraPhi=0.0f, cameraDist=50.0f; // spherical coordinates of the point of view
Camera camera(30,30,50.0f);
float rotationAngle=0; // rotation angle of the teapot /z
GLint topWindow;
GLint mainWindow;
GLint toolsWindow;
bool toolsWinOpened=true;

//ObjLoader::ObjLoader objectLoader = new ObjLoader();
//BlinkyBlocks::BlinkyBlocksGlBlock block = new BlinkyBlocks::BlinkyBlocksGlBlock();


//Cottes pour la fenetre de commandes en fonction des dimensions
float offsetX = 0.01f, offsetY = 0.1f;
float timelineHeight = 0.5f, timelineOffset = 0.05f;
float toolsSeparationY = 0.05f;
float toolsButtonSize = 0.3f;
float toolbarOffsetX = 0.30f, buttonSeparation = 0.005f;
float recButtonOffset = 0.1f;

float timelineX = width*(1-2*offsetX), timelineY = timelineHeight*toolHeight;
ObjLoader::ObjLoader *objBlock = nullptr;

//Variables de test
//durée en secondes
float duree = 3.749f;
float currentTime = 0.0f;

bool enableShadows=false; // BPi todo for true

int main(int argc, char** argv) {
    glutInit(&argc, argv);

    // create the window
    glutInitWindowPosition(0, 0);
    glutInitWindowSize(width,height+toolHeight+separ);
    glutInitDisplayMode(GLUT_RGB | GLUT_DEPTH | GLUT_DOUBLE);
    topWindow=glutCreateWindow("VisibleSim Replayer");

    initShaders(enableShadows);

    std::string versionString = std::string((const char*)glGetString(GL_VERSION));
    cout << "Opengl Version: " << versionString << endl;

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
    mainWindow = glutCreateSubWindow(topWindow,0,0,width,height);
    glutDisplayFunc(drawFuncMW);
    glutReshapeFunc(reshapeFuncMW);
    glutKeyboardFunc(kbdFunc);
    glutMouseFunc(mouseFuncMW);
    /* bind mouse motion function */
    glutMotionFunc(motionFuncMW); // drag
    //glutPassiveMotionFunc(passiveMotionFunc);

    initGL();


    toolsWindow = glutCreateSubWindow(topWindow, 0,height+separ,width, toolHeight);
    glutDisplayFunc(drawFuncTW);
    glutReshapeFunc(reshapeFuncTW);
    glutKeyboardFunc(kbdFunc);
    glutMouseFunc(mouseFuncTW);
    glutMotionFunc(motionFuncTW);
    //initGL();

//	glutFullScreen();
//  glutSetCursor(GLUT_CURSOR_NONE); // allow to hide cursor

    glutMainLoop();
    return 0;
}

/*********************************************************/
/* frame drawing function                                */
static void drawFunc(void) {
    cout << "topdraw" << endl;
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
static void drawFuncMW(void) {
    glClearColor(0.2f,0.2f,0.2f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    noshadowRenderingStart(&camera);
    glPushMatrix();

    glPushMatrix();
        glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,blue);
        glutSolidCylinder(0.02,2.0,20,5);
        glPushMatrix();
            glRotatef(-90.0,1,0,0);
            glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,green);
            glutSolidCylinder(0.02,2.0,20,5);
        glPopMatrix();
        glRotatef(90.0,0,1,0);
        glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,red);
        glutSolidCylinder(0.02,2.0,20,5);
    glPopMatrix();

    glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,grey);
    glPushMatrix();
        glRotatef(rotationAngle,0.0f,0.0f,1.0f);
        glScalef(0.1f,0.1f,0.1f);
        glPushMatrix();
            objBlock->glDraw();
            glTranslatef(25.0f,0,0);
            objBlock->glDraw();
        glPopMatrix();
    glPopMatrix();

    glPopMatrix();
    noshadowRenderingStop();
    glutSwapBuffers();
}

void drawString(int ix,int iy,char* str) {
    glRasterPos2i(ix,iy);
    while (*str) {
        glutBitmapCharacter(GLUT_BITMAP_HELVETICA_18, *str);
        str++;
    }
}

/*********************************************************/
/* frame drawing function                                */
static void drawFuncTW(void) {
    cout << "draw TW" << endl;
    char str[50];


    glClearColor(0.5f,0.5f,0.5f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT);
    glDisable(GL_LIGHTING);
    glDisable(GL_DEPTH_TEST);

    //Drawing timeline
    drawTimeline();

    //Drawing buttons
    //TODO adater à la fenetre mieux
    glPushMatrix();
    glTranslatef(width*offsetX,toolHeight*(1-offsetY-timelineHeight-toolsSeparationY-toolsButtonSize),0);
    glTranslatef(width*toolbarOffsetX-toolHeight*toolsButtonSize,0,0);

    //Return to beginning button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_QUADS);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.4f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.4f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();
    glBegin(GL_TRIANGLES);
    glVertex2i(toolHeight*0.4f*toolsButtonSize,toolHeight*0.5f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();

    //Go Back button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_QUADS);
    glVertex2i(toolHeight*0.6f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.6f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();
    glBegin(GL_TRIANGLES);
    glVertex2i(toolHeight*0.2f*toolsButtonSize,toolHeight*0.5f*toolsButtonSize);
    glVertex2i(toolHeight*0.5f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.5f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();

    //Pause button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_QUADS);
    glVertex2i(toolHeight*0.35f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.35f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.45f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.45f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);

    glVertex2i(toolHeight*0.55f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.55f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.65f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.65f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();

    //Play button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_POLYGON);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.5f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.8f*toolsButtonSize,toolHeight*0.5f*toolsButtonSize);
    glVertex2i(toolHeight*0.5f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();

    //Go fourth button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_QUADS);
    glVertex2i(toolHeight*0.4f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.4f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();
    glBegin(GL_TRIANGLES);
    glVertex2i(toolHeight*0.8f*toolsButtonSize,toolHeight*0.5f*toolsButtonSize);
    glVertex2i(toolHeight*0.5f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.5f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();

    //Go to the end button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_QUADS);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.6f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.6f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();
    glBegin(GL_TRIANGLES);
    glVertex2i(toolHeight*0.6f*toolsButtonSize,toolHeight*0.5f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glEnd();

    //TODO adater à la fenetre mieux
    glTranslatef(width*recButtonOffset,0,0);
    //Start sequence button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_QUAD_STRIP);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.7f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.4f*toolsButtonSize,toolHeight*0.7f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.4f*toolsButtonSize,toolHeight*0.3f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.3f*toolsButtonSize);
    glEnd();

    //rec button
    drawNextButtonSquare();
    glBegin(GL_TRIANGLE_STRIP);

    glColor3fv(red);
    for(int i=0;i<=360;i++)
    {
        glVertex2i(toolHeight*0.5f*toolsButtonSize+toolHeight*0.35f*toolsButtonSize*cos(i*6.28/360),
                   toolHeight*0.5f*toolsButtonSize+toolHeight*0.35f*toolsButtonSize*sin(i*6.28/360));
        glVertex2i(toolHeight*0.5f*toolsButtonSize+toolHeight*0.35f*toolsButtonSize*cos((i+1)*6.28/360),
                   toolHeight*0.5f*toolsButtonSize+toolHeight*0.35f*toolsButtonSize*sin((i+1)*6.28/360));
        glVertex2i(toolHeight*0.5f*toolsButtonSize,toolHeight*0.5f*toolsButtonSize);
    }
    glEnd();

    //End sequence button
    drawNextButtonSquare();
    glColor3fv(black);
    glBegin(GL_QUAD_STRIP);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.7f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.8f*toolsButtonSize);
    glVertex2i(toolHeight*0.6f*toolsButtonSize,toolHeight*0.7f*toolsButtonSize);
    glVertex2i(toolHeight*0.7f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.6f*toolsButtonSize,toolHeight*0.3f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.2f*toolsButtonSize);
    glVertex2i(toolHeight*0.3f*toolsButtonSize,toolHeight*0.3f*toolsButtonSize);
    glEnd();
    glPopMatrix();


//    sprintf(str,"angle = %6.1f",rotationAngle);
//    drawString(40,20,str);

    glEnable(GL_DEPTH_TEST);
    glutSwapBuffers();
}

void drawTimeline()
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

    float power = floor(log10(duree));
    float firstDigit = duree/(pow(10,power));
    float stepDuration;
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
    int stepCount = duree/stepDuration;
    glColor3fv(black);

    for(float i=0;i<=stepCount;i++)
    {
        int xPosition = timelineOffset*timelineX+i/stepCount*(timelineX*(1-2*timelineOffset));
        if((int)i%10==0)
        {
            stepHeight = 1.0f;
            sprintf(str,"%2.1f",i*stepDuration) ;
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


    cout << "DEBUG : "<<stepDuration<<" SUITE :"<<stepCount<<endl;

    //drawCursor();
    glTranslatef(timelineOffset*timelineX+timelineX*(1-2*timelineOffset)*currentTime/duree,0.65*timelineY,0);
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
    sprintf(str,"%2.1f",currentTime) ;
    drawString(-12,2,str);
    glPopMatrix();
}

void drawCursor()
{

}
void drawNextButtonSquare()
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
/*********************************************************/
/* Window size update function                           */
/* width: width of the drawing area                      */
/* height: width of the drawing area                     */
static void reshapeFunc(int w,int h) {
    cout << "reshapeFunc:" << w << "," << h << endl;
    width=w;
    height = toolsWinOpened? h-toolHeight-separ:h-separ;

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

void updateSubWindows() {

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
    glutSetWindow(topWindow);
}

static void reshapeFuncMW(int w,int h) {
    cout << "reshapeMW:" << w << "," << h << endl;
    glViewport(0,0,w,h);

    camera.setW_H(double(w)/double(h));
    // size of the OpenGL drawing area
    glViewport(0,0,w,h);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    camera.glProjection();
    // camera intrinsic parameters
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

static void reshapeFuncTW(int w,int h) {
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
static void idleFunc(void) {
    static int initTime = glutGet(GLUT_ELAPSED_TIME); // ms
    int currentTime = glutGet(GLUT_ELAPSED_TIME);

    float dt= static_cast<float>(currentTime-initTime)/1000.0f;

//	sprintf_s(chaineFPS,"FR : %lf",1.0/dt);
    initTime = currentTime;

    rotationAngle += dt*20.0f; // turn at 20° / s
// cout << "rotationAngle=" << rotationAngle << endl;
    glutPostWindowRedisplay(mainWindow);
}

/*********************************************************/
/* Key pressed function                                  */
/* c: key pressed character                              */
/* x,y: mouse coordinates                                */
static void kbdFunc(unsigned char c, int x, int y) {
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
    }
    glutPostWindowRedisplay(mainWindow);
}

/*********************************************************/
/* Mouse clicked function                                */
/* button: sum of pressed buttons id                     */
/* state: action                                         */
/* x,y: mouse coordinates                                */
static void mouseFuncMW(int button,int state,int x,int y) {
    keyboardModifier = glutGetModifiers();
    if (keyboardModifier != GLUT_ACTIVE_CTRL) { // rotation du point de vue
        switch (button) {
            case GLUT_LEFT_BUTTON:
                if (state == GLUT_DOWN) {
                    camera.mouseDown(x, y);
                } else if (state == GLUT_UP) {
                    camera.mouseUp(x, y);
                }
                break;
            case GLUT_RIGHT_BUTTON:
                if (state == GLUT_DOWN) {
                    camera.mouseDown(x, y, true);
                } else if (state == GLUT_UP) {
                    camera.mouseUp(x, y);
                }
                break;
            case 3 :
                camera.mouseZoom(-10);
                break;
            case 4 :
                camera.mouseZoom(10);
                break;
        }
    }
}

bool isIn(int x, int y, int x0, int y0, int w, int h) {
    return (x>x0 && y>y0 && x<x0+w && y<y0+h);
}

static void mouseFunc(int button,int state,int x,int y) {
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
static void motionFuncMW(int x,int y) {
    if (keyboardModifier!=GLUT_ACTIVE_CTRL) { // rotation du point de vue
        camera.mouseMove(x,y);
        glutPostRedisplay();
    }
    glutPostWindowRedisplay(mainWindow);
}
static void motionFuncTW(int x,int y)
{
    if(x>=offsetX*width && x<= width*(1-offsetX))
    {
        if(y>=offsetY*toolHeight && y<= toolHeight*(timelineHeight+offsetY))
        {
            if(x<offsetX*width + timelineX*timelineOffset)
            {
                currentTime = 0;
            }
            else if(x>width*(1-offsetX)-timelineX*timelineOffset)
            {
                currentTime = duree;
            }
            else
            {
                currentTime = (x - timelineOffset*timelineX-offsetX*width)*duree/
                              (width*(1-2*offsetX)-2*timelineX*timelineOffset);
            }

        }
    }
    glutPostWindowRedisplay(toolsWindow);
}
static void mouseFuncTW(int button,int state,int x,int y) {

    if(x>=offsetX*width && x<= width*(1-offsetX))
    {
        if(y>=offsetY*toolHeight && y<= toolHeight*(timelineHeight+offsetY))
        {

            if(x<offsetX*width + timelineX*timelineOffset)
            {
                currentTime = 0;
            }
            else if(x>width*(1-offsetX)-timelineX*timelineOffset)
            {
                currentTime = duree;
            }
            else
            {
                currentTime = (x - timelineOffset*timelineX-offsetX*width)*duree/
                              (width*(1-2*offsetX)-2*timelineX*timelineOffset);
            }
        }
    }
    glutPostWindowRedisplay(toolsWindow);
}

/*********************************************************/
/* Initialisation of the OPENGL window and parameters    */
static void initGL() {
    initShaders(enableShadows);

    camera.setDirection(0,30);
    camera.setNearFar(1.0,1000.0);
    camera.setTarget(Vector3D(0,0,0));
    camera.setAngle(10.0);
    camera.setLightParameters(Vector3D(0.0,0.0,0.0),80.0,50.0,50.0,50.0,1.0,1000.0);
    std::string versionString = std::string((const char*)glGetString(GL_VERSION));
    cout << "Opengl Version: " << versionString << endl;

    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/smartBlocksTextures","smartBlockSimple.obj");
}

static void quit() {

}
