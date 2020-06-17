/*********************************************************/
/* Benoit Piranda          Université de Franche-Comté   */
/* M1 IOT Copyright 2019                                 */
/*********************************************************/
#ifdef _WIN32
#include <windows.h>
#endif
#include <iostream>
#include <string>
#include <GL/freeglut.h>
#include <cmath>

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
static void idleFunc();
static void quit();

/*********************************************************/
/* global variables                                      */
static GLfloat red[4] = { 1.0f, 0.0f, 0.0f, 1.0f}; // red color material
static GLfloat green[4] = { 0.0f, 1.0f, 0.0f, 1.0f}; // green color material
static GLfloat blue[4] = { 0.0f, 0.0f, 1.0f, 1.0f}; // blue color material
static GLfloat lightgrey[4] = { 0.8f, 0.8f, 0.8f, 1.0f}; // lightgrey color material
static GLfloat grey[4] = { 0.4f, 0.4f, 0.4f, 1.0f}; // grey color material
static GLfloat black[4] = { 0.0f, 0.0f, 0.0f, 1.0f}; // black color material

int width=1024,height=600,toolHeight=100,separ=32; // initial size of the screen
const float cameraPos[] = {5.0f,2.0f,8.0f};
float cameraTheta=0.0f, cameraPhi=0.0f, cameraDist=5.0f; // spherical coordinates of the point of view
float rotationAngle=0; // rotation angle of the teapot /z
int mouseX0,mouseY0;
GLint topWindow;
GLint mainWindow;
GLint toolsWindow;
bool toolsWinOpened=true;

int main(int argc, char** argv) {
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
    initGL();

//	glutFullScreen();
//  glutSetCursor(GLUT_CURSOR_NONE); // allow to hide cursor

    glutMainLoop();

    return 0;
}

/*********************************************************/
/* frame drawing function                                */
static void drawFunc(void) {
    glClearColor(0.8f,0.8f,0.8f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glDisable(GL_LIGHTING);
    glDisable(GLUT_DEPTH);
    glColor3fv(black);

    glBegin(GL_LINES);
    glVertex2i(0, 1);
    glVertex2i(width,1);
    glVertex2i(0,separ);
    glVertex2i(width,separ);
    glEnd();

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

    glEnable(GL_DEPTH);

    glutSwapBuffers();
}

/*********************************************************/
/* frame drawing function                                */
static void drawFuncMW(void) {
    glClearColor(0.2f,0.2f,0.2f,1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    glPushMatrix();
    double x = cameraDist*cos(cameraTheta)*cos(cameraPhi);
    double y  = cameraDist*sin(cameraTheta)*cos(cameraPhi);
    double z = cameraDist*sin(cameraPhi);
    gluLookAt(x,y,z,0,0,0,0,0,1.0);

    GLfloat pos[4] = { 0.0f, 0.0f, 5.0f, 1.0f}; // position
    glLightfv(GL_LIGHT0, GL_POSITION, pos );

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

    glPushMatrix();
    glRotatef(90.0,1.0f,0.0f,0.0f);
    glutSolidTeapot(1.0f);
    //glutSolidCone(0.5,2.0,10,1);
    glPopMatrix();

    glPopMatrix();

    glPopMatrix();
    glutSwapBuffers();
}

void drawString(int ix,int iy,char* str) {
    glRasterPos2i(ix,iy);
    while (*str) {
        glutBitmapCharacter(GLUT_BITMAP_TIMES_ROMAN_24, *str);
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
    glDisable(GL_DEPTH);
    glColor3fv(blue);

/*    sprintf(str,"angle = %6.1f",rotationAngle);
    drawString(40,20,str);
*/
    glBegin(GL_LINES);
    glVertex2i(0,0);
    glVertex2i(width,toolHeight);
    glVertex2i(0,toolHeight);
    glVertex2i(width,0);
    glEnd();

    glEnable(GL_DEPTH);
    glutSwapBuffers();
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

    // place and size mainWindow
    glutSetWindow(mainWindow);
    glutPositionWindow(0,0);
    glutReshapeWindow(width, height);

    if (toolsWinOpened) {
        // place and size toolsWindow
        glutSetWindow(toolsWindow);
        glutPositionWindow(0, height + separ);
        glutReshapeWindow(width, toolHeight);
    }
}

static void reshapeFuncMW(int w,int h) {
    cout << "reshapeMW:" << w << "," << h << endl;
    glViewport(0,0,w,h);
    // initialize Projection matrix
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    gluPerspective(30.0,(double)width/(double)height,0.1,100.0);
    // initialize ModelView matrix
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

    rotationAngle += dt*20.0f; // turn at 20� / s
//cout << "rotationAngle=" << rotationAngle << endl;
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
        case '+' :
            cameraDist+=0.10f;
            break;
        case '-' :
            cameraDist-=0.10f;
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
    mouseX0=x;
    mouseY0=y;
}

bool isIn(int x, int y, int x0, int y0, int w, int h) {
    return (x>x0 && y>y0 && x<x0+w && y<y0+h);
}

static void mouseFunc(int button,int state,int x,int y) {
    int hy=toolsWinOpened?height+separ-y:height+separ-y;
    cout << x << ',' << y << "/" << hy << ":" << isIn(x,hy,width-1.5f*separ,toolHeight+0.1f*separ,0.8*separ,0.8*separ) << endl;
    if (state==GLUT_UP && isIn(x,hy,width-1.5f*separ,0.1f*separ,0.8f*separ,0.8f*separ)) {
        if (toolsWinOpened) {
            toolsWinOpened=false;
            glutSetWindow(toolsWindow);
            glutHideWindow();
            glutSetWindow(topWindow);
            reshapeFunc(width,height+toolHeight+separ);
        } else {
            toolsWinOpened=true;
            glutSetWindow(toolsWindow);
            glutShowWindow();
            glutSetWindow(topWindow);
            reshapeFunc(width,height+separ);
        }
        glutPostWindowRedisplay(topWindow);
    }
    mouseX0=x;
    mouseY0=y;
}

/*********************************************************/
/* Mouse move function (with button pressed)             */
/* x,y: mouse coordinates                                */
static void motionFuncMW(int x,int y) {
    cameraTheta += (x-mouseX0)*0.01f;
    cameraPhi += (y-mouseY0)*0.01f;

    mouseX0=x;
    mouseY0=y;
    glutPostWindowRedisplay(mainWindow);
}

/*********************************************************/
/* Initialisation of the OPENGL window and parameters    */
static void initGL() {
    glEnable(GL_DEPTH_TEST);
    glShadeModel(GL_SMOOTH);
    glEnable(GL_NORMALIZE); // auto-normalize normal vectors

    glClearColor(0.0,0.0,0.0,0.0); // background color

    glEnable(GL_LIGHTING);
    /* LIGHT0 light source parameters */
    GLfloat pos[4] = { 0.0f, 0.0f, 5.0f, 1.0f}; // position
    GLfloat light_diffuse[4] = {1.0f, 1.0f, 1.0f, 1.0f}; // colors
    GLfloat light_ambient[4] = {0.2f, 0.2f, 0.2f, 1.0f};

    glEnable(GL_LIGHT0);
    glLightfv(GL_LIGHT0, GL_POSITION, pos );
    glLightfv(GL_LIGHT0, GL_DIFFUSE, light_diffuse);
    glLightfv(GL_LIGHT0, GL_AMBIENT, light_ambient);
    glLightfv(GL_LIGHT0, GL_SPECULAR, light_diffuse);
}

static void quit() {

}
