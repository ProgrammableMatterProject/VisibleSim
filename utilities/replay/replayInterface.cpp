#include <iostream>
#include "replayInterface.h"
#include "../../simulatorCore/src/replay/replayTags.h"


using namespace std;
using namespace GlutContext;
using namespace ReplayTags;
Button::Button(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w)
{

    initX = xPosition;
    initY = yPosition;
    initHeight = h;
    initWidth = w;

    fixedX = xFixed;
    fixedY = yFixed;

    x = xPosition+fixedX;
    y = yPosition+fixedY;
    height = h;
    width = w;
}

void Button::mouseFunc(int button,int state,int xMouse,int yMouse)
{

    if(button == GLUT_LEFT_BUTTON)
    {
        if(xMouse>x && xMouse<x+width)
        {
            if(yMouse>y && yMouse<y+height)
            {
                activate();
            }
        }
    }
}
void Button::reshapeFunc()
{
    x = fixedX+initX*(1+(ReplayGlutContext::width-1024.0f)/700);
}
PlayButton::PlayButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w) :
Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void PlayButton::drawFunc()
{
    glPushMatrix();
    glTranslatef(x,y,0);
    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(0,0);
    glVertex2i(0,ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,0);
    glEnd();

    if(ReplayGlutContext::replayMode == REPLAY_MODE_PLAY)
    {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glEnd();
    }
    else {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * 0.1f,
                   ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * 0.1f);
        glVertex2i(ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * 0.1f,
                   ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * (1 - 0.1f));
        glVertex2i(ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * (1 - 0.1f),
                   ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * (1 - 0.1f));
        glVertex2i(ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * (1 - 0.1f),
                   ReplayGlutContext::toolHeight * ReplayGlutContext::toolsButtonSize * 0.1f);
        glEnd();
    }
    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_POLYGON);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();

    glPopMatrix();
}

void PlayButton::activate() {
    ReplayGlutContext::replayMode = REPLAY_MODE_PLAY;
    if(ReplayGlutContext::world->getCurrentTime()==ReplayGlutContext::world->getExportDuration())
    {
        ReplayGlutContext::world->setCurrentTime(0.0f);
    }
}


PauseButton::PauseButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w) :
        Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void PauseButton::drawFunc()
{
    glPushMatrix();
    glTranslatef(x,y,0);
    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(0,0);
    glVertex2i(0,ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,0);
    glEnd();

    if(ReplayGlutContext::replayMode == REPLAY_MODE_PAUSE)
    {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glEnd();
    }
    else
    {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
        glEnd();
    }


    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(ReplayGlutContext::toolHeight*0.35f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.35f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);

    glVertex2i(ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.65f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.65f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();

    glPopMatrix();
}

void PauseButton::activate() {
    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
}

GoToBeginningButton::GoToBeginningButton(float xPosition, float xFixed, float yPosition,
        float yFixed, float h, float w) :
        Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void GoToBeginningButton::drawFunc() {
    glPushMatrix();
    glTranslatef(x,y,0);
    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(0,0);
    glVertex2i(0,ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,0);
    glEnd();

    if(ReplayGlutContext::world->getCurrentTime() == 0.0f)
    {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glEnd();
    }
    else
    {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
        glEnd();
    }

    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.4f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.4f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();

    glBegin(GL_TRIANGLES);
    glVertex2i(ReplayGlutContext::toolHeight*0.4f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();
    glPopMatrix();
}

void GoToBeginningButton::activate() {
    ReplayGlutContext::world->setCurrentTime(0.0f);
    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
    ReplayGlutContext::world->updateMap();

}


GoToEndButton::GoToEndButton(float xPosition, float xFixed, float yPosition, float yFixed,
        float h, float w) :
        Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void GoToEndButton::drawFunc() {
    glPushMatrix();
    glTranslatef(x,y,0);
    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(0,0);
    glVertex2i(0,ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,0);
    glEnd();

    if(ReplayGlutContext::world->getCurrentTime() == ReplayGlutContext::world->getExportDuration())
    {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.15f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.15f);
        glEnd();
    }
    else
    {
        glColor3fv(ReplayGlutContext::white);
        glBegin(GL_QUADS);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
        glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
                   ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
        glEnd();
    }

    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();
    glBegin(GL_TRIANGLES);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();

    glPopMatrix();
}

void GoToEndButton::activate() {
    ReplayGlutContext::world->setCurrentTime(ReplayGlutContext::world->getExportDuration());
    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
    ReplayGlutContext::world->updateMap();

}

StepBackwardButton::StepBackwardButton(float xPosition, float xFixed, float yPosition, float yFixed,
        float h, float w) :
        Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void StepBackwardButton::drawFunc() {
    glPushMatrix();
    glTranslatef(x,y,0);
    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(0,0);
    glVertex2i(0,ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,0);
    glEnd();

    glColor3fv(ReplayGlutContext::white);
    glBegin(GL_QUADS);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
    glEnd();


    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();
    glBegin(GL_TRIANGLES);
    glVertex2i(ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();


    glPopMatrix();
}

void StepBackwardButton::activate() {
    ReplayGlutContext::world->setCurrentTime(max(ReplayGlutContext::world->getCurrentTime()-5*ReplayGlutContext::stepDuration,0.0f));
    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
    ReplayGlutContext::world->updateMap();

}

StepForwardButton::StepForwardButton(float xPosition, float xFixed, float yPosition, float yFixed,
        float h, float w) :
        Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void StepForwardButton::drawFunc() {
    glPushMatrix();
    glTranslatef(x,y,0);
    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(0,0);
    glVertex2i(0,ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize,0);
    glEnd();

    glColor3fv(ReplayGlutContext::white);
    glBegin(GL_QUADS);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f,
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f));
    glVertex2i(ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*(1-0.1f),
               ReplayGlutContext::toolHeight*ReplayGlutContext::toolsButtonSize*0.1f);
    glEnd();

    glColor3fv(ReplayGlutContext::black);
    glBegin(GL_QUADS);
    glVertex2i(ReplayGlutContext::toolHeight*0.4f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.4f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();
    glBegin(GL_TRIANGLES);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.5f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glEnd();


    glPopMatrix();
}

void StepForwardButton::activate() {
    ReplayGlutContext::world->setCurrentTime(
            min(ReplayGlutContext::world->getCurrentTime()+5*ReplayGlutContext::stepDuration,
                    ReplayGlutContext::world->getExportDuration()));

    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
    ReplayGlutContext::world->updateMap();

}