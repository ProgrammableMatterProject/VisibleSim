/*!
 * @file replayInterface.cpp
 * @brief Contains all buttons definition with their draw,
 * activation and click detection methods
 * @author Mattéo Daluz
 */

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
    if(ReplayGlutContext::world->getCurrentTime()==ReplayGlutContext::world->getEndZoom())
    {
        ReplayGlutContext::world->setCurrentTime(ReplayGlutContext::world->getStartZoom());
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
    ReplayGlutContext::world->setCurrentTime(ReplayGlutContext::world->getStartZoom());
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
    ReplayGlutContext::world->setCurrentTime(ReplayGlutContext::world->getEndZoom());
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
    ReplayGlutContext::world->setCurrentTime(
            max(ReplayGlutContext::world->getCurrentTime()-5*ReplayGlutContext::stepDuration,
                ReplayGlutContext::world->getStartZoom()));
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
                    ReplayGlutContext::world->getEndZoom()));

    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
    ReplayGlutContext::world->updateMap();

}


ZoomInButton::ZoomInButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w)
: Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void ZoomInButton::drawFunc() {
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
    glVertex2i(ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);

    glVertex2i(ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize);
    glEnd();



    glPopMatrix();
}

void ZoomInButton::activate() {

    float newStart = ReplayGlutContext::world->getStartZoom()+
            (ReplayGlutContext::world->getEndZoom()-ReplayGlutContext::world->getStartZoom())/4;
    ReplayGlutContext::world->setStartZoom(newStart);
    float newEnd =ReplayGlutContext::world->getEndZoom()-
                  (ReplayGlutContext::world->getEndZoom()-ReplayGlutContext::world->getStartZoom())/4;
    ReplayGlutContext::world->setEndZoom(newEnd);

    if(ReplayGlutContext::world->getCurrentTime()>newEnd)
    {
        ReplayGlutContext::world->setCurrentTime(newEnd);
    }
    if(ReplayGlutContext::world->getCurrentTime() < newStart)
    {
        ReplayGlutContext::world->setCurrentTime(newStart);
    }
    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
}

ZoomOutButton::ZoomOutButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w)
        : Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void ZoomOutButton::drawFunc() {
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
    glVertex2i(ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize);
    glEnd();



    glPopMatrix();
}

void ZoomOutButton::activate() {

    float newStart = ReplayGlutContext::world->getStartZoom()-
                     (ReplayGlutContext::world->getEndZoom()-ReplayGlutContext::world->getStartZoom())/2;

    float newEnd = ReplayGlutContext::world->getEndZoom()+
                   (ReplayGlutContext::world->getEndZoom()-ReplayGlutContext::world->getStartZoom())/2;

    ReplayGlutContext::world->setStartZoom(max(newStart,0.0f));
    ReplayGlutContext::world->setEndZoom(min(newEnd, ReplayGlutContext::world->getExportDuration()));


    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
}

CenterZoomButton::CenterZoomButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w)
        : Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void CenterZoomButton::drawFunc() {
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
    glVertex2i(ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.20f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.30f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.30f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.20f*ReplayGlutContext::toolsButtonSize);

    glVertex2i(ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.55f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.45f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);

    glEnd();



    glPopMatrix();
}

void CenterZoomButton::activate() {

    float newStart = ReplayGlutContext::world->getCurrentTime()-
                     (ReplayGlutContext::world->getEndZoom()-ReplayGlutContext::world->getStartZoom())/2;

    float newEnd = ReplayGlutContext::world->getCurrentTime()+
                   (ReplayGlutContext::world->getEndZoom()-ReplayGlutContext::world->getStartZoom())/2;
    ReplayGlutContext::world->setStartZoom(max(newStart,0.0f));
    ReplayGlutContext::world->setEndZoom(min(newEnd, ReplayGlutContext::world->getExportDuration()));


    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
}

SetStartZoomButton::SetStartZoomButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w)
        : Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void SetStartZoomButton::drawFunc() {
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
    glBegin(GL_QUAD_STRIP);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.4f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.4f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize);

    glEnd();



    glPopMatrix();
}

void SetStartZoomButton::activate() {

    float newStart = ReplayGlutContext::world->getCurrentTime();
    if(newStart != ReplayGlutContext::world->getEndZoom())
    {
        ReplayGlutContext::world->setStartZoom(newStart);
    }

    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
}

SetEndZoomButton::SetEndZoomButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w)
        : Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void SetEndZoomButton::drawFunc() {
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
    glBegin(GL_QUAD_STRIP);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
            ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize);

    glEnd();



    glPopMatrix();
}

void SetEndZoomButton::activate() {

    float newEnd = ReplayGlutContext::world->getCurrentTime();
    if(newEnd != ReplayGlutContext::world->getStartZoom())
    {
        ReplayGlutContext::world->setEndZoom(newEnd);
    }

    ReplayGlutContext::replayMode = REPLAY_MODE_PAUSE;
}

ShowKeyframesButton::ShowKeyframesButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w)
        : Button( xPosition, xFixed,  yPosition,yFixed,  h,  w)
{

}

void ShowKeyframesButton::drawFunc() {
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
    glBegin(GL_QUAD_STRIP);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.8f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.7f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.2f*ReplayGlutContext::toolsButtonSize);
    glVertex2i(ReplayGlutContext::toolHeight*0.6f*ReplayGlutContext::toolsButtonSize,
               ReplayGlutContext::toolHeight*0.3f*ReplayGlutContext::toolsButtonSize);

    glEnd();



    glPopMatrix();
}

void ShowKeyframesButton::activate() {

    if(ReplayGlutContext::showKeyframesTime)
    {
        ReplayGlutContext::showKeyframesTime = false;
        ReplayGlutContext::keyframesTime.clear();
    }
    else
    {
        ReplayGlutContext::showKeyframesTime = true;
        ReplayGlutContext::world->player->parseKeyframeForTimeline();
    }
}