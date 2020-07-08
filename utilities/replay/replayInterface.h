/**
 * @file   ReplayInterface.h
 * @author Matteo Daluz
 * @date   Tue Jun  9 11:13:33 2020
 *
 * @brief  Replay interface containing buttons and windows
 *
 *
 */

#pragma once

#include <fstream>
#include <map>
#include "replayGlutContext.h"
#include "../../simulatorCore/src/replay/replayTags.h"


class Button
{


public:
    bool isActive;
    bool isDown = false;
    bool isHighlighted;
    int x, initX, fixedX;
    int y, initY, fixedY;
    int height, initHeight;
    int width, initWidth;

    Button(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    void mouseFunc(int button,int state,int xMouse,int yMouse);
    void reshapeFunc();
    virtual void drawFunc() {};
    virtual void activate() {};
};

class PlayButton : public Button
{
public:
    PlayButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class PauseButton : public Button
{
public:
    PauseButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class GoToBeginningButton : public Button
{
public:
    GoToBeginningButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class GoToEndButton : public Button
{
public:
    GoToEndButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class StepForwardButton : public Button
{
public:
    StepForwardButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class StepBackwardButton : public Button
{
public:
    StepBackwardButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};