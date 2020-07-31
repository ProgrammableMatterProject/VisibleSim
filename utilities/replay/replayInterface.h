/*!
 * @file replayInterface.h
 * @brief Contains all buttons definition with their draw,
 * activation and click detection methods
 * @author Mattéo Daluz
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

    /**
     * @brief Called on click, checks if the user clicked on the button
     */
    void mouseFunc(int button,int state,int xMouse,int yMouse);

    /**
     * @brief Adapt button scale with window scale
     */
    void reshapeFunc();

    /**
     * @brief Draw function of the button (to be updated for better looking buttons)
     */
    virtual void drawFunc() {};

    /**
     * @brief Called when clicked on the button
     */
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

class ZoomInButton : public Button
{
public:
    ZoomInButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class ZoomOutButton : public Button
{
public:
    ZoomOutButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class CenterZoomButton : public Button
{
public:
    CenterZoomButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class SetStartZoomButton : public Button
{
public:
    SetStartZoomButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class SetEndZoomButton : public Button
{
public:
    SetEndZoomButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};

class ShowKeyframesButton : public Button
{
public:
    ShowKeyframesButton(float xPosition, float xFixed, float yPosition, float yFixed, float h, float w);
    virtual void drawFunc() override;
    virtual void activate() override;
};