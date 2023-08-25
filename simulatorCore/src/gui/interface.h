/*
 * Interface.h
 *
 *  Created on: 27 févr. 2012
 *      Author: Ben
 */

#ifndef INTERFACE_H_
#define INTERFACE_H_

#include <vector>
#include <iostream>
#include <fstream>
#include <memory.h>
#include <map>
#include <vector>

#include "../base/glBlock.h"
#include "../utils/color.h"
#include "math/cell3DPosition.h"

#ifndef GLUT
#define GLUT
#endif

// Forward declare the BaseSimulator namespace
namespace BaseSimulator {}

using namespace std;
using namespace BaseSimulator;

enum class TextMode {
    TEXTMODE_STANDARD, TEXTMODE_TITLE, TEXTMODE_BOLD, TEXTMODE_ID, TEXTMODE_POPUP
};
enum class TextSize {
    TEXTSIZE_STANDARD, TEXTSIZE_LARGE
};

class GlutWindow {

protected :
    vector<GlutWindow *> children;
    GLuint idTexture;

    void addChild(GlutWindow *child);

    void bindTexture();

    TextSize currentTextSize = TextSize::TEXTSIZE_STANDARD;
public :
    GLuint id;
    GLint x, y, w, h;
    bool isVisible;

    GlutWindow(GlutWindow *parent, GLuint pid, GLint px, GLint py, GLint pw, GLint ph, const string &titreTexture);

    virtual ~GlutWindow();

    virtual void clearChildren();

    inline void setGeometry(GLint px, GLint py, GLint pw, GLint ph) {
        x = px;
        y = py;
        w = pw;
        h = ph;
    };

    inline GlutWindow *getChild(int n) { return children[n]; };

    virtual void glDraw();

    virtual int mouseFunc(int button, int state, int mx, int my);

    virtual bool passiveMotionFunc(int mx, int my);

    virtual int keyFunc(int charcode);

    virtual void reshapeFunc(int wx, int wy, int mw, int mh) {};

    static GLfloat
    drawString(GLfloat x, GLfloat y, const char *str, void *mode = GLUT_BITMAP_8_BY_13, GLint height = 13);

    GLfloat drawString(GLfloat x, GLfloat y, const char *str, TextMode mode);

    virtual void setTextSize(TextSize ts);

    TextSize getTextSize() { return currentTextSize; }

    void show(bool v) { isVisible = v; };

    void showHide() { isVisible = !isVisible; };

    inline bool isShown() { return isVisible; };
};


class GlutButton : public GlutWindow {
    bool isActive;
    bool isDown;
    bool isHighlighted;
public :
    GlutButton(GlutWindow *parent, GLuint pid, GLint px, GLint py, GLint pw, GLint ph, const string &titreTexture,
               bool pia = true);

    virtual ~GlutButton() {};

    void addSubMenu(GlutWindow *child);

    inline void activate(bool v) { isActive = v; };

    inline bool isActivated() { return isActive; };

    int mouseFunc(int button, int state, int x, int y) override;

    bool passiveMotionFunc(int mx, int my) override;

    void glDraw() override;
};

class GlutRotationButton : public GlutWindow {
    bool isHighlighted;
    bool isBlue;
    uint8_t id0, id1;
    float characterWidth;
public :
    Cell3DPosition finalPosition;
    short finalOrientation;

    GlutRotationButton(GlutWindow *parent, GLuint pid, GLint px, GLint py, GLint pw, GLint ph,
                       const string &titreTexture, bool blue, uint8_t idSrc, uint8_t idDest, Cell3DPosition &pos,
                       short orientation, float cw = 0.0625);

    virtual ~GlutRotationButton() {};

    int mouseFunc(int button, int state, int x, int y) override;

    bool passiveMotionFunc(int mx, int my) override;

    void glDraw() override;
};

class GlutRBMotionButton : public GlutWindow {
    bool isHighlighted;
    bool isBlue;
    uint8_t directionID, actionID;
    float characterWidth;
public :
    Cell3DPosition finalPosition;
    short finalOrientation;

    GlutRBMotionButton(GlutWindow *parent, GLuint pid, GLint px, GLint py, GLint pw, GLint ph,
                       const string &titreTexture, bool isRotation, uint8_t idDest, Cell3DPosition &pos, short orient,
                       float cw = 0.125);

    virtual ~GlutRBMotionButton() {};

    int mouseFunc(int button, int state, int x, int y) override;

    bool passiveMotionFunc(int mx, int my) override;

    void glDraw() override;
};

class GlutRotation2DButton : public GlutWindow {
    bool isHighlighted;
    bool isBlue;
    uint8_t id0, id1;
    float characterWidth;
public :
    Cell3DPosition finalPosition;
    short finalOrientation;

    GlutRotation2DButton(GlutWindow *parent, GLuint pid, GLint px, GLint py, GLint pw, GLint ph,
                         const string &titreTexture, bool blue, uint8_t idSrc, uint8_t idDest, Cell3DPosition &pos,
                         short orientation, float cw = 0.0625);

    virtual ~GlutRotation2DButton() {};

    int mouseFunc(int button, int state, int x, int y) override;

    bool passiveMotionFunc(int mx, int my) override;

    void glDraw() override;
};

//
class BlockDebugData {
public :
    bID blockId;
    string str;
    Color color;

    BlockDebugData(bID id, const string &s, const Color &c) : blockId(id), str(s), color(c) {};
};

class GlutSlider : public GlutWindow {
    int buttonHeight, buttonY;
    int dataTextLines, nbreTextLines, dataPosition;
    bool mouseDown;
    int currentMousePos;
public :
    GlutSlider(GlutWindow *parent, GLuint pid, GLint px, GLint py, GLint ph, const string &titreTexture);

    virtual ~GlutSlider();

    virtual void setTextSize(TextSize ts) override {
        GlutWindow::setTextSize(ts);
        update();
    };

    void setDataTextLines(int dtl) {
        dataTextLines = dtl;
        dataPosition = 0;
        update();
    };

    void incDataTextLines() {
        dataTextLines++;
        update();
    };

    int getPosition() { return dataPosition; }

    void setPosition(int pos) { dataPosition = pos; }

    void glDraw() override;

    int mouseFunc(int button, int state, int x, int y) override;

    void update();
};

class GlutInputWindow : public GlutWindow {
    string text;
    // GlutWindow *server;      // unused
public :
    bool hasFocus;

    GlutInputWindow(GlutWindow *parent, GLuint pid, GLint px, GLint py, GLint pw, GLint ph);

    virtual ~GlutInputWindow();

    int keyFunc(int keycode) override;

    int mouseFunc(int button, int state, int x, int y) override;

    void glDraw() override;

    string getTextAndClear();
};

class GlutSlidingMainWindow : public GlutWindow {
    int openningLevel;
    GlutButton *buttonOpen, *buttonClose, *buttonSize;
    multimap<Time, BlockDebugData *> traces;
    GlutSlider *slider;
    GlBlock *selectedGlBlock;
public :
    GlutSlidingMainWindow(GLint px, GLint py, GLint pw, GLint ph, const string &titreTexture);

    virtual ~GlutSlidingMainWindow();

    void openClose();

    int mouseFunc(int button, int state, int mx, int my) override;

    void reshapeFunc(int wx, int wy, int mw, int mh) override;

    void glDraw() override;

    void addTrace(bID id, const string &str, const Color &color);

    void select(GlBlock *sb);

    bool passiveMotionFunc(int mx, int my) override;

    inline bool hasselectedGlBlock() { return selectedGlBlock != NULL; };

    inline bool isOpened() { return openningLevel != 0; }

    virtual void setTextSize(TextSize ts) override;

private :
    void updateSliderWindow();

    void setOpenCloseButtonPosition(bool openning);
};

class GlutPopupWindow : public GlutWindow {
    string info;
public :
    GlutPopupWindow(GlutWindow *parent, GLint px, GLint py, GLint pw, GLint ph);

    virtual ~GlutPopupWindow() {};

    void setCenterPosition(int ix, int iy) {
        x = ix - w / 2;
        y = iy;
    };

    void setInfo(const string &str) { info = str; };

    void glDraw() override;
};

class GlutPopupMenuWindow : public GlutWindow {
public :
    GlutPopupMenuWindow(GlutWindow *parent, GLint px, GLint py, GLint pw, GLint ph);

    virtual ~GlutPopupMenuWindow() {};

    void setCenterPosition(int ix, int iy) {
        x = ix - w / 2;
        y = iy;
    };

    void addButton(GlutWindow *button);

    void addButton(int id, const string &titre, GlutPopupMenuWindow *subMenu = NULL);

    GlutWindow *getButton(unsigned int id);

    int mouseFunc(int button, int state, int x, int y) override;

    void glDraw() override;

    void activate(unsigned int n, bool value);

    void addTrace(bID id, int time, const string &str);
};

class GlutHelpWindow : public GlutWindow {
    vector<string> columns;
    string title;
public :
    GlutHelpWindow(GlutWindow *parent, GLint px, GLint py, GLint pw, GLint ph, const string &textFile);

    virtual ~GlutHelpWindow();

    int mouseFunc(int button, int state, int x, int y) override;
    void glDraw() override;
};

#endif /* INTERFACE_H_ */
