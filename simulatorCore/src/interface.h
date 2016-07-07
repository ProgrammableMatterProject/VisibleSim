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

#include "glBlock.h"
#include "color.h"

#ifndef GLUT
#define GLUT
#endif

// Forward declare the BaseSimulator namespace
namespace BaseSimulator {};

using namespace std;
using namespace BaseSimulator;

class GlutWindow {

protected :
	vector<GlutWindow*> children;
	GLuint idTexture;

	void addChild(GlutWindow *child);
	void bindTexture();
  public :
	GLuint id;
	GLint x,y,w,h;
	GlutWindow(GlutWindow *parent,GLuint pid,GLint px,GLint py,GLint pw,GLint ph,const char *titreTexture);
	virtual ~GlutWindow();
	inline void setGeometry(GLint px,GLint py,GLint pw,GLint ph) { x=px; y=py; w=pw; h=ph; };

	virtual void glDraw();
	virtual int mouseFunc(int button,int state,int mx,int my);
	virtual bool passiveMotionFunc(int mx,int my);
	virtual int keyFunc(int charcode);
	virtual void reshapeFunc(int wx,int wy,int mw,int mh) {};
	static GLuint loadTexture(const char *titre,int &tw,int &th);
	static unsigned char *lectureTarga(const char *titre, int& width, int& height ,bool retourner=false);
	static GLfloat drawString(GLfloat x,GLfloat y,const char *str,void* mode=GLUT_BITMAP_8_BY_13,GLint height=13);
};

class GlutButton : public GlutWindow {
	bool isActive;
	bool isDown;
	bool isHighlighted;
public :
	GlutButton(GlutWindow *parent,GLuint pid,GLint px,GLint py,GLint pw,GLint ph,const char *titreTexture,bool pia=true);
	virtual ~GlutButton() {};

	inline void activate(bool v) { isActive=v; };
	inline bool isActivated() { return isActive; };
	int mouseFunc(int button,int state,int x,int y);
	bool passiveMotionFunc(int mx,int my);
	void glDraw();
};

class BlockDebugData {
public :
	int blockId;
	string str;
	Color color;

	BlockDebugData(int id,const string &s,const Color &c):blockId(id),str(s),color(c) {};
};

class GlutSlider : public GlutWindow {
	int buttonHeight,buttonY;
	int dataTextLines,nbreTextLines,dataPosition;
	bool mouseDown;
	int currentMousePos;
public :
	GlutSlider(GlutWindow *parent,GLuint pid,GLint px,GLint py,GLint ph,const char *titreTexture,int ntl);
	virtual ~GlutSlider();
	void setDataTextLines(int dtl) { dataTextLines=dtl; dataPosition=0; update(); };
	void incDataTextLines() { dataTextLines++; update(); };
	int getPosition() { return dataPosition; }
	void setPosition(int pos) { dataPosition=pos; }
	void glDraw();
	int mouseFunc(int button,int state,int x,int y);
	void update();
};

class GlutInputWindow : public GlutWindow {
    string text;
	// GlutWindow *server; 		// unused
public :
    bool hasFocus;
    GlutInputWindow(GlutWindow *parent,GLuint pid,GLint px,GLint py,GLint pw,GLint ph);
	virtual ~GlutInputWindow();

	int keyFunc(int keycode);
	int mouseFunc(int button,int state,int x,int y);
	void glDraw();
	string getTextAndClear();
};

class GlutSlidingMainWindow : public GlutWindow {
	int openingLevel;
	GlutButton* buttonOpen, *buttonClose;
	multimap<uint64_t,BlockDebugData*> traces;
	GlutSlider *slider;
	GlBlock *selectedGlBlock;
public :
	GlutSlidingMainWindow(GLint px,GLint py,GLint pw,GLint ph,const char *titreTexture);
	virtual ~GlutSlidingMainWindow();

/*	inline void open() { openingLevel++; };
	inline void close() { openingLevel--; };*/
    void openClose();
	int mouseFunc(int button,int state,int mx,int my);
	void reshapeFunc(int wx,int wy,int mw,int mh);
	void glDraw();
	void addTrace(int id,const string &str,const Color &color);
	void select(GlBlock *sb);
	inline bool hasselectedGlBlock()  { return selectedGlBlock!=NULL; };
	inline bool isOpened() { return openingLevel!=0; }
};

class GlutSlidingDebugWindow : public GlutWindow {
	int openingLevel;
	int debugId;
	GlutButton* buttonOpen, *buttonClose;
	GlutSlider* slider;
	GlutInputWindow* input;
	vector <BlockDebugData*> tabDebug;
public :
	GlutSlidingDebugWindow(GLint px,GLint py,GLint pw,GLint ph,const char *titreTexture);
	virtual ~GlutSlidingDebugWindow();

/*	inline void open() { openingLevel++; };
	inline void close() { openingLevel--; };
*/
	int mouseFunc(int button,int state,int mx,int my);
	int keyFunc(int charcode);
	void reshapeFunc(int wx,int wy,int mw,int mh);
	void glDraw();
	inline bool isOpened() { return openingLevel!=0; }
};

class GlutPopupWindow : public GlutWindow {
	string info;
	bool isVisible;
public :
	GlutPopupWindow(GlutWindow *parent,GLint px,GLint py,GLint pw,GLint ph);
	virtual ~GlutPopupWindow() {};

	void show(bool v) { isVisible=v;};
	inline bool isShown() { return isVisible; };
	void setCenterPosition(int ix,int iy) { x=ix-w/2; y=iy; };
	void setInfo(const string &str) { info=str; };
	void glDraw();
};

class GlutPopupMenuWindow : public GlutWindow {
	bool isVisible;
public :
	GlutPopupMenuWindow(GlutWindow *parent,GLint px,GLint py,GLint pw,GLint ph);
	virtual ~GlutPopupMenuWindow() {};

	void show(bool v) { isVisible=v;};
	void setCenterPosition(int ix,int iy) { x=ix-w/2; y=iy; };
	void addButton(int id,const char *titre);
	int mouseFunc(int button,int state,int x,int y);
	void glDraw();
	void activate(unsigned int n,bool value);
	void addTrace(int id,int time,const string &str);
};

class GlutHelpWindow : public GlutWindow {
	bool isVisible;
	unsigned char* text;
public :
	GlutHelpWindow(GlutWindow *parent,GLint px,GLint py,GLint pw,GLint ph,const char *textFile);
	virtual ~GlutHelpWindow();

	void show(bool v) { isVisible=v;};
	void showHide() { isVisible=!isVisible;};
	int mouseFunc(int button,int state,int x,int y);
	void glDraw();
};

#endif /* INTERFACE_H_ */
