/*
 * glBlock.h
 *
 *  Created on: 23 avr. 2013
 *      Author: ben
 */

#ifndef GLBLOCK_H_
#define GLBLOCK_H_
#include <string>
#include "shaders.h"
#include "vecteur.h"
#include "color.h"

using namespace std;

class GlBlock {
protected :
	bool isHighlighted;
public :
	GLfloat position[3];
	GLfloat color[4];
	int blockId;

	GlBlock(int id);
	GlBlock(int id,const Vecteur &pos, const Vecteur &col);
	virtual ~GlBlock();

	virtual void setPosition(const Vecteur &p);
	virtual void setColor(const Color &c);
	virtual void setColor(const Vecteur &c);
	virtual void toggleHighlight();
	virtual string getInfo();
	virtual string getPopupInfo();
	virtual const Vecteur getPosition() { return Vecteur(position[0],position[1],position[2],1); };
};

#endif /* GLBLOCK_H_ */
