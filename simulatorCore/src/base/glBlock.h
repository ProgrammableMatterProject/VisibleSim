/*
 * glBlock.h
 *
 *  Created on: 23 avr. 2013
 *      Author: ben
 */

#ifndef GLBLOCK_H_
#define GLBLOCK_H_
#include <string>
#include "../gui/shaders.h"
#include "../math/vector3D.h"
#include "../utils/color.h"
#include "../utils/tDefs.h"

namespace ObjLoader {
class ObjLoader;
}

using namespace std;

class GlBlock {
protected :
    bool visible;
    bool highlighted;

public :
    GLfloat position[3];
    GLubyte color[3];
    bID blockId;
    string popupInfoString;

    GlBlock(bID id);
    GlBlock(bID id,const Vector3D &pos, const Vector3D &col);
    virtual ~GlBlock();

    virtual void setPosition(const Vector3D &p);
    virtual void setColor(const Color &c);
    virtual void setColor(const Vector3D &c);
    virtual bool isVisible() const;
    virtual void setVisible(bool visible);
    virtual void toggleHighlight();
    inline void setHighlight(bool v) { highlighted=v; }
    inline bool isHighlighted() const { return highlighted; }
    virtual string getInfo() const;
    virtual string getPopupInfo() const;
    void setPopupInfo(const string &str) { popupInfoString=str; };
    virtual const Vector3D getPosition() const;

    /**
     * Triggers the function of this GlBlock's BlockCode that should be called when this block is selected
     */
    virtual void fireSelectedTrigger() {};

    virtual void glDraw(ObjLoader::ObjLoader *ptrObj) {};
    virtual void glDrawShadows(ObjLoader::ObjLoader *ptrObj) { glDraw(ptrObj); };
    virtual void glDrawId(ObjLoader::ObjLoader *ptrObj,int n);
    virtual void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n);
};

#endif /* GLBLOCK_H_ */
