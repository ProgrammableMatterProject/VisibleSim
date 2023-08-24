/*
 * catoms3DGlBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef CATOMS3DGLBLOCK_H_
#define CATOMS3DGLBLOCK_H_
#include <string>
#include "../../gui/objLoader.h"
#include "../../math/matrix44.h"
#include "../../base/glBlock.h"

namespace Catoms3D {
class Catoms3DGlBlock:public GlBlock {
protected :
public :
    Matrix mat{};

    Catoms3DGlBlock(bID id) : GlBlock(id) {};
    virtual ~Catoms3DGlBlock() {};
    virtual const Vector3D getPosition() const;

    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawId(ObjLoader::ObjLoader *ptrObj,int n) override;
    void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) override;
    void fireSelectedTrigger() override;
    string getInfo() const override;
//    string getPopupInfo() override;
};
}
#endif /* CATOMS3DGLBLOCK_H_ */
