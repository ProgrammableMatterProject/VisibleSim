/**
 * @file   nodes2DGlBlock.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:08:01 2019
 *
 * @brief
 *
 *
 */

#ifndef NODES2DGLBLOCK_H_
#define NODES2DGLBLOCK_H_

#include <string>
#include "../../gui/objLoader.h"
#include "../../math/matrix44.h"
#include "../../base/glBlock.h"

namespace Nodes2D {
class Nodes2DGlBlock:public GlBlock {
    int displayedValue;
public :
    Matrix mat{};

    Nodes2DGlBlock(bID id);
    virtual ~Nodes2DGlBlock() {};

    void setDisplayedValue(int n) { displayedValue = n; }
    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawShadows(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawId(ObjLoader::ObjLoader *ptrObj, int n) override;
    void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) override;
};
}
#endif /* NODES2DGLBLOCK_H_ */
