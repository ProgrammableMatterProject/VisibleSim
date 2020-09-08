/**
 * @file   nodeGlBlock.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:08:01 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODESGLBLOCK_H_
#define HEXANODESGLBLOCK_H_

#include <string>
#include "../../gui/objLoader.h"

#include "../../math/matrix44.h"
#include "../../base/glBlock.h"

namespace Hexanodes {
class HexanodesGlBlock:public GlBlock {
    int displayedValue;
public :
    Matrix mat{};

    HexanodesGlBlock(bID id);
    virtual ~HexanodesGlBlock() {};

    void setDisplayedValue(int n) { displayedValue = n; }
    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawShadows(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawId(ObjLoader::ObjLoader *ptrObj, int n) override;
    void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) override;
};
}
#endif /* HEXANODESGLBLOCK_H_ */
