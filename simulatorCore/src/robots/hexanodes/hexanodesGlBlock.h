/**
 * @file   nodeGlBlock.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:08:01 2019
 *
 * @brief
 *
 *
 */

#ifndef HEXANODEGLBLOCK_H_
#define HEXANODEGLBLOCK_H_

#include <string>
#include <objLoader.h>

#include "matrix44.h"
#include "glBlock.h"

namespace Hexanode {
class HexanodeGlBlock:public GlBlock {
    int displayedValue;
public :
    Matrix mat{};

    HexanodeGlBlock(bID id);
    virtual ~HexanodeGlBlock() {};

    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
		void glDrawShadows(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawId(ObjLoader::ObjLoader *ptrObj, int n) override;
    void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) override;
};
}
#endif /* HEXANODEGLBLOCK_H_ */
