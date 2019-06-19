/**
 * @file   nodeGlBlock.h
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Jun 19 14:08:01 2019
 *
 * @brief
 *
 *
 */

#ifndef NODEGLBLOCK_H_
#define NODEGLBLOCK_H_

#include <string>
#include <objLoader.h>

#include "matrix44.h"
#include "glBlock.h"

namespace Node {
class NodeGlBlock:public GlBlock {
    int displayedValue;
public :
    Matrix mat{};
    uint8_t tabPosConnectors[4];

    NodeGlBlock(bID id);
    virtual ~NodeGlBlock() {};

    void setPosition(const Vector3D &p) override;

    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawId(ObjLoader::ObjLoader *ptrObj, int n) override;
    void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) override;
};
}
#endif /* NODEGLBLOCK_H_ */
