/*!
  * \file okteenGlBlock.h
  * \brief okteen Block
  * \date 05/03/2015
  * \author Benoît Piranda
  */

#ifndef OKTEENGLBLOCK_H_
#define OKTEENGLBLOCK_H_
#include <string>
#include "../../gui/objLoader.h"
#include "../../math/matrix44.h"
#include "../../base/glBlock.h"

namespace Okteen {
class OkteenGlBlock:public GlBlock {
protected :
public :
    Matrix mat{};
    uint8_t tabPosConnectors[6];

    OkteenGlBlock(bID id);
    virtual ~OkteenGlBlock() {};

    void setPosition(const Vector3D &p) override;

    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawConnectors(ObjLoader::ObjLoader *ptrObj);
    void glDrawIdConnectors(ObjLoader::ObjLoader *ptrObj, int n);
    void glDrawId(ObjLoader::ObjLoader *ptrObj, int n) override;
    void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) override;
};
}
#endif /* OKTEENGLBLOCK_H_ */
