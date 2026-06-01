/*!
 * \file datomsGlBlock.h
 * \brief deformable atoms gl
 * \date 28/01/2018
 * \author Benoît Piranda
 */

#ifndef DATOMSGLBLOCK_H_
#define DATOMSGLBLOCK_H_
#include <string>
#include "../../gui/objLoader.h"
#include "../../math/matrix44.h"
#include "../../base/glBlock.h"
#include "math/cell3DPosition.h"
#include "datomsUtils.h"

namespace Datoms {
class DatomsGlBlock:public GlBlock {
public :
    Matrix mat{};
    PistonId piston;
    float coef;

    DatomsGlBlock(bID id) : GlBlock(id) { piston=AllPistonsOff; };
    virtual ~DatomsGlBlock() {};

    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    void glDrawId(ObjLoader::ObjLoader *ptrObj,int n) override;
    void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) override;
    string getPopupInfo() const override;
};
}
#endif /* DATOMSGLBLOCK_H_ */
