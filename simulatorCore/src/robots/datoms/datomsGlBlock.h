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

namespace Datoms {
class DatomsGlBlock:public GlBlock {
protected :
public :
    Matrix mat{};
    GLshort currentModel;

    DatomsGlBlock(bID id) : GlBlock(id) { currentModel=1; };
    virtual ~DatomsGlBlock() {};

    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    string getPopupInfo() const override;
};
}
#endif /* DATOMSGLBLOCK_H_ */
