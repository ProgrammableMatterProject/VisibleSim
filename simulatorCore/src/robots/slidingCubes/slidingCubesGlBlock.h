/*
 * slidingCubesGlBlock.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef SLIDINGCUBESGLBLOCK_H_
#define SLIDINGCUBESGLBLOCK_H_

#include <string>
#include <sstream>
#include "../../gui/objLoader.h"
#include "../../base/glBlock.h"

namespace SlidingCubes {
    class SlidingCubesGlBlock : public GlBlock {
    protected :
    public :
        Matrix mat{};

        SlidingCubesGlBlock(bID id) : GlBlock(id) {};
        virtual ~SlidingCubesGlBlock() {};

        string getInfo() const override;
        string getPopupInfo() const override;

        void glDraw(ObjLoader::ObjLoader *ptrObj) override;
        void glDrawId(ObjLoader::ObjLoader *ptrObj, int n) override;
        void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj, int &n) override;
    };
}
#endif /* SLIDINGCUBESGLBLOCK_H_ */
