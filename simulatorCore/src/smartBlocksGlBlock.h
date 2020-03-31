/*
 * smartBlocksGlBlock.h
 *
 *  Created on: 23 avr. 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSGLBLOCK_H_
#define SMARTBLOCKSGLBLOCK_H_
#include <string>
#include <objLoader.h>
#include "glBlock.h"

namespace SmartBlocks {
class SmartBlocksGlBlock:public GlBlock {
protected :
//	string popupInfo,textInfo;
    int displayedValue;
public :
    SmartBlocksGlBlock(bID id) : GlBlock(id), displayedValue(id) {};
    virtual ~SmartBlocksGlBlock() {};

    /*	virtual string getPopupInfo();*/
    void setDisplayedValue(int n) { displayedValue=n; }
    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    virtual void glDrawId(ObjLoader::ObjLoader *ptrObj,int n) override;
    virtual void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) override;
};
}
#endif /* SMARTBLOCKSGLBLOCK_H_ */
