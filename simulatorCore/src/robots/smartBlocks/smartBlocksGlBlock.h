/*
 * smartBlocksGlBlock.h
 *
 *  Created on: 23 avr. 2013
 *      Author: ben
 */

#ifndef SMARTBLOCKSGLBLOCK_H_
#define SMARTBLOCKSGLBLOCK_H_
#include <string>
#include "../../gui/objLoader.h"
#include "../../base/glBlock.h"

namespace SmartBlocks {
class SmartBlocksGlBlock:public GlBlock {
protected :
//	string popupInfo,textInfo;
    uint16_t displayedValue;
public :
    static const uint16_t noDisplay=1000;
    SmartBlocksGlBlock(bID id) : GlBlock(id), displayedValue(noDisplay) {};
    virtual ~SmartBlocksGlBlock() {};

    /*	virtual string getPopupInfo();*/
    void setDisplayedValue(uint16_t n) { displayedValue=n; }
    void glDraw(ObjLoader::ObjLoader *ptrObj) override;
    virtual void glDrawId(ObjLoader::ObjLoader *ptrObj,int n) override;
    virtual void glDrawIdByMaterial(ObjLoader::ObjLoader *ptrObj,int &n) override;
};
}
#endif /* SMARTBLOCKSGLBLOCK_H_ */
