#ifndef Catoms3DRotateCode_H_
#define Catoms3DRotateCode_H_
#include <algorithm>
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DMotionEngine.h"
#include "motion/teleportationEvents.h"

static const int UPDATE_MSG_ID = 1001;

using namespace Catoms3D;

class ModuleData {
public:
    Cell3DPosition posFrom;
    Cell3DPosition posTo;
    //uint8_t orient;
    uint16_t stage;

    //ModuleData(bID _id,const Cell3DPosition _pos,uint8_t _orient,uint16_t _stage):id(_id),position(_pos),orient(_orient),stage(_stage) {};
    ModuleData(const Cell3DPosition _posFrom,const Cell3DPosition _posTo,uint16_t _stage):posFrom(_posFrom),posTo(_posTo),stage(_stage) {};
};

class Catoms3DRotateCode : public Catoms3DBlockCode {
private:
    Catoms3DBlock *module;
    FCCLattice *lattice;
    bool isMobile=false;
    int myStage=0;
    int myOrderNum = 0;
    int myDepth = 0;
    vector<ModuleData> &myCurrent;
public :
    Catoms3DRotateCode(Catoms3DBlock *host);
    ~Catoms3DRotateCode() {};

    void myUpdateFunc(std::shared_ptr<Message>_msg,P2PNetworkInterface *sender);

    void startup();
    bool tryToMove(int index);
    void parseUserBlockElements(TiXmlElement *config);
    void onMotionEnd();
    void saveConfiguration();
    void resetConfiguration();
    /**
  * Called by openglviewer during interface drawing phase, can be used by a user
  *  to draw a custom Gl string onto the bottom-left corner of the GUI
  * @note call is made from OpenGlViewer::drawFunc
  * @return a string (can be multi-line with `
`) to display on the GUI
  */
    string onInterfaceDraw() override;

    /*****************************************************************************/
    /** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new Catoms3DRotateCode((Catoms3DBlock*)host));
    };
    /*****************************************************************************/
};

#endif /* Catoms3DRotateCode_H_ */
