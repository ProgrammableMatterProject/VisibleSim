#ifndef myMotionTestCode_H_
#define myMotionTestCode_H_

#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DWorld.h"
#include "robots/catoms3D/catoms3DBlockCode.h"


using namespace Catoms3D;

class MyMotionTestCode : public Catoms3DBlockCode {
private:
    Catoms3DBlock *module = nullptr;
public :
    MyMotionTestCode(Catoms3DBlock *host);
    ~MyMotionTestCode() {};
    Cell3DPosition targetPos;
/**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

/**
  * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from each block config. Has to be overridden in the child class.
  * @param config : pointer to the TiXmlElement representing the block configuration file, all information related to concerned block have already been parsed
  *
  */
    void parseUserBlockElements(TiXmlElement *config) override;

/**
  * @brief Callback function executed whenever the module finishes a motion
  */
    void onMotionEnd() override;

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new MyMotionTestCode((Catoms3DBlock*)host));
    }
/*****************************************************************************/
};

#endif /* myMotionTestCode_H_ */