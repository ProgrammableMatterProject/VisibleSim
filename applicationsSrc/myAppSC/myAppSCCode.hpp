#ifndef myAppSCCode_H_
#define myAppSCCode_H_

#include "robots/slidingCubes/slidingCubesSimulator.h"
#include "robots/slidingCubes/slidingCubesWorld.h"
#include "robots/slidingCubes/slidingCubesBlockCode.h"

static const int ELECT_MSG_ID = 1001;

using namespace SlidingCubes;

class MyAppSCCode : public SlidingCubesBlockCode {
private:
    SlidingCubesBlock *module = nullptr;
    bool isLeader;
public :
    MyAppSCCode(SlidingCubesBlock *host);

    ~MyAppSCCode() {};

/**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

/**
    * @brief Message handler for the message 'Elect'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myElectFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

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

/**
  * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from it.
  * @param config : pointer to the TiXmlDocument representing the configuration file, all information related to VisibleSim's core have already been parsed
  *
  * Called from BuildingBlock constructor, only once.
  */
    void parseUserElements(TiXmlDocument *config) override;

/**
  * Call by world during GL drawing phase, can be used by a user
  *  to draw custom Gl content into the simulated world
  * @note call is made from World::GlDraw
  */
    void onGlDraw() override;

    bool tryToMove();
    P2PNetworkInterface *findNeighborAt(const Cell3DPosition &pos);

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new MyAppSCCode((SlidingCubesBlock *) host));
    }
/*****************************************************************************/
};

#endif /* myAppSCCode_H_ */