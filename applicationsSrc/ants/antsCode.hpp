#ifndef antsCode_H_
#define antsCode_H_

#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"

static const int MOVE_MSG_ID = 1001;
static const int IT_WAKEUP_ID = 2001;


using namespace BlinkyBlocks;

enum AntsStates{ANTS_WAIT=0, ANTS_SEARCH, ANTS_TAKE_BACK, ANTS_BACK};
// 0: wait
// 1: chercher la nourriture,
// 2: prendre la nourriture et faire demi-tour,
// 3: se diriger vers le nid en déposant de la phéromone,
// 4:déposer la nourriture dans le nid et faire demi-tour pour revenir à l’état initial.

class AntsData {
public:
    uint16_t id;
    AntsStates state;
    uint16_t pheromone_reserve;
    uint8_t from;
};

class AntsCode : public BlinkyBlocksBlockCode {
private:
    BlinkyBlocksBlock *module = nullptr;
    uint16_t pheromone;
    uint16_t pheromones[6];
    bool isNest, isFood;
    vector<AntsData> ants;
public :
    AntsCode(BlinkyBlocksBlock *host);
    ~AntsCode() {};

/**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

    Time getRandomTime();
    void updateColor();
    void onInterruptionEvent(shared_ptr<Event> event) override;
/**
  * @brief Message handler for the message 'move'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myMoveFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

/**
  * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from it.
  * @param config : pointer to the TiXmlDocument representing the configuration file, all information related to VisibleSim's core have already been parsed
  *
  * Called from BuildingBlock constructor, only once.
  */
    void parseUserElements(TiXmlDocument *config) override;

/**
  * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from each block config. Has to be overridden in the child class.
  * @param config : pointer to the TiXmlElement representing the block configuration file, all information related to concerned block have already been parsed
  *
  */
    void parseUserBlockElements(TiXmlElement *config) override;

/*****************************************************************************/
/** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new AntsCode((BlinkyBlocksBlock *) host));
    }
/*****************************************************************************/
};

#endif /* antsCode_H_ */