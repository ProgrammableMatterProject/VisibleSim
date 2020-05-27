#ifndef gameOfLife3DCode_H_
#define gameOfLife3DCode_H_

#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"
#include <vector>

// Rules of game of life
static const std::vector<int> RuleToSurvive{2,3};
static const std::vector<int> RuleToBeBorn{3};

//Random initialization
static const float propAlive = 1.0 / 3; //proportion of alive modules

static const int ALIVE = 2;
static const int DEAD = 1;
static const int ABSENT = 0;

//To be able to make a difference between all recieved status, different messages are used.
static const int TOPSTATUS_MSG_ID = 1001;
static const int TOPRIGHTSTATUS_MSG_ID = 1002;
static const int RIGHTSTATUS_MSG_ID = 1003;
static const int BOTTOMRIGHTSTATUS_MSG_ID = 1004;
static const int BOTTOMSTATUS_MSG_ID = 1005;
static const int BOTTOMLEFTSTATUS_MSG_ID = 1006;
static const int LEFTSTATUS_MSG_ID = 1007;
static const int TOPLEFTSTATUS_MSG_ID = 1008;
static const int FRONTSTATUS_MSG_ID = 1009;
static const int TOPFRONTSTATUS_MSG_ID = 1010;
static const int TOPRIGHTFRONTSTATUS_MSG_ID = 1011;
static const int RIGHTFRONTSTATUS_MSG_ID = 1012;
static const int BOTTOMRIGHTFRONTSTATUS_MSG_ID = 1013;
static const int BOTTOMFRONTSTATUS_MSG_ID = 1014;
static const int BOTTOMLEFTFRONTSTATUS_MSG_ID = 1015;
static const int LEFTFRONTSTATUS_MSG_ID = 1016;
static const int TOPLEFTFRONTSTATUS_MSG_ID = 1017;
static const int BACKSTATUS_MSG_ID = 1018;
static const int TOPBACKSTATUS_MSG_ID = 1019;
static const int TOPRIGHTBACKSTATUS_MSG_ID = 1020;
static const int RIGHTBACKSTATUS_MSG_ID = 1021;
static const int BOTTOMRIGHTBACKSTATUS_MSG_ID = 1022;
static const int BOTTOMBACKSTATUS_MSG_ID = 1023;
static const int BOTTOMLEFTBACKSTATUS_MSG_ID = 1024;
static const int LEFTBACKSTATUS_MSG_ID = 1025;
static const int TOPLEFTBACKSTATUS_MSG_ID = 1026;
static const int TIME_MSG_ID = 1027;
static const int SYNCHRONIZED_MSG_ID = 1028;
static const int ASKINITTIME_MSG_ID = 1029;
static const int INITTIME_MSG_ID = 1030;
static const int UPDATE_MSG_ID = 1031;

//indices of the neighbors in neighborsStatus & updatedNeighbors
static const int topId = 0;
static const int topRightId = 1;
static const int rightId = 2;
static const int bottomRightId = 3;
static const int bottomId = 4;
static const int bottomLeftId = 5;
static const int leftId = 6;
static const int topLeftId = 7;
static const int frontId = 8;
static const int topFrontId = 9;
static const int topRightFrontId = 17;
static const int RightFrontId = 11;
static const int BottomRightFrontId = 12;
static const int BottomFrontId = 13;
static const int BottomLeftFrontId = 14;
static const int LeftFrontId = 15;
static const int TopLeftFrontId = 16;
static const int backId = 10;
static const int topBackId = 18;
static const int topRightBackId = 19;
static const int RightBackId = 20;
static const int BottomRightBackId = 21;
static const int BottomBackId = 22;
static const int BottomLeftBackId = 23;
static const int LeftBackId = 24;
static const int TopLeftBackId = 25;

using namespace BlinkyBlocks;

class GameOfLife3DCode : public BlinkyBlocksBlockCode
{
private:
    BlinkyBlocksBlock *module = nullptr;

    int status = DEAD;
    std::vector<int> neighborsStatus;
    std::vector<bool> updatedNeighbors;
    int time = 0;
    P2PNetworkInterface *topItf;
    P2PNetworkInterface *rightItf;
    P2PNetworkInterface *leftItf;
    P2PNetworkInterface *bottomItf;
    P2PNetworkInterface *frontItf;
    P2PNetworkInterface *backItf;
    /* The BB alternates between two phases : one during which they send & recieve status,
       and the other during which they update their status according to the number of living neighbors. */
    std::vector<bool> readyNeighbors; // true if all status were recieved and the neighbor's ready to update its status
    std::vector<bool> syncNeighbors;  //true if the neighbor's status is updated and it's ready to send and recieve status
    int nb_updates = 0;
    bool sync_time = false; //true if time was synchronized after the last update
    bool status_updated = false ;
    bool alive = false;     // to initialize
    static inline bool randomAliveInit = false;

public:
    GameOfLife3DCode(BlinkyBlocksBlock *host);
    ~GameOfLife3DCode(){};

    /**
  * This function is called on startup of the blockCode, it can be used to perform initial
  *  configuration of the host or this instance of the program.
  * @note this can be thought of as the main function of the module
  */
    void startup() override;

    /**
 * @brief Sends the status of this to its neighbors
 */
    void sendSelfStatus();
    /**
 * @brief Sends a message to its neighbors saying that it's ready for the update
 */
    void readyForUpdate();

    /**
 * @brief Updates the status according to the nb of living neighbors
 */
    void statusUpdate();

    /**
 * @brief All neighbors have updated their status and are synchronized : restart sending status
 */
    void statusSynchronized();

    /**
 * @brief Check which neighbors are connected and modifies neighborsStatus, as well as updatedNeighbors,
 * readyNeighbors and syncNeighbors so that this doesn't wait for non-existing cubes to be ready.
 * AND tells the neighbors that those neighbors aren't connected
 */
    void checkConnectedNeighbors();

    /**
  * @brief Message handler for the message 'TopStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopRightStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopRightStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'RightStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myRightStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomRightStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomRightStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomLeftStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomLeftStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'LeftStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myLeftStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopLeftStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopLeftStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'FrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopRightFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopRightFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'RightFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myRightFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomRightFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomRightFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomLeftFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomLeftFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'LeftFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myLeftFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopLeftFrontStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopLeftFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopRightBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopRightBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'RightBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myRightBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomRightBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomRightBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'BottomLeftBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myBottomLeftBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'LeftBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myLeftBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'TopLeftBackStatus'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTopLeftBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'Time'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'Synchronized'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void mySynchronizedFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'InitTime'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myInitTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'Update'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myUpdateFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Handler for all events received by the host block
 * @param pev pointer to the received event
 */
    void processLocalEvent(std::shared_ptr<Event> pev) override;

    /**
 * @brief Provides the user with a pointer to the configuration file parser, which can be used to read additional user information from each block config. Has to be overridden in the child class.
 * @param config : pointer to the TiXmlElement representing the block configuration file, all information related to concerned block have already been parsed
 *
 */
    void parseUserBlockElements(TiXmlElement *config) override;

    /**
 * User-implemented keyboard handler function that gets called when
 *  a key press event could not be caught by openglViewer
 * @note call is made from GlutContext::keyboardFunc (openglViewer.h)
 */
    bool parseUserCommandLineArgument(int &argc, char **argv[]) override;


    /*****************************************************************************/
    /** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host)
    {
        return (new GameOfLife3DCode((BlinkyBlocksBlock *)host));
    };
    /*****************************************************************************/
};

#endif /* gameOfLife3DCode_H_ */