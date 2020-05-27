#ifndef gameOfLifeCode_H_
#define gameOfLifeCode_H_

#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"

#include <vector>

/* README :

In the shell, -I enables the random initialization and -a <seed> allows to choose the seed to initialize the random numbers generator.

The only case this code doesn't work, is if there are missing modules in the config in diagonals :
XO
OX X module, O missing
In this case, the two modules are neighbors but they don't communicate with each other, so the rules of game of life aren't respected.
*/

// Rules of game of life
static const std::vector<int> RuleToSurvive{2, 3}; //nb of neighbors necessary to survive. regular game of life : 2,3
static const std::vector<int> RuleToBeBorn{3};     //nb of neighbors necessary to be born. regular game of life : 3

//Random initialization
static const float propAlive = 1.0 / 3; //proportion of alive modules

static const int ALIVE = 2;
static const int DEAD = 1;
static const int ABSENT = 0;

static const Color COLOR_ALIVE = BLACK;
static const Color COLOR_DEAD = WHITE;

//To be able to make a difference between all recieved status, different messages are used.
static const int TOPRIGHTLIVES_MSG_ID = 1001;
static const int RIGHTLIVES_MSG_ID = 1002;
static const int BOTTOMRIGHTLIVES_MSG_ID = 1003;
static const int BOTTOMLIVES_MSG_ID = 1004;
static const int BOTTOMLEFTLIVES_MSG_ID = 1005;
static const int LEFTLIVES_MSG_ID = 1006;
static const int TOPLEFTLIVES_MSG_ID = 1007;
static const int TOPLIVES_MSG_ID = 1008;
static const int TIME_MSG_ID = 1009;
static const int SYNCHRONIZED_MSG_ID = 1010;
static const int ASK_INIT_TIME_MSG_ID = 1011;
static const int INIT_TIME_MSG_ID = 1012;
static const int UPDATE_MSG_ID = 1013;

//indices of the neighbors in neighborsStatus & updatedNeighbors
static const int topId = 0;
static const int topRightId = 1;
static const int rightId = 2;
static const int bottomRightId = 3;
static const int bottomId = 4;
static const int bottomLeftId = 5;
static const int leftId = 6;
static const int topLeftId = 7;

using namespace BlinkyBlocks;

class GameOfLifeCode : public BlinkyBlocksBlockCode
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
    /* The BB alternates between two phases : one during which they send & recieve status,
       and the other during which they update their status according to the number of living neighbors. */
    std::vector<bool> readyNeighbors; // true if all status were recieved and the neighbor's ready to update its status
    std::vector<bool> syncNeighbors;  //true if the neighbor's status is updated and it's ready to send and recieve status
    int nb_updates = 0;
    bool sync_time = false; //true if time was synchronized after the last update
    bool alive = false;     // to initialize
    static inline bool randomAliveInit = false;

public:
    GameOfLifeCode(BlinkyBlocksBlock *host);
    ~GameOfLifeCode(){};

    /**
 * This function is called on startup of the blockCode, it can be used to perform initial
 *  configuration of the host or this instance of the program.
 * @note this can be thought of as the main function of the module
 */
    void startup() override;

<<<<<<< HEAD
/**
 * @brief initializes all variables, except status : useful to "restart"
 */
    void initialization();

/**
=======
    /**
>>>>>>> bc907b30804a2aa497e331137c49ef60db9e68ff
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
 * @brief Message handler for the message 'TopRightLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myTopRightLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'RightLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myRightLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'BottomRightLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myBottomRightLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'BottomLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myBottomLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'BottomLeftLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myBottomLeftLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'LeftLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myLeftLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'TopLeftLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myTopLeftLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'TopLives'
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myTopLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

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
 * @brief Message handler for the message 'Asking Init Time' => asking for initialization of time
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myAskInitTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'Init Time' => initializes the time to synchronize added blocks
 * @param _msg Pointer to the message received by the module, requires casting
 * @param sender Connector of the module that has received the message and that is connected to the sender
 */
    void myInitTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
 * @brief Message handler for the message 'Update' => restarts the handling
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
        return (new GameOfLifeCode((BlinkyBlocksBlock *)host));
    };
    /*****************************************************************************/
};

#endif /* gameOfLifeCode_H_ */
