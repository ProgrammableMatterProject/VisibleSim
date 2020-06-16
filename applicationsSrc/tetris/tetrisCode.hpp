#ifndef tetrisCode_H_
#define tetrisCode_H_

#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"
#include "utils/color.h"
#include "utils.cpp"
#include <vector>

// Possible roles of modules in a pixel
static const int ALONE = 0; //if the pixel's size is 1x1
static const int CORE = 1;  //all modules that are not on the border of the pixel
static const int TOP_BORDER = 2;
static const int TOP_RIGHT_CORNER = 3;
static const int RIGHT_BORDER = 4;
static const int BOTTOM_RIGHT_CORNER = 5;
static const int BOTTOM_BORDER = 6;
static const int BOTTOM_LEFT_CORNER = 7;
static const int LEFT_BORDER = 8;
static const int TOP_LEFT_CORNER = 9;

//Minimum size of the BB set to display a tetris game (& min nb of pixels on each dimension)
static const int MIN_HEIGHT = 10;
static const int MIN_WIDTH = 3;

//Types of tetramino
static const int NO_TMN = 0; //if the module doesn't belong to any tetramino
//All the types of tetramino considered here, and the numbers associated with the positions of the pixels :
static const int NO_POSITION = 0;
/*
NB : The position 1 is the position of the "center" of the tetramino, that means it is the position of the pixel that
"creates" the tetramino AND the center of rotation, except for the first tetramino that doesn't rotate.

Tetramino 1
 --- ---
| 1 | 2 |
 --- ---
| 3 | 4 |
 --- ---

Tetramino 2 
 ---
| 2 |
 ---
| 1 |
 ---
| 3 |
 ---
| 4 |
 ---

Tetramino 3
 ---
| 2 |
 ---
| 1 |
 --- ---
| 3 | 4 |
 --- ---

Tetramino 4
     ---
    | 2 |
     ---
    | 1 |
 --- ---
| 4 | 3 |
 --- ---

Tetramino 5
     ---
    | 3 |
 --- --- ---
| 2 | 1 | 4 |
 --- --- ---

Tetramino 6
 ---
| 2 |
 --- ---
| 1 | 3 |
 --- ---
    | 4 |
     ---

Tetramino 7
     ---
    | 2 |
 --- ---
| 3 | 1 |
 --- ---
| 4 |
 ---

*/

//Types of rotations
static const int NO_ROTATION = 0; //if the module belongs to no tetramino or the rotation wasn't defined
static const int NORTH = 1;       //the tetramino is upright
static const int EAST = 2;        //the top of the tetramino is on the right
static const int SOUTH = 3;       //the tetramino is upside down
static const int WEST = 4;        //the top of the tetramino is on the left

static const int NO_COLOR = 6; //color if the module doesn't belong to any tetramino => WHITE

//Types of movements of the tetraminos
static const int NO_MVT = 0;
static const int DOWN = 1;
static const int ROT_CK = 2;         //rotation clockwise
static const int ROT_COUNTER_CK = 3; // rotation counter clockwise

//Types of free answer
static const int NO_ANSWER = 0;
static const int FREE = 1;     //if the pixel is free
static const int OCCUPIED = 2; //if not

//Message IDs
static const int COORDSMSG_ID = 1001;
static const int BACKMSG_ID = 1002;
static const int MAXHEIGHTMSG_MSG_ID = 1004;
static const int MAXWIDTHMSG_MSG_ID = 1005;
static const int NEWTMNMSG_ID = 1006;
static const int TMN1_MSG_ID = 1007;
static const int TMN2_MSG_ID = 1008;
static const int TMN3_MSG_ID = 1009;
static const int TMN4_MSG_ID = 1010;
static const int TMN5_MSG_ID = 1011;
static const int TMN6_MSG_ID = 1012;
static const int TMN7_MSG_ID = 1013;
static const int TMNBACK_MSG_ID = 1014;
static const int REINITPIX_MSG_ID = 1015;
static const int REINITBACK_MSG_ID = 1016;
static const int START_TMN1_MSG_ID = 1017;
static const int START_TMN2_MSG_ID = 1018;
static const int START_TMN3_MSG_ID = 1019;
static const int START_TMN4_MSG_ID = 1020;
static const int START_TMN5_MSG_ID = 1021;
static const int START_TMN6_MSG_ID = 1022;
static const int START_TMN7_MSG_ID = 1023;
static const int ISFREE_MSG_ID = 1024;
static const int FREEMSG_ID = 1025;
static const int BACKFREEMSG_ID = 1026;
static const int BFMSG_ID = 1027;

//IDs of the interfaces
static const int topId = 0;
static const int bottomId = 1;
static const int rightId = 2;
static const int leftId = 3;

//IDs of the interfaces, to order them depending on the rotation of the tetramino (the north itf is always at the top of the tetramino)
static const int northId = 0;
static const int eastId = 1;
static const int southId = 2;
static const int westId = 3;

using namespace BlinkyBlocks;

class TetrisCode : public BlinkyBlocksBlockCode
{
private:
    BlinkyBlocksBlock *module = nullptr;
    P2PNetworkInterface *topItf = nullptr;
    P2PNetworkInterface *bottomItf = nullptr;
    P2PNetworkInterface *rightItf = nullptr;
    P2PNetworkInterface *leftItf = nullptr;
    int height = 0;                          //"vertical" coordinate of the module -> to initialize
    int width = 0;                           //"horizontal" coordinate of the module -> to initialize
    unsigned int spanTree = module->blockId; //nb of the spanning tree the module belongs to
    int nbBackMsg = 0;                       //number of back messages needed to be ready to start the game
    P2PNetworkInterface *parent = nullptr;   //interface that sent the current spanning tree

    std::vector<bool> readyNghb; //true if the neighbors has confirmed they're ready to start the game
    int maxHeight = 0;           //maximum height of the BBs set
    int maxWidth = 0;            //maximum width of the BBs set
    int pixelHCoord = 0;         //"vertical" coordinate of the pixel the module belongs to
    int pixelWCoord = 0;         //"horizontal" coordinate of the pixel the module belongs to
    int roleInPixel = 100;       // role of the module in the pixel (core, border, corner)

    bool appear_module = false; //true if the module is the one which picks the form, rotation and color of the new tetramino
    int nbTmn = 0;              //number of the current moving tetramino

    P2PNetworkInterface *itf[4];
    bool northBool = false;
    bool eastBool = false;
    bool southBool = false;
    bool westBool = false;

    int tmn = NO_TMN;           // represents the type of tetramino the module is part of
    int rotation = NO_ROTATION; //represents the rotation of the tetramino the module is part of
    int position = NO_POSITION; //reprensents the position of the pixel in the tetramino the module is part of
    int color = NO_COLOR;       //represents the color of the tetramino the module is part of
    int update = 0;             //"id" of the update (to prevent infinite loops)
    int nbTmnBackMsg = 0;       //nb of back messages needed to update the tetramino
    bool init = false;          //true if a reinitialization is needed
    int nbReinit = 0;           //"id" of the reinitialization (to avoid infinite loops)
    int nbReinitBackMsg = 0;    //nb of back messages needed to be sure that the message of reinitialization have been sent to everyone

    int nbFree = 0;  //nb of free pixels verifications
    int nbFBack = 0; //nb of free pixels answers
    std::vector<freeAnswer> verifications;

public:
    TetrisCode(BlinkyBlocksBlock *host);
    ~TetrisCode(){};

    /**
    * This function is called on startup of the blockCode, it can be used to perform initial
    *  configuration of the host or this instance of the program.
    * @note this can be thought of as the main function of the module
    */
    void startup() override;

    /**
    * @brief Message sender of height,width and nbSpanTree to all neighbors -> to initialize
    */
    void sendCoords();

    /**
    * @brief Message handler for the message 'CoordsMsg'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myCoordsMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'BackMsg'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    @brief Message sender of int to all neighbors
    @param MSG_ID int of the id of the message
    @param i int that has to be sent
    
    */
    void sendIntToAll(int MSG_ID, int i);

    /**
    * @brief Message handler for the message 'maxheightMsg'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myMaxHeightMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'widthMsg'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myMaxWidthMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    @brief height and width are the coordinates of this module in the BB set. This function calculates 
    the size of the pixels in the set and wich pixel this module belongs to.
    @return 0 if the set is too small to display a tetris game, 1 if the pixel's coords and role have been calculated.
    
    */
    int pixelCalculation();

    /**
    * @brief Message handler for the message 'newTmnMsg' : spreads that a new tetramino can be created, or start its creation
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myNewTmnMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    @brief creates a tetramino and starts to spread it
    */
    void tmnAppearance();

    /**
    * @brief spread the first tetramino (square)
    * @param reinit (bool) true if the reinitialisation of selected pixels is spread, false if the tetramino itself is spread
    * @param movement (int) if reinit is true, movement is the movement done by the tetramino (to know which pixels have to be reinitialized)
    */
    void sendTmn1(bool reinit, int movement);

    /**
    * @brief Message handler for the message 'tmn1'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmn1Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'RestartTmn1'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myRestartTmn1Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief spread the second tetramino (column)
    * @param reinit (bool) true if the reinitialisation of selected pixels is spread, false if the tetramino itself is spread
    * @param movement (int) if reinit is true, movement is the movement done by the tetramino (to know which pixels have to be reinitialized)
    */
    void sendTmn2(bool reinit, int movement);

    /**
    * @brief Message handler for the message 'tmn2'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmn2Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'RestartTmn2'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myRestartTmn2Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief spread the third tetramino (L)
    * @param reinit (bool) true if the reinitialisation of selected pixels is spread, false if the tetramino itself is spread
    * @param movement (int) if reinit is true, movement is the movement done by the tetramino (to know which pixels have to be reinitialized)
    */
    void sendTmn3(bool reinit, int movement);

    /**
    * @brief Message handler for the message 'tmn3'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmn3Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'RestartTmn3'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myRestartTmn3Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief spread the fourth tetramino (L in mirror)
    * @param reinit (bool) true if the reinitialisation of selected pixels is spread, false if the tetramino itself is spread
    * @param movement (int) if reinit is true, movement is the movement done by the tetramino (to know which pixels have to be reinitialized)
    */
    void sendTmn4(bool reinit, int movement);

    /**
    * @brief Message handler for the message 'tmn4'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmn4Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'RestartTmn4'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myRestartTmn4Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief spread the fifth tetramino (T)
    * @param reinit (bool) true if the reinitialisation of selected pixels is spread, false if the tetramino itself is spread
    * @param movement (int) if reinit is true, movement is the movement done by the tetramino (to know which pixels have to be reinitialized)
    */
    void sendTmn5(bool reinit, int movement);

    /**
    * @brief Message handler for the message 'tmn5'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmn5Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'RestartTmn5'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myRestartTmn5Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief spread the sixth tetramino ("chair")
    * @param reinit (bool) true if the reinitialisation of selected pixels is spread, false if the tetramino itself is spread
    * @param movement (int) if reinit is true, movement is the movement done by the tetramino (to know which pixels have to be reinitialized)
    */
    void sendTmn6(bool reinit, int movement);

    /**
    * @brief Message handler for the message 'tmn6'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmn6Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'RestartTmn6'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myRestartTmn6Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief spread the seventh tetramino ("chair" in mirror)
    * @param reinit (bool) true if the reinitialisation of selected pixels is spread, false if the tetramino itself is spread
    * @param movement (int) if reinit is true, movement is the movement done by the tetramino (to know which pixels have to be reinitialized)
    */
    void sendTmn7(bool reinit, int movement);

    /**
    * @brief Message handler for the message 'tmn7'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmn7Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'RestartTmn7'
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myRestartTmn7Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'tmnback'. Builds a spanning tree inside of the tetramino, to know
    * when it can be updated, and handles the update.
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myTmnBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'ReinitPix'. Spreads that a reinitialization may be needed, 
    * and then calculates if a reinitialization is needed by this module.
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myReinitPixMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'Reinitback'. Builds a spanning tree inside of the tetramino, to know
    * when the reinitialization message have been sent to everyone.
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myReinitBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Updates the tetramino when the verifications have been made
    */
    void updateOfTmn();

    /**
    * @brief Message handler for the message 'IsFree'. Propagates the verification that pixels are free or not.
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myIsFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'Free'. Propagates the verificationto the pixel that needs verification.
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myIFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'BackFree'. Propagates the answer that pixels are free or not.
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myBackFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Message handler for the message 'BF'. The target pixel's answer.
    * @param _msg Pointer to the message received by the module, requires casting
    * @param sender Connector of the module that has received the message and that is connected to the sender
    */
    void myBFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
    * @brief Calculates which pixels have to be verified, and sends the required messages.
    * @param mvt movement made by the tetramino
    */
    void verifTmn1(int mvt);

    /**
    * @brief Calculates which pixels have to be verified, and sends the required messages.
    * @param mvt movement made by the tetramino
    */
    void verifTmn2(int mvt);

    /**
    * @brief Calculates which pixels have to be verified, and sends the required messages.
    * @param mvt movement made by the tetramino
    */
    void verifTmn3(int mvt);

    /**
    * @brief Calculates which pixels have to be verified, and sends the required messages.
    * @param mvt movement made by the tetramino
    */
    void verifTmn4(int mvt);

    /**
    * @brief Calculates which pixels have to be verified, and sends the required messages.
    * @param mvt movement made by the tetramino
    */
    void verifTmn5(int mvt);

    /**
    * @brief Calculates which pixels have to be verified, and sends the required messages.
    * @param mvt movement made by the tetramino
    */
    void verifTmn6(int mvt);

    /**
    * @brief Calculates which pixels have to be verified, and sends the required messages.
    * @param mvt movement made by the tetramino
    */
    void verifTmn7(int mvt);

    /**
    * @brief Spreads the verification messages and answers
    * @param answer true if the data spread is the answer
    * @param data data to be spread
    */
    void sendVerifTmn1(bool answer, isFreeData data);
    
    /**
    * @brief Spreads the verification messages and answers
    * @param answer true if the data spread is the answer
    * @param data data to be spread
    */
    void sendVerifTmn2(bool answer, isFreeData data);
    /**
    * @brief Handler for all events received by the host block
    * @param pev pointer to the received event
    */
    void processLocalEvent(std::shared_ptr<Event> pev) override;

    /**
    * User-implemented keyboard handler function that gets called when
    *  a key press event could not be caught by openglViewer
    * @param c key that was pressed (see openglViewer.cpp)
    * @param x location of the pointer on the x axis
    * @param y location of the pointer on the y axis
    * @note call is made from GlutContext::keyboardFunc (openglViewer.h)
    */
    void onUserKeyPressed(unsigned char c, int x, int y) override;

    /*****************************************************************************/
    /** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host)
    {
        return (new TetrisCode((BlinkyBlocksBlock *)host));
    };
    /*****************************************************************************/
};

#endif /* tetrisCode_H_ */