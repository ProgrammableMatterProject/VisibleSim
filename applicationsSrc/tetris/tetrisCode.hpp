#ifndef tetrisCode_H_
#define tetrisCode_H_

#include "robots/blinkyBlocks/blinkyBlocksSimulator.h"
#include "robots/blinkyBlocks/blinkyBlocksWorld.h"
#include "robots/blinkyBlocks/blinkyBlocksBlockCode.h"

// Possible roles of modules in a pixel
static const int ALONE = 0 ; //if the pixel's size is 1x1
static const int CORE = 1; //all modules that are not on the border of the pixel
static const int TOP_BORDER = 2;
static const int TOP_RIGHT_CORNER = 3;
static const int RIGHT_BORDER = 4;
static const int BOTTOM_RIGHT_CORNER = 5;
static const int BOTTOM_BORDER = 6;
static const int BOTTOM_LEFT_CORNER = 7;
static const int LEFT_BORDER = 8;
static const int TOP_LEFT_CORNER = 9;

//Minimum size of the BB set to display a tetris game (& min nb of pixels on each dimension)
static const int MIN_HEIGHT = 10 ; 
static const int MIN_WIDTH = 3 ;

//Message IDs
static const int HEIGHTMSG_MSG_ID = 1001;
static const int WIDTHMSG_MSG_ID = 1002;
static const int MAXHEIGHTMSG_MSG_ID = 1003;
static const int MAXWIDTHMSG_MSG_ID = 1004;

using namespace BlinkyBlocks;

class TetrisCode : public BlinkyBlocksBlockCode
{
private:
    BlinkyBlocksBlock *module = nullptr;
    P2PNetworkInterface *topItf = nullptr;
    P2PNetworkInterface *bottomItf = nullptr;
    P2PNetworkInterface *rightItf = nullptr;
    P2PNetworkInterface *leftItf = nullptr;
    int height = 0;        //"vertical" coordinate of the module -> to initialize
    int width = 0;         //"horizontal" coordinate of the module -> to initialize
    int maxHeight = 0;     //maximum height of the BBs set
    int maxWidth = 0;      //maximum width of the BBs set
    int pixelHCoord = 0;   //"vertical" coordinate of the pixel the module belongs to
    int pixelWCoord = 0;   //"horizontal" coordinate of the pixel the module belongs to
    int roleInPixel = 100; // role of the module in the pixel (core, border, corner)

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
  * @brief Message sender of height to all neighbors -> to initialize
  */
    void sendHeight();

    /**
  * @brief Message sender of width to all neighbors -> to initialize
  */
    void sendWidth();

    /**
    @brief Message sender of int to all neighbors
    @param MSG_ID int of the id of the message
    @param i int that has to be sent
    
    */
    void sendIntToAll(int MSG_ID, int i);

    /**
    @brief height and width are the coordinates of this module in the BB set. This function calculates 
    the size of the pixels in the set and wich pixel this module belongs to.
    @return 0 if the set is too small to display a tetris game, 1 if the pixel's coords and role have been calculated.
    
    */
    int pixelCalculation();

    /**
  * @brief Message handler for the message 'heightMsg'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myHeightMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

    /**
  * @brief Message handler for the message 'widthMsg'
  * @param _msg Pointer to the message received by the module, requires casting
  * @param sender Connector of the module that has received the message and that is connected to the sender
  */
    void myWidthMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender);

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