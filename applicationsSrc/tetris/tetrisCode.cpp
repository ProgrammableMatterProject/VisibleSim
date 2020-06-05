#include "tetrisCode.hpp"
#include "utils/random.h"
#include <iostream>
#include <array>

TetrisCode::TetrisCode(BlinkyBlocksBlock *host) : BlinkyBlocksBlockCode(host), module(host)
{
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host)
        return;

    // Registers a callback (myHeightMsgFunc) to the message of type E
    addMessageEventFunc2(HEIGHTMSG_MSG_ID,
                         std::bind(&TetrisCode::myHeightMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myWidthMsgFunc) to the message of type D
    addMessageEventFunc2(WIDTHMSG_MSG_ID,
                         std::bind(&TetrisCode::myWidthMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myMaxHeightMsgFunc) to the message of type ?
    addMessageEventFunc2(MAXHEIGHTMSG_MSG_ID,
                         std::bind(&TetrisCode::myMaxHeightMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myMaxWidthMsgFunc) to the message of type ?
    addMessageEventFunc2(MAXWIDTHMSG_MSG_ID,
                         std::bind(&TetrisCode::myMaxWidthMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySendTmn1Func) to the message of type E
    addMessageEventFunc2(TMN1_MSG_ID,
                         std::bind(&TetrisCode::myTmn1Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Initialization of random numbers generator
    srand(Random::getSimulationSeed());
}

void TetrisCode::startup()
{
    console << "start " << module->blockId << "\n";
    topItf = module->getInterface(SCLattice::Direction::Top);
    bottomItf = module->getInterface(SCLattice::Direction::Bottom);
    rightItf = module->getInterface(SCLattice::Direction::Right);
    leftItf = module->getInterface(SCLattice::Direction::Left);

    sendHeight();
    sendWidth();
    module->setColor(Colors[color]);
}

void TetrisCode::sendHeight()
{
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height + 1), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected() and height > 0) // negative height is not send
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height - 1), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected())
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height), leftItf, 0, 0);
    }
}

void TetrisCode::sendWidth()
{
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width + 1), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected() and width > 0) // negative width is not send
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width - 1), leftItf, 0, 0);
    }
}

void TetrisCode::sendIntToAll(int MSG_ID, int i)
{
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("int Message", new MessageOf<int>(MSG_ID, i), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("int Message", new MessageOf<int>(MSG_ID, i), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("int Message", new MessageOf<int>(MSG_ID, i), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected()) // negative width is not send
    {
        sendMessage("int Message", new MessageOf<int>(MSG_ID, i), leftItf, 0, 0);
    }
}

int TetrisCode::pixelCalculation()
{
    if (maxHeight < MIN_HEIGHT || maxWidth < MIN_WIDTH) //The set is too small to display a tetris game
    {
        return 0;
    }

    int sizeOfPixel = maxHeight / MIN_HEIGHT;
    if (sizeOfPixel == 1)
    {
        pixelHCoord = height;
        pixelWCoord = width;
        roleInPixel = ALONE;
    }
    else
    {
        pixelHCoord = height / sizeOfPixel;
        pixelWCoord = width / sizeOfPixel;
        int hPosition = height % sizeOfPixel;
        int wPosition = width % sizeOfPixel;
        if (hPosition == 0 && wPosition == 0)
        {
            roleInPixel = BOTTOM_LEFT_CORNER;
        }
        else if (hPosition == 0 && wPosition == sizeOfPixel - 1)
        {
            roleInPixel = BOTTOM_RIGHT_CORNER;
        }
        else if (hPosition == sizeOfPixel - 1 && wPosition == 0)
        {
            roleInPixel = TOP_LEFT_CORNER;
        }
        else if (hPosition == sizeOfPixel - 1 && wPosition == sizeOfPixel - 1)
        {
            roleInPixel = TOP_RIGHT_CORNER;
        }
        else if (hPosition == 0)
        {
            roleInPixel = LEFT_BORDER;
        }
        else if (hPosition == sizeOfPixel - 1)
        {
            roleInPixel = RIGHT_BORDER;
        }
        else if (wPosition == 0)
        {
            roleInPixel = BOTTOM_BORDER;
        }
        else if (wPosition == sizeOfPixel - 1)
        {
            roleInPixel = TOP_BORDER;
        }
        else
        {
            roleInPixel = CORE;
        }
    }

    int totalHNbPixels = maxHeight / sizeOfPixel; //the number of (full) pixels that are displayed on height
    int totalWNbPixels = maxWidth / sizeOfPixel;  //the number of (full) pixels that are displayed on width
    //The tetraminos appear at the top of the set, in the middle
    if (height == (totalHNbPixels - 1) * sizeOfPixel && width == (totalWNbPixels / 2) * sizeOfPixel)
    {
        appear_module = true;
    }
    else
    {
        appear_module = false;
    }
    return 1;
}

void TetrisCode::tmnAppearance()
{
    int r = (int)rand();
    tmn = r % 7 + 1;
    r = (int)rand();
    rotation = r % 4 + 1;
    while (color == NO_COLOR)
    {
        r = (int)rand();
        color = r % 9;
    }
    module->setColor(Colors[color]);
}

void TetrisCode::myHeightMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (height < msgData)
    {
        height = msgData;
        sendHeight();
    }
    if (height > maxHeight)
    {
        maxHeight = height;
        sendIntToAll(MAXHEIGHTMSG_MSG_ID, maxHeight);
        pixelCalculation();
    }
};

void TetrisCode::myWidthMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (width < msgData)
    {
        width = msgData;
        sendWidth();
    }
    if (width > maxWidth)
    {
        maxWidth = width;
        sendIntToAll(MAXWIDTHMSG_MSG_ID, maxWidth);
        pixelCalculation();
    }
};

void TetrisCode::myMaxHeightMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (maxHeight < msgData)
    {
        maxHeight = msgData;
        sendIntToAll(MAXHEIGHTMSG_MSG_ID, maxHeight);
        pixelCalculation();
    }
};

void TetrisCode::myMaxWidthMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (maxWidth < msgData)
    {
        maxWidth = msgData;
        sendIntToAll(MAXWIDTHMSG_MSG_ID, maxWidth);
        pixelCalculation();
    }
};

void TetrisCode::sendTmn1() // NB : the first tetramino doesn't rotate (square)
{
    std::array<int> data[4] = {rotation, position, color};
    if (roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER)
    {
        if (position == 3)
        {
            data[1] = 1; //If the position is 3, the position sent to the top pixel is 1.
        }
        else if (position == 4)
        {
            data[1] = 2;
        }
        if (position == 3 || position == 4) //The pixels in position 1 and 2 doesn't spread the data to their top pixel
        {
            if (topItf != nullptr and topItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), topItf, 0, 0);
            }
        }
    }
    else
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), topItf, 0, 0);
        }
    }
    data[1] = position;
    if (roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER)
    {
        if (position == 1)
        {
            data[1] = 3; //If the position is 1, the position sent to the bottom pixel is 3.
        }
        else if (position == 2)
        {
            data[1] = 4;
        }
        if (position == 1 || position == 2) //The pixels in position 3 and 4 doesn't spread the data to their bottom pixel
        {
            if (bottomItf != nullptr and bottomItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), bottomItf, 0, 0);
            }
        }
    }
    else
    {
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), bottomItf, 0, 0);
        }
    }
    data[1] = position;
    if (roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER)
    {
        if (position == 1)
        {
            data[1] = 2;
        }
        else if (position == 3)
        {
            data[1] = 4;
        }
        if (position == 1 || position == 3)
        {
            if (rightItf != nullptr and rightItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), rightItf, 0, 0);
            }
        }
    }
    else
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), rightItf, 0, 0);
        }
    }
    data[1] = position;
    if (roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER)
    {
        if (position == 2)
        {
            data[1] = 1;
        }
        else if (position == 4)
        {
            data[1] = 3;
        }
        if (position == 2 || position == 4)
        {
            if (leftItf != nullptr and leftItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), leftItf, 0, 0);
            }
        }
    }
    else
    {
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<std::array>(TMN1_MSG_ID, data), leftItf, 0, 0);
        }
    }
}

void TetrisCode::myTmn1Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<std::array> *msg = static_cast<MessageOf<std::array> *>(_msg.get());
    std::array msgData = *msg->getData();
    if(rotation != msgData[0] || position != msgdata[1] || color != msgData[2])
    {
        rotation = msgData[0];
        position = msgdata[1];
        color = msgData[2]
        module->setColor(Color[color]);
        sendTmn1();
    }
};

void TetrisCode::processLocalEvent(std::shared_ptr<Event> pev)
{
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    BlinkyBlocksBlockCode::processLocalEvent(pev);

    switch (pev->eventType)
    {
    case EVENT_ADD_NEIGHBOR:
    {
        // Do something when a neighbor is added to an interface of the module
        break;
    }

    case EVENT_REMOVE_NEIGHBOR:
    {
        // Do something when a neighbor is removed from an interface of the module
        break;
    }

    case EVENT_INTERRUPTION:
    {
        // Do something when the module receives an event
        break;
    }
    break;
    }
};

void TetrisCode::onUserKeyPressed(unsigned char c, int x, int y)
{
    switch (c)
    {
    case 'a': // update with your code
        break;
    }
};
