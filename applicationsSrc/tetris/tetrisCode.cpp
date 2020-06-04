#include "tetrisCode.hpp"
#include <iostream>

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

    module->setColor(Colors[(height + width) % NB_COLORS]);
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
        return 0 ;
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
    module->setColor(Colors[(pixelHCoord + pixelWCoord) % NB_COLORS]);
    return 1 ;
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
