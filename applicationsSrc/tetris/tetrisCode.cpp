#include "tetrisCode.hpp"
#include "utils/random.h"
#include "utils.cpp"
#include <iostream>
#include <array>

TetrisCode::TetrisCode(BlinkyBlocksBlock *host) : BlinkyBlocksBlockCode(host), module(host)
{
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host)
        return;

    // Registers a callback (myCoordsMsgFunc) to the message of type E
    addMessageEventFunc2(COORDSMSG_ID,
                         std::bind(&TetrisCode::myCoordsMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySpanTreeMsgFunc) to the message of type ?
    // addMessageEventFunc2(SPANTREE_ID,
    //                      std::bind(&TetrisCode::mySpanTreeMsgFunc, this,
    //                                std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myReadyMsgFun) to the message of type ?
    addMessageEventFunc2(READYMSG_ID,
                         std::bind(&TetrisCode::myReadyMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myMaxHeightMsgFunc) to the message of type ?
    addMessageEventFunc2(MAXHEIGHTMSG_MSG_ID,
                         std::bind(&TetrisCode::myMaxHeightMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myMaxWidthMsgFunc) to the message of type ?
    addMessageEventFunc2(MAXWIDTHMSG_MSG_ID,
                         std::bind(&TetrisCode::myMaxWidthMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myNewTmnMsgFunc) to the message of type ?
    addMessageEventFunc2(NEWTMNMSG_ID,
                         std::bind(&TetrisCode::myNewTmnMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTmn1Func) to the message of type E
    addMessageEventFunc2(TMN1_MSG_ID,
                         std::bind(&TetrisCode::myTmn1Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTmn2Func) to the message of type E
    addMessageEventFunc2(TMN2_MSG_ID,
                         std::bind(&TetrisCode::myTmn2Func, this,
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

    sendCoords();
    pixelCalculation();
    module->setColor(Colors[color]);
}

void TetrisCode::sendCoords()
{
    stringstream strstm;
    strstm << "coords : " << height << "," << width << "max dims : " << maxHeight << "," << maxWidth << " tree : " << nbSpanTree;
    scheduler->trace(strstm.str(), module->blockId, MAGENTA);
    if (topItf != nullptr and topItf->isConnected())
    {
        CoordsData *data = new CoordsData(height + 1, width, nbSpanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), topItf, 0, 0);
    }
    if (height > 0 and bottomItf != nullptr and bottomItf->isConnected()) // negative height is not sent
    {
        CoordsData *data = new CoordsData(height - 1, width, nbSpanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        CoordsData *data = new CoordsData(height, width + 1, nbSpanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), rightItf, 0, 0);
    }
    if (width > 0 and leftItf != nullptr and leftItf->isConnected()) // negative width is not sent
    {
        CoordsData *data = new CoordsData(height, width - 1, nbSpanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), leftItf, 0, 0);
    }
}

void TetrisCode::myCoordsMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<CoordsData *> *msg = static_cast<MessageOf<CoordsData *> *>(_msg.get());
    CoordsData *msgData = *msg->getData();
    if (height < msgData->height || width < msgData->width)
    {
        height = msgData->height;
        width = msgData->width;
        nbSpanTree = msgData->nbTree;
        sendCoords();

        if (height > maxHeight)
        {
            maxHeight = height;
            sendIntToAll(MAXHEIGHTMSG_MSG_ID, maxHeight);
            pixelCalculation();
        }
        if (width > maxWidth)
        {
            maxWidth = width;
            sendIntToAll(MAXWIDTHMSG_MSG_ID, maxWidth);
            pixelCalculation();
        }
    }
    else
    {
        stringstream strstm;
        strstm << "coords refused : my coords " << height << "," << width << " VS proposed coords : " << msgData->height << "," << msgData->width;
        scheduler->trace(strstm.str(), module->blockId, CYAN);
    }
};

// void TetrisCode::sendSpanTree()
// {
//     spanNeighbors = 0;
//     if (topItf != nullptr and topItf->isConnected())
//     {
//         sendMessage("Spanning tree Message", new MessageOf<unsigned int>(SPANTREE_ID, nbSpanTree), topItf, 0, 0);
//         spanNeighbors += 1;
//     }
//     if (bottomItf != nullptr and bottomItf->isConnected())
//     {
//         sendMessage("Spanning tree Message", new MessageOf<unsigned int>(SPANTREE_ID, nbSpanTree), bottomItf, 0, 0);
//         spanNeighbors += 1;
//     }
//     if (rightItf != nullptr and rightItf->isConnected())
//     {
//         sendMessage("Spanning tree Message", new MessageOf<unsigned int>(SPANTREE_ID, nbSpanTree), rightItf, 0, 0);
//         spanNeighbors += 1;
//     }
//     if (leftItf != nullptr and leftItf->isConnected())
//     {
//         sendMessage("Spanning tree Message", new MessageOf<unsigned int>(SPANTREE_ID, nbSpanTree), leftItf, 0, 0);
//         spanNeighbors += 1;
//     }
// }

// void TetrisCode::mySpanTreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
// {
//     MessageOf<unsigned int> *msg = static_cast<MessageOf<unsigned int> *>(_msg.get());
//     unsigned int msgData = *msg->getData();
//     if (nbSpanTree == msgData)
//     {
//         spanNeighbors -= 1;
//     }
//     if (spanNeighbors <= 0)
//     {
//         ready = true;
//         sendReady();
//     }
// }

void TetrisCode::sendReady()
{
    nbReadyNghb = 0;
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Ready Message", new MessageOf<unsigned int>(READYMSG_ID, nbSpanTree), topItf, 0, 0);
        nbReadyNghb += 1;
    }
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("Ready Message", new MessageOf<unsigned int>(READYMSG_ID, nbSpanTree), bottomItf, 0, 0);
        nbReadyNghb += 1;
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("Ready Message", new MessageOf<unsigned int>(READYMSG_ID, nbSpanTree), rightItf, 0, 0);
        nbReadyNghb += 1;
    }
    if (leftItf != nullptr and leftItf->isConnected())
    {
        sendMessage("Ready Message", new MessageOf<unsigned int>(READYMSG_ID, nbSpanTree), leftItf, 0, 0);
        nbReadyNghb += 1;
    }
}
void TetrisCode::myReadyMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<unsigned int> *msg = static_cast<MessageOf<unsigned int> *>(_msg.get());
    unsigned int msgData = *msg->getData();
    if (msgData == nbSpanTree)
    {
        nbReadyNghb -= 1;
    }
    //if the module is the root of the tree, and all neighbors are ready, a tetramino can be created
    if (module->blockId == nbSpanTree && nbReadyNghb == 0)
    {
        nbTmn += 1;
        sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<int>(NEWTMNMSG_ID, nbTmn), 0, 0, 0);
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

int TetrisCode::pixelCalculation()
{
    module->setColor(Colors[(pixelHCoord + pixelWCoord) % NB_COLORS]);
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
        module->setColor(CYAN);
    }
    else
    {
        appear_module = false;
    }
    return 1;
}

void TetrisCode::myNewTmnMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (msgData > nbTmn)
    {
        nbTmn = msgData;
        if (appear_module)
        {
            tmnAppearance();
        }
        else
        {
            sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<int>(NEWTMNMSG_ID, nbTmn), 0, 0, 0);
        }
    }
}

void TetrisCode::tmnAppearance()
{
    int r = (int)rand();
    tmn = 1; // r % 7 + 1;
    r = (int)rand();
    rotation = r % 4 + 1;
    while (color == NO_COLOR)
    {
        r = (int)rand();
        color = r % 9;
    }
    module->setColor(Colors[color]);
    if (tmn == 1)
    {
        sendTmn1();
    }
}

void TetrisCode::sendTmn1() // NB : the first tetramino doesn't rotate (square)
{
    TmnData *data = new TmnData(rotation, position, color);
    if (roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER)
    {
        if (position == 3)
        {
            data->position = 1; //If the position is 3, the position sent to the top pixel is 1.
        }
        else if (position == 4)
        {
            data->position = 2;
        }
        if (position == 3 || position == 4) //The pixels in position 1 and 2 doesn't spread the data to their top pixel
        {
            if (topItf != nullptr and topItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), topItf, 0, 0);
            }
        }
    }
    else
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), topItf, 0, 0);
        }
    }
    data->position = position;
    if (roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER)
    {
        if (position == 1)
        {
            data->position = 3; //If the position is 1, the position sent to the bottom pixel is 3.
        }
        else if (position == 2)
        {
            data->position = 4;
        }
        if (position == 1 || position == 2) //The pixels in position 3 and 4 doesn't spread the data to their bottom pixel
        {
            if (bottomItf != nullptr and bottomItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), bottomItf, 0, 0);
            }
        }
    }
    else
    {
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), bottomItf, 0, 0);
        }
    }
    data->position = position;
    if (roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER)
    {
        if (position == 1)
        {
            data->position = 2;
        }
        else if (position == 3)
        {
            data->position = 4;
        }
        if (position == 1 || position == 3)
        {
            if (rightItf != nullptr and rightItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), rightItf, 0, 0);
            }
        }
    }
    else
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), rightItf, 0, 0);
        }
    }
    data->position = position;
    if (roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER)
    {
        if (position == 2)
        {
            data->position = 1;
        }
        else if (position == 4)
        {
            data->position = 3;
        }
        if (position == 2 || position == 4)
        {
            if (leftItf != nullptr and leftItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), leftItf, 0, 0);
            }
        }
    }
    else
    {
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData *>(TMN1_MSG_ID, data), leftItf, 0, 0);
        }
    }
}

void TetrisCode::myTmn1Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<TmnData *> *msg = static_cast<MessageOf<TmnData *> *>(_msg.get());
    TmnData *msgData = *msg->getData();
    if (tmn != 1)
    {
        tmn = 1;
    }
    if (rotation != msgData->rotation || position != msgData->rotation || color != msgData->color)
    {
        rotation = msgData->rotation;
        position = msgData->position;
        color = msgData->color;
        module->setColor(Colors[color]);
        sendTmn1();
    }
};

void TetrisCode::sendTmn2()
{
    TmnData *data = new TmnData(rotation, position, color);
    P2PNetworkInterface *itf[4];
    bool northBool = false;
    bool eastBool = false;
    bool southBool = false;
    bool westBool = false;
    if (rotation == NORTH)
    {
        itf[northId] = topItf;
        itf[eastId] = rightItf;
        itf[southId] = bottomItf;
        itf[westId] = leftItf;
        northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER;
        eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
        southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
        westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER;
    }
    else if (rotation == EAST)
    {
        itf[northId] = rightItf;
        itf[eastId] = bottomItf;
        itf[southId] = leftItf;
        itf[westId] = topItf;
        northBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
        eastBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
        southBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER;
        westBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER;
    }
    else if (rotation == SOUTH)
    {
        itf[northId] = bottomItf;
        itf[eastId] = leftItf;
        itf[southId] = topItf;
        itf[westId] = rightItf;
        northBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
        eastBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER;
        southBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER;
        westBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
    }
    else if (rotation == WEST)
    {
        itf[northId] = leftItf;
        itf[eastId] = topItf;
        itf[southId] = rightItf;
        itf[westId] = bottomItf;
        northBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER;
        eastBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER;
        southBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
        westBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER;
    }
}

void TetrisCode::myTmn2Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<TmnData *> *msg = static_cast<MessageOf<TmnData *> *>(_msg.get());
    TmnData *msgData = *msg->getData();
    if (tmn != 2)
    {
        tmn = 2;
    }
    if (rotation != msgData->rotation || position != msgData->position || color != msgData->color)
    {
        rotation = msgData->rotation;
        position = msgData->position;
        color = msgData->color;
        module->setColor(Colors[color]);
        sendTmn2();
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
