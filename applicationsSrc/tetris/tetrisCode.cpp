#include "tetrisCode.hpp"
#include "utils/random.h"
#include "utils.cpp"
#include <algorithm>

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

    // Registers a callback (myCoordsMsgFunc) to the message of type ?
    addMessageEventFunc2(BACKMSG_ID,
                         std::bind(&TetrisCode::myBackMsgFunc, this,
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

    // Registers a callback (myTmn3Func) to the message of type E
    addMessageEventFunc2(TMN3_MSG_ID,
                         std::bind(&TetrisCode::myTmn3Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTmn4Func) to the message of type E
    addMessageEventFunc2(TMN4_MSG_ID,
                         std::bind(&TetrisCode::myTmn4Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTmn5Func) to the message of type E
    addMessageEventFunc2(TMN5_MSG_ID,
                         std::bind(&TetrisCode::myTmn5Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTmn6Func) to the message of type E
    addMessageEventFunc2(TMN6_MSG_ID,
                         std::bind(&TetrisCode::myTmn6Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTmn7Func) to the message of type E
    addMessageEventFunc2(TMN7_MSG_ID,
                         std::bind(&TetrisCode::myTmn7Func, this,
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
    if (topItf != parent && topItf != nullptr and topItf->isConnected())
    {
        CoordsData *data = new CoordsData(height + 1, width, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), topItf, 0, 0);
        nbBackMsg += 1;
    }
    if (height > 0 and bottomItf != parent and bottomItf != nullptr and bottomItf->isConnected()) // negative height is not sent
    {
        CoordsData *data = new CoordsData(height - 1, width, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), bottomItf, 0, 0);
        nbBackMsg += 1;
    }
    if (rightItf != parent and rightItf != nullptr and rightItf->isConnected())
    {
        CoordsData *data = new CoordsData(height, width + 1, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), rightItf, 0, 0);
        nbBackMsg += 1;
    }
    if (width > 0 and leftItf != parent and leftItf != nullptr and leftItf->isConnected()) // negative width is not sent
    {
        CoordsData *data = new CoordsData(height, width - 1, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData *>(COORDSMSG_ID, data), leftItf, 0, 0);
        nbBackMsg += 1;
    }
}

void TetrisCode::myCoordsMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<CoordsData *> *msg = static_cast<MessageOf<CoordsData *> *>(_msg.get());
    CoordsData *msgData = *msg->getData();

    //The coordinates have to be better on at least one of the dimensions, and not worse on the other
    if ((height < msgData->height && width <= msgData->width) || (height <= msgData->height && width < msgData->width))
    {
        height = msgData->height;
        width = msgData->width;
        spanTree = msgData->nbTree;
        nbBackMsg = 0;
        parent = sender;

        sendCoords();

        if (nbBackMsg == 0)
        {
            sendMessage("Back Message", new MessageOf<int>(BACKMSG_ID, spanTree), parent, 0, 0);
        }

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
    else if (msgData->nbTree == spanTree)
    {
        sendMessage("Back Message", new MessageOf<int>(BACKMSG_ID, spanTree), sender, 0, 0);
    }
};

void TetrisCode::myBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<unsigned int> *msg = static_cast<MessageOf<unsigned int> *>(_msg.get());
    unsigned int msgData = *msg->getData();

    if (msgData == spanTree)
    {
        nbBackMsg -= 1;
    }
    if (nbBackMsg == 0)
    {
        //if the module is the root of the tree, and all neighbors are ready, a tetramino can be created
        if (module->blockId == spanTree)
        {
            nbTmn += 1;
            sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<int>(NEWTMNMSG_ID, nbTmn), 0, 0, 0);
        }
        else
        {
            sendMessage("Back Message", new MessageOf<int>(BACKMSG_ID, spanTree), parent, 0, 0);
        }
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
        stringstream strstm;
        strstm << "Role is ALONE, pixel is " << pixelHCoord << "," << pixelWCoord;
        scheduler->trace(strstm.str(), module->blockId, GREEN);
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
    if (height == (totalHNbPixels - 2) * sizeOfPixel && width == (totalWNbPixels / 2) * sizeOfPixel)
    {
        appear_module = true;
    }
    else
    {
        appear_module = false;
    }

    if (pixelHCoord + pixelWCoord == 0)
    {
        module->setColor(MAGENTA);
    }
    else
    {
        module->setColor(WHITE);
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
        update = 0;
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
    position = 1;
    update = 1;
    int r = (int)rand();
    tmn = 7; // r % 7 + 1;
    r = (int)rand();
    rotation = r % 4 + 1;
    //Some tetramino would exceed the set
    if ((tmn == 2 && rotation == SOUTH) || (tmn == 3 && rotation == NORTH) || (tmn == 4 && rotation == NORTH))
    {
        position = 3;
    }

    console << "rotation = " << rotation << "\n";
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
    else if (tmn == 2)
    {
        sendTmn2();
    }
    else if (tmn == 3)
    {
        sendTmn3();
    }
    else if (tmn == 4)
    {
        sendTmn4();
    }
    else if (tmn == 5)
    {
        sendTmn5();
    }
    else if (tmn == 6)
    {
        sendTmn6();
    }
    else if (tmn == 7)
    {
        sendTmn7();
    }
}

void TetrisCode::sendTmn1() // NB : the first tetramino doesn't rotate (square)
{
    TmnData data = TmnData(update, rotation, position, color);
    if (roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE)
    {
        if (position == 3)
        {
            data.position = 1; //If the position is 3, the position sent to the top pixel is 1.
        }
        else if (position == 4)
        {
            data.position = 2;
        }
        if (position == 3 || position == 4) //The pixels in position 1 and 2 doesn't spread the data to their top pixel
        {
            if (topItf != nullptr and topItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), topItf, 0, 0);
            }
        }
    }
    else
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), topItf, 0, 0);
        }
    }
    data.position = position;
    if (roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)
    {
        if (position == 1)
        {
            data.position = 3; //If the position is 1, the position sent to the bottom pixel is 3.
        }
        else if (position == 2)
        {
            data.position = 4;
        }
        if (position == 1 || position == 2) //The pixels in position 3 and 4 doesn't spread the data to their bottom pixel
        {
            if (bottomItf != nullptr and bottomItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), bottomItf, 0, 0);
            }
        }
    }
    else
    {
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), bottomItf, 0, 0);
        }
    }
    data.position = position;
    if (roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)
    {
        if (position == 1)
        {
            data.position = 2;
        }
        else if (position == 3)
        {
            data.position = 4;
        }
        if (position == 1 || position == 3)
        {
            if (rightItf != nullptr and rightItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), rightItf, 0, 0);
            }
        }
    }
    else
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), rightItf, 0, 0);
        }
    }
    data.position = position;
    if (roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE)
    {
        if (position == 2)
        {
            data.position = 1;
        }
        else if (position == 4)
        {
            data.position = 3;
        }
        if (position == 2 || position == 4)
        {
            if (leftItf != nullptr and leftItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), leftItf, 0, 0);
            }
        }
    }
    else
    {
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), leftItf, 0, 0);
        }
    }
}

void TetrisCode::myTmn1Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 1 || rotation != msgData.rotation || position != msgData.rotation || color != msgData.color))
    {
        tmn = 1;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn1();
    }
};

void TetrisCode::sendTmn2()
{
    TmnData data = TmnData(update, rotation, position, color);
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
        northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == EAST)
    {
        itf[northId] = rightItf;
        itf[eastId] = bottomItf;
        itf[southId] = leftItf;
        itf[westId] = topItf;
        northBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == SOUTH)
    {
        itf[northId] = bottomItf;
        itf[eastId] = leftItf;
        itf[southId] = topItf;
        itf[westId] = rightItf;
        northBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == WEST)
    {
        itf[northId] = leftItf;
        itf[eastId] = topItf;
        itf[southId] = rightItf;
        itf[westId] = bottomItf;
        northBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    if (northBool)
    {
        if (position == 1)
        {
            data.position = 2;
        }
        else if (position == 3)
        {
            data.position = 1;
        }
        else if (position == 4)
        {
            data.position = 3;
        }
        if (position != 2)
        {
            P2PNetworkInterface *i = itf[northId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 2 Message", new MessageOf<TmnData>(TMN2_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[northId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 2 Message", new MessageOf<TmnData>(TMN2_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (southBool)
    {
        if (position == 1)
        {
            data.position = 3;
        }
        else if (position == 2)
        {
            data.position = 1;
        }
        else if (position == 3)
        {
            data.position = 4;
        }
        if (position != 4)
        {
            P2PNetworkInterface *i = itf[southId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 2 Message", new MessageOf<TmnData>(TMN2_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[southId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 2 Message", new MessageOf<TmnData>(TMN2_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (!eastBool)
    {
        P2PNetworkInterface *i = itf[eastId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 2 Message", new MessageOf<TmnData>(TMN2_MSG_ID, data), i, 0, 0);
        }
    }
    if (!westBool)
    {
        P2PNetworkInterface *i = itf[westId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 2 Message", new MessageOf<TmnData>(TMN2_MSG_ID, data), i, 0, 0);
        }
    }
}

void TetrisCode::myTmn2Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 2 || rotation != msgData.rotation || position != msgData.position || color != msgData.color))
    {
        tmn = 2;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn2();
    }
};

void TetrisCode::sendTmn3()
{
    TmnData data = TmnData(update, rotation, position, color);
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
        northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == EAST)
    {
        itf[northId] = rightItf;
        itf[eastId] = bottomItf;
        itf[southId] = leftItf;
        itf[westId] = topItf;
        northBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == SOUTH)
    {
        itf[northId] = bottomItf;
        itf[eastId] = leftItf;
        itf[southId] = topItf;
        itf[westId] = rightItf;
        northBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == WEST)
    {
        itf[northId] = leftItf;
        itf[eastId] = topItf;
        itf[southId] = rightItf;
        itf[westId] = bottomItf;
        northBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    if (northBool)
    {
        if (position == 3)
        {
            data.position = 4;
        }
        else if (position == 1)
        {
            data.position = 3;
        }
        if (position == 3 || position == 1)
        {
            P2PNetworkInterface *i = itf[northId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[northId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (southBool)
    {
        if (position == 4)
        {
            data.position = 3;
        }
        else if (position == 3)
        {
            data.position = 1;
        }
        if (position == 3 || position == 4)
        {
            P2PNetworkInterface *i = itf[southId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[southId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (eastBool)
    {
        if (position == 1)
        {
            data.position = 2;
        }
        if (position == 1)
        {
            P2PNetworkInterface *i = itf[eastId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[eastId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (westBool)
    {
        if (position == 2)
        {
            data.position = 1;
        }
        if (position == 2)
        {
            P2PNetworkInterface *i = itf[westId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[westId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 3 Message", new MessageOf<TmnData>(TMN3_MSG_ID, data), i, 0, 0);
        }
    }
}

void TetrisCode::myTmn3Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 3 || rotation != msgData.rotation || position != msgData.position || color != msgData.color))
    {
        tmn = 3;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn3();
    }
}

void TetrisCode::sendTmn4()
{
    TmnData data = TmnData(update, rotation, position, color);
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
        northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == EAST)
    {
        itf[northId] = rightItf;
        itf[eastId] = bottomItf;
        itf[southId] = leftItf;
        itf[westId] = topItf;
        northBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == SOUTH)
    {
        itf[northId] = bottomItf;
        itf[eastId] = leftItf;
        itf[southId] = topItf;
        itf[westId] = rightItf;
        northBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == WEST)
    {
        itf[northId] = leftItf;
        itf[eastId] = topItf;
        itf[southId] = rightItf;
        itf[westId] = bottomItf;
        northBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    if (northBool)
    {
        if (position == 3)
        {
            data.position = 4;
        }
        else if (position == 1)
        {
            data.position = 3;
        }
        if (position == 3 || position == 1)
        {
            P2PNetworkInterface *i = itf[northId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[northId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (southBool)
    {
        if (position == 4)
        {
            data.position = 3;
        }
        else if (position == 3)
        {
            data.position = 1;
        }
        if (position == 3 || position == 4)
        {
            P2PNetworkInterface *i = itf[southId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[southId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (eastBool)
    {
        if (position == 2)
        {
            data.position = 1;
        }
        if (position == 2)
        {
            P2PNetworkInterface *i = itf[eastId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[eastId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (westBool)
    {
        if (position == 1)
        {
            data.position = 2;
        }
        if (position == 1)
        {
            P2PNetworkInterface *i = itf[westId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[westId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 4 Message", new MessageOf<TmnData>(TMN4_MSG_ID, data), i, 0, 0);
        }
    }
}

void TetrisCode::myTmn4Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 4 || rotation != msgData.rotation || position != msgData.position || color != msgData.color))
    {
        tmn = 4;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn4();
    }
}

void TetrisCode::sendTmn5()
{
    TmnData data = TmnData(update, rotation, position, color);
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
        northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == EAST)
    {
        itf[northId] = rightItf;
        itf[eastId] = bottomItf;
        itf[southId] = leftItf;
        itf[westId] = topItf;
        northBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == SOUTH)
    {
        itf[northId] = bottomItf;
        itf[eastId] = leftItf;
        itf[southId] = topItf;
        itf[westId] = rightItf;
        northBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == WEST)
    {
        itf[northId] = leftItf;
        itf[eastId] = topItf;
        itf[southId] = rightItf;
        itf[westId] = bottomItf;
        northBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    if (northBool)
    {
        if (position == 1)
        {
            data.position = 3;
        }
        if (position == 1)
        {
            P2PNetworkInterface *i = itf[northId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[northId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (southBool)
    {
        if (position == 3)
        {
            data.position = 1;
        }
        if (position == 3)
        {
            P2PNetworkInterface *i = itf[southId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[southId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (eastBool)
    {
        if (position == 1)
        {
            data.position = 4;
        }
        else if (position == 2)
        {
            data.position = 1;
        }
        if (position == 1 || position == 2)
        {
            P2PNetworkInterface *i = itf[eastId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[eastId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (westBool)
    {
        if (position == 1)
        {
            data.position = 2;
        }
        else if (position == 4)
        {
            data.position = 1;
        }
        if (position == 1 || position == 4)
        {
            P2PNetworkInterface *i = itf[westId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[westId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
        }
    }
}

void TetrisCode::myTmn5Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 5 || rotation != msgData.rotation || position != msgData.position || color != msgData.color))
    {
        tmn = 5;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn5();
    }
}

void TetrisCode::sendTmn6()
{
    TmnData data = TmnData(update, rotation, position, color);
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
        northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == EAST)
    {
        itf[northId] = rightItf;
        itf[eastId] = bottomItf;
        itf[southId] = leftItf;
        itf[westId] = topItf;
        northBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == SOUTH)
    {
        itf[northId] = bottomItf;
        itf[eastId] = leftItf;
        itf[southId] = topItf;
        itf[westId] = rightItf;
        northBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == WEST)
    {
        itf[northId] = leftItf;
        itf[eastId] = topItf;
        itf[southId] = rightItf;
        itf[westId] = bottomItf;
        northBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    if (northBool)
    {
        if (position == 1)
        {
            data.position = 2;
        }
        else if (position == 4)
        {
            data.position = 3;
        }
        if (position == 1 || position == 4)
        {
            P2PNetworkInterface *i = itf[northId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[northId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (southBool)
    {
        if (position == 2)
        {
            data.position = 1;
        }
        else if (position == 3)
        {
            data.position = 4;
        }
        if (position == 2 || position == 3)
        {
            P2PNetworkInterface *i = itf[southId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[southId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (eastBool)
    {
        if (position == 1)
        {
            data.position = 3;
        }
        if (position == 1)
        {
            P2PNetworkInterface *i = itf[eastId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[eastId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (westBool)
    {
        if (position == 3)
        {
            data.position = 1;
        }
        if (position == 3)
        {
            P2PNetworkInterface *i = itf[westId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[westId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 6 Message", new MessageOf<TmnData>(TMN6_MSG_ID, data), i, 0, 0);
        }
    }
}

void TetrisCode::myTmn6Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 6 || rotation != msgData.rotation || position != msgData.position || color != msgData.color))
    {
        tmn = 6;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn6();
    }
}

void TetrisCode::sendTmn7()
{
    TmnData data = TmnData(update, rotation, position, color);
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
        northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == EAST)
    {
        itf[northId] = rightItf;
        itf[eastId] = bottomItf;
        itf[southId] = leftItf;
        itf[westId] = topItf;
        northBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == SOUTH)
    {
        itf[northId] = bottomItf;
        itf[eastId] = leftItf;
        itf[southId] = topItf;
        itf[westId] = rightItf;
        northBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    else if (rotation == WEST)
    {
        itf[northId] = leftItf;
        itf[eastId] = topItf;
        itf[southId] = rightItf;
        itf[westId] = bottomItf;
        northBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        eastBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
        southBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        westBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    }
    if (northBool)
    {
        if (position == 1)
        {
            data.position = 2;
        }
        else if (position == 4)
        {
            data.position = 3;
        }
        if (position == 1 || position == 4)
        {
            P2PNetworkInterface *i = itf[northId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[northId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (southBool)
    {
        if (position == 2)
        {
            data.position = 1;
        }
        else if (position == 3)
        {
            data.position = 4;
        }
        if (position == 2 || position == 3)
        {
            P2PNetworkInterface *i = itf[southId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[southId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (eastBool)
    {
        if (position == 3)
        {
            data.position = 1;
        }
        if (position == 3)
        {
            P2PNetworkInterface *i = itf[eastId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[eastId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
        }
    }
    data.position = position;
    if (westBool)
    {
        if (position == 1)
        {
            data.position = 3;
        }
        if (position == 1)
        {
            P2PNetworkInterface *i = itf[westId];
            if (i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[westId];
        if (i != nullptr and i->isConnected())
        {
            sendMessage("Tmn 7 Message", new MessageOf<TmnData>(TMN7_MSG_ID, data), i, 0, 0);
        }
    }
}

void TetrisCode::myTmn7Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 7 || rotation != msgData.rotation || position != msgData.position || color != msgData.color))
    {
        tmn = 7;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn7();
    }
}

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
