#include "tetrisCode.hpp"

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
    //The role of the deciding pixel is the same as in the rest of the game : if the root of the spanning trees
    //in the tetramino changes, it causes synchronization problems.
    if (pixelHCoord == totalHNbPixels - 2 && pixelWCoord == totalWNbPixels / 2 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
    {
        appear_module = true;
    }
    else
    {
        appear_module = false;
    }

    if ((pixelHCoord + 1) * sizeOfPixel > maxHeight || (pixelWCoord + 1) * sizeOfPixel > maxWidth) //if the pixel isn't complete
    {
        tmn = PIXEL_NON_VALID;
        module->setColor(BLACK);
    }
    else
    {
        tmn = NO_TMN;
        module->setColor(Colors[NO_COLOR]);
    }

    // module->setColor(Colors[(pixelHCoord+pixelWCoord)%NB_COLORS]);
    // if(appear_module)
    // {
    //     module->setColor(DARKGREEN);
    // }
    return 1;
}