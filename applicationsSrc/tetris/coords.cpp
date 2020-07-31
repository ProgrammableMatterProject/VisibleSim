#include "tetrisCode.hpp"

void TetrisCode::sendCoords()
{
    if (topItf != parent && topItf != nullptr and topItf->isConnected())
    {
        CoordsData data = CoordsData(stage, height + 1, width, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData>(COORDSMSG_ID, data), topItf, 0, 0);
        nbBackMsg += 1;
    }
    if (height > 0 and bottomItf != parent and bottomItf != nullptr and bottomItf->isConnected()) // negative height is not sent
    {
        CoordsData data = CoordsData(stage, height - 1, width, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData>(COORDSMSG_ID, data), bottomItf, 0, 0);
        nbBackMsg += 1;
    }
    if (rightItf != parent and rightItf != nullptr and rightItf->isConnected())
    {
        CoordsData data = CoordsData(stage, height, width + 1, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData>(COORDSMSG_ID, data), rightItf, 0, 0);
        nbBackMsg += 1;
    }
    if (width > 0 and leftItf != parent and leftItf != nullptr and leftItf->isConnected()) // negative width is not sent
    {
        CoordsData data = CoordsData(stage, height, width - 1, spanTree);
        sendMessage("Coords Message", new MessageOf<CoordsData>(COORDSMSG_ID, data), leftItf, 0, 0);
        nbBackMsg += 1;
    }
}

void TetrisCode::myCoordsMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<CoordsData> *msg = static_cast<MessageOf<CoordsData> *>(_msg.get());
    CoordsData msgData = *msg->getData();

    console << "new coords : stage = " << stage << "new stage = " << msgData.stage << "\n";
    if (msgData.stage > stage)
    {
        console << "recieved new coords\n";
        stage = msgData.stage;
        height = msgData.height;
        width = msgData.width;
        spanTree = msgData.nbTree;
        maxWidth = 0;
        parent = sender;
        nbBackMsg = 0;
        sendCoords();
        if (nbBackMsg == 0)
        {
            BackCoords data = BackCoords(stage, spanTree);
            if (parent != nullptr && parent->isConnected())
            {
                sendMessage("Back Message Parent", new MessageOf<BackCoords>(BACKMSG_ID, data), parent, 0, 0);
            }
        }
        if (rightItf == nullptr || !rightItf->isConnected())
        {
            console << "sending new max width " << width << "\n";
            maxWidth = width;
            sendIntToAll(MAXWIDTHMSG_MSG_ID, maxWidth);
        }
        pixelCalculation();
    }
    //The coordinates have to be better on at least one of the dimensions, and not worse on the other
    else if (msgData.stage == stage && ((height < msgData.height && width <= msgData.width) || (height <= msgData.height && width < msgData.width)))
    {
        height = msgData.height;
        width = msgData.width;
        spanTree = msgData.nbTree;
        nbBackMsg = 0;
        parent = sender;

        sendCoords();

        if (nbBackMsg == 0)
        {
            BackCoords data = BackCoords(stage, spanTree);
            if (parent != nullptr && parent->isConnected())
            {
                sendMessage("Back Message Parent", new MessageOf<BackCoords>(BACKMSG_ID, data), parent, 0, 0);
            }
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
    else if (msgData.nbTree == spanTree && sender != nullptr && sender->isConnected())
    {
        BackCoords data = BackCoords(stage, spanTree);
        sendMessage("Back Message", new MessageOf<BackCoords>(BACKMSG_ID, data), sender, 0, 0);
    }
};

void TetrisCode::myBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<BackCoords> *msg = static_cast<MessageOf<BackCoords> *>(_msg.get());
    BackCoords msgData = *msg->getData();

    if (msgData.stage >= stage && msgData.spanTree == spanTree)
    {
        stage = msgData.stage;
        nbBackMsg -= 1;
    }
    if (nbBackMsg == 0)
    {
        //if the module is the root of the tree, and all neighbors are ready, a tetramino can be created
        if (module->blockId == spanTree)
        {
            console<<"new tetramino after spreading coords\n";
            module->setColor(MAGENTA);
            nbTmn += 1;
            NewTmnData data = NewTmnData(stage, nbTmn);
            sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<NewTmnData>(NEWTMNMSG_ID, data), 0, 0, 0);
            stringstream strstm;
            strstm << "new tetramino";
            scheduler->trace(strstm.str(), module->blockId, MAGENTA);
        }
        else if (parent != nullptr && parent->isConnected())
        {
            BackCoords data = BackCoords(stage, spanTree);
            sendMessage("Back Message Parent", new MessageOf<BackCoords>(BACKMSG_ID, data), parent, 0, 0);
        }
    }
}

void TetrisCode::sendIntToAll(int MSG_ID, int i)
{
    IntData data = IntData(stage, i);
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("int Message", new MessageOf<IntData>(MSG_ID, data), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("int Message", new MessageOf<IntData>(MSG_ID, data), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("int Message", new MessageOf<IntData>(MSG_ID, data), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected()) // negative width is not send
    {
        sendMessage("int Message", new MessageOf<IntData>(MSG_ID, data), leftItf, 0, 0);
    }
}

void TetrisCode::myMaxHeightMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<IntData> *msg = static_cast<MessageOf<IntData> *>(_msg.get());
    IntData msgData = *msg->getData();
    if (msgData.stage >= stage && maxHeight < msgData.data)
    {
        stage = msgData.stage;
        maxHeight = msgData.data;
        sendIntToAll(MAXHEIGHTMSG_MSG_ID, maxHeight);
        pixelCalculation();
    }
};

void TetrisCode::myMaxWidthMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<IntData> *msg = static_cast<MessageOf<IntData> *>(_msg.get());
    IntData msgData = *msg->getData();

    console << "recieved max width : stage = " << stage << " new stage = " << msgData.stage << " width = " << msgData.data << "\n";
    if (msgData.stage > stage)
    {
        console << "recieved new max width\n";
        stage = msgData.stage;
        maxWidth = msgData.data;
        sendIntToAll(MAXWIDTHMSG_MSG_ID, maxWidth);
        pixelCalculation();
    }
    else if (msgData.stage == stage && maxWidth < msgData.data)
    {
        maxWidth = msgData.data;
        sendIntToAll(MAXWIDTHMSG_MSG_ID, maxWidth);
        pixelCalculation();
    }
};

int TetrisCode::pixelCalculation()
{
    //console<<"pixel calculation\n";    
    if (maxHeight < MIN_HEIGHT || maxWidth < MIN_WIDTH) //The set is too small to display a tetris game
    {
        tmn = PIXEL_NON_VALID;
        module->setColor(BLACK);
    return 0;
    }

    if (stage == 0) //there is no rescaling when the set is split
    {
        sizeOfPixel = maxHeight / MIN_HEIGHT;
        pixelHCoord = height / sizeOfPixel;
        pixelWCoord = width / sizeOfPixel;
        if (sizeOfPixel == 1)
        {
            roleInPixel = ALONE;
        }
        else
        {

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
            else if (wPosition == 0)
            {
                roleInPixel = LEFT_BORDER;
            }
            else if (wPosition == sizeOfPixel - 1)
            {
                roleInPixel = RIGHT_BORDER;
            }
            else if (hPosition == 0)
            {
                roleInPixel = BOTTOM_BORDER;
            }
            else if (hPosition == sizeOfPixel - 1)
            {
                roleInPixel = TOP_BORDER;
            }
            else
            {
                roleInPixel = CORE;
            }
        }
    }
    //console<<"calculating pixels\n";
    pixelHCoord = height / sizeOfPixel;
    pixelWCoord = width / sizeOfPixel;
    int totalHNbPixels = maxHeight / sizeOfPixel; //the number of (full) pixels that are displayed on height
    int totalWNbPixels = maxWidth / sizeOfPixel;  //the number of (full) pixels that are displayed on width
    //The tetraminos appear at the top of the set, in the middle
    //The role of the deciding pixel is the same as in the rest of the game : if the root of the spanning trees
    //in the tetramino changes, it causes synchronization problems.
    //console << " h coord = " << totalHNbPixels - 2 << " w coord = " << totalWNbPixels / 2 << "\n";
    //console << "my h coord = " << pixelHCoord << " my w coord = " << pixelWCoord << "\n";
    if (pixelHCoord == totalHNbPixels - 2 && pixelWCoord == totalWNbPixels / 2 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
    {
        appear_module = true;
    }
    else
    {
        appear_module = false;
    }

    if ((pixelHCoord + 1) * sizeOfPixel - 1 > maxHeight || (pixelWCoord + 1) * sizeOfPixel - 1 > maxWidth) //if the pixel isn't complete
    {
        if (tmn >= 1 || tmn <= 7) //if this module belonged to a tmn, it means that this is a split, and the tetramino needs to be reset.
        {
            if (tmn == 2) //sending the reset
            {
                sendTmn2('S');
            }
            rotation = NO_ROTATION;
            position = NO_POSITION;
            color = NO_COLOR;
            update = 0;
            init = false;
            nbFBack = 0;
            nbFree = 0;
            nbReinit = 0;
            nbReinitBackMsg = 0;
            nbTmnBackMsg = 0;
        }
        tmn = PIXEL_NON_VALID;
        module->setColor(BLACK);
    }
    else if (tmn == PIXEL_NON_VALID)
    {
        tmn = NO_TMN;
        module->setColor(Colors[NO_COLOR]);
    }

    totalBckdModules = (maxWidth / sizeOfPixel) * sizeOfPixel; //the number of modules on width that belong to full pixels;
    int v = blockedLeft + blockedRight + 1;
    if (v == totalBckdModules && (roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE) && topItf != nullptr && topItf->isConnected())
    {
        sendMessage("Asking line Tmn Info", new MessageOf<int>(ASK_INFO_MSG_ID, stage), topItf, 0, 0);
    }

    // module->setColor(Colors[roleInPixel%NB_COLORS]);
    // if(appear_module)
    // {
    //     module->setColor(DARKGREEN);
    // }

    return 1;
}
