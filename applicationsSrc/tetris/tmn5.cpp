#include "tetrisCode.hpp"

void TetrisCode::sendTmn5(bool reinit, int movement)
{
    TmnData data = TmnData(update, rotation, position, color, nbReinit, nbFree);
    ReinitData rData = ReinitData(nbReinit, tmn, movement);
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
            if (reinit && i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
                nbReinitBackMsg += 1;
            }
            else
            {
                if (i != parent && i != nullptr and i->isConnected())
                {
                    sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[northId];
        if (reinit && i != parent && i != nullptr and i->isConnected())
        {
            sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
            nbReinitBackMsg += 1;
        }
        else
        {
            if (i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                nbTmnBackMsg += 1;
            }
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
            if (reinit && i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
                nbReinitBackMsg += 1;
            }
            else
            {
                if (i != parent && i != nullptr and i->isConnected())
                {
                    sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[southId];
        if (reinit && i != parent && i != nullptr and i->isConnected())
        {
            sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
            nbReinitBackMsg += 1;
        }
        else
        {
            if (i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                nbTmnBackMsg += 1;
            }
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
            if (reinit && i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
                nbReinitBackMsg += 1;
            }
            else
            {
                if (i != parent && i != nullptr and i->isConnected())
                {
                    sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[eastId];
        if (reinit && i != parent && i != nullptr and i->isConnected())
        {
            sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
            nbReinitBackMsg += 1;
        }
        else
        {
            if (i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                nbTmnBackMsg += 1;
            }
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
            if (reinit && i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
                nbReinitBackMsg += 1;
            }
            else
            {
                if (i != parent && i != nullptr and i->isConnected())
                {
                    sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        P2PNetworkInterface *i = itf[westId];
        if (reinit && i != parent && i != nullptr and i->isConnected())
        {
            sendMessage("Reinit Tmn 5 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), i, 0, 0);
            nbReinitBackMsg += 1;
        }
        else
        {
            if (i != parent && i != nullptr and i->isConnected())
            {
                sendMessage("Tmn 5 Message", new MessageOf<TmnData>(TMN5_MSG_ID, data), i, 0, 0);
                nbTmnBackMsg += 1;
            }
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
        nbReinit = msgData.nbReinit;
        nbFree = msgData.nbFree;
        parent = sender;
        nbTmnBackMsg = 0;
        module->setColor(Colors[color]);
        sendTmn5(false, NO_MVT);
        if (nbTmnBackMsg == 0 && parent != nullptr && parent->isConnected())
        {
            sendMessage("Tmn Back Message Parent", new MessageOf<int>(TMNBACK_MSG_ID, update), parent, 0, 0);
            // parent = nullptr ;
        }
    }
    else if (update == msgData.nbupdate)
    {
        sendMessage("Tmn Back Message", new MessageOf<int>(TMNBACK_MSG_ID, update), sender, 0, 0);
    }
}

void TetrisCode::myRestartTmn5Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)
    {
        parent = nullptr;
        tmn = 5;
        update = msgData.nbupdate;
        nbReinit = msgData.nbReinit;
        nbFree = msgData.nbFree;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        module->setColor(Colors[color]);
        sendTmn5(false, NO_MVT);
    }
    else if (bottomItf != nullptr && bottomItf->isConnected())
    {
        sendMessage("Restart Tmn 5 Message", new MessageOf<TmnData>(START_TMN5_MSG_ID, msgData), bottomItf, 0, 0);
    }
}

void TetrisCode::verifTmn5()
{
    verifications.clear();
    if (movement == DOWN)
    {
        if (rotation == NORTH)
        {
            verifications.push_back(freeAnswer(2, SOUTH));
            verifications.push_back(freeAnswer(4, SOUTH));
            if (bottomItf != nullptr && bottomItf->isConnected())
            {
                sendMessage("Direct Verification Message", new Message(FREEMSG_ID), bottomItf, 0, 0);
            }
            else
            {
                nbTmn += 1;
                sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<int>(NEWTMNMSG_ID, nbTmn), 0, 0, 0);
            }
        }
        else if (rotation == SOUTH)
        {
            nbFree += 1;
            verifications.push_back(freeAnswer(2, SOUTH));
            verifications.push_back(freeAnswer(3, SOUTH));
            verifications.push_back(freeAnswer(4, SOUTH));
            isFreeData data = isFreeData(nbFree, 4, SOUTH);
            sendVerifTmn2(false, data);
        }
        else if (rotation == EAST)
        {
            nbFree += 1;
            verifications.push_back(freeAnswer(4, SOUTH));
            verifications.push_back(freeAnswer(3, SOUTH));
            isFreeData data = isFreeData(nbFree, 3, SOUTH);
            sendVerifTmn2(false, data);
        }
        else if (rotation == WEST)
        {
            nbFree += 1;
            verifications.push_back(freeAnswer(2, SOUTH));
            verifications.push_back(freeAnswer(3, SOUTH));
            isFreeData data = isFreeData(nbFree, 3, SOUTH);
            sendVerifTmn2(false, data);
        }
    }
}

void TetrisCode::sendVerifTmn5(bool answer, isFreeData data)
{
    P2PNetworkInterface *i = itf[westId];
    if (((position != 2 && position != 3) || !westBool) && i != nullptr && i->isConnected())
    {
        if (answer)
        {
            sendMessage("Back Free Message", new MessageOf<isFreeData>(BACKFREEMSG_ID, data), i, 0, 0);
        }
        else
        {
            sendMessage("Verification Message", new MessageOf<isFreeData>(ISFREE_MSG_ID, data), i, 0, 0);
        }
    }
    i = itf[eastId];
    if (((position != 4 && position != 3) || !eastBool) && i != nullptr && i->isConnected())
    {
        if (answer)
        {
            sendMessage("Back Free Message", new MessageOf<isFreeData>(BACKFREEMSG_ID, data), i, 0, 0);
        }
        else
        {
            sendMessage("Verification Message", new MessageOf<isFreeData>(ISFREE_MSG_ID, data), i, 0, 0);
        }
    }
    i = itf[northId];
    if ((position ==1 || !northBool) && i != nullptr && i->isConnected())
    {
        if (answer)
        {
            sendMessage("Back Free Message", new MessageOf<isFreeData>(BACKFREEMSG_ID, data), i, 0, 0);
        }
        else
        {
            sendMessage("Verification Message", new MessageOf<isFreeData>(ISFREE_MSG_ID, data), i, 0, 0);
        }
    }
    i = itf[southId];
    if ((position ==3 || !southBool) && i != nullptr && i->isConnected())
    {
        if (answer)
        {
            sendMessage("Back Free Message", new MessageOf<isFreeData>(BACKFREEMSG_ID, data), i, 0, 0);
        }
        else
        {
            sendMessage("Verification Message", new MessageOf<isFreeData>(ISFREE_MSG_ID, data), i, 0, 0);
        }
    }
}