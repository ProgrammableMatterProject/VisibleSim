#include "tetrisCode.hpp"

void TetrisCode::sendTmn1(bool reinit, bool blocked) // NB : the first tetramino doesn't rotate (square)
{
    TmnData data = TmnData(update, rotation, position, color, nbReinit, nbFree);
    ReinitData rData = ReinitData(nbReinit, tmn, movement);

    itf[northId] = topItf;
    itf[eastId] = rightItf;
    itf[southId] = bottomItf;
    itf[westId] = leftItf;
    northBool = roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE;
    eastBool = roleInPixel == RIGHT_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    southBool = roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
    westBool = roleInPixel == LEFT_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;

    if (northBool)
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
            if (reinit && topItf != parent && topItf != nullptr and topItf->isConnected())
            {
                sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), topItf, 0, 0);
                nbReinitBackMsg += 1;
            }
            else if (blocked && topItf != nullptr and topItf->isConnected())
            {
                sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), topItf, 0, 0);
            }
            else
            {
                if (topItf != parent && topItf != nullptr and topItf->isConnected())
                {
                    sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), topItf, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        if (reinit && topItf != parent && topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), topItf, 0, 0);
            nbReinitBackMsg += 1;
        }
        else if (blocked && topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), topItf, 0, 0);
        }
        else
        {
            if (topItf != parent && topItf != nullptr and topItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), topItf, 0, 0);
                nbTmnBackMsg += 1;
            }
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
            if (reinit and bottomItf != parent and bottomItf != nullptr and bottomItf->isConnected())
            {
                sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), bottomItf, 0, 0);
                nbReinitBackMsg += 1;
            }
            else if (blocked && bottomItf != nullptr and bottomItf->isConnected())
            {
                sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), bottomItf, 0, 0);
            }
            else
            {
                if (bottomItf != parent and bottomItf != nullptr and bottomItf->isConnected())
                {
                    sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), bottomItf, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        if (reinit and bottomItf != parent and bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), bottomItf, 0, 0);
            nbReinitBackMsg += 1;
        }
        else if (blocked && bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), bottomItf, 0, 0);
        }
        else
        {
            if (bottomItf != parent and bottomItf != nullptr and bottomItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), bottomItf, 0, 0);
                nbTmnBackMsg += 1;
            }
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
            if (reinit && rightItf != parent && rightItf != nullptr and rightItf->isConnected())
            {
                sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), rightItf, 0, 0);
                nbReinitBackMsg += 1;
            }
            else if (blocked && rightItf != nullptr and rightItf->isConnected())
            {
                sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), rightItf, 0, 0);
            }
            else
            {
                if (rightItf != parent && rightItf != nullptr and rightItf->isConnected())
                {
                    sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), rightItf, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        if (reinit && rightItf != parent && rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), rightItf, 0, 0);
            nbReinitBackMsg += 1;
        }
        else if (blocked && rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), rightItf, 0, 0);
        }
        else
        {
            if (rightItf != parent && rightItf != nullptr and rightItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), rightItf, 0, 0);
                nbTmnBackMsg += 1;
            }
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
            if (reinit && leftItf != parent && leftItf != nullptr and leftItf->isConnected())
            {
                sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), leftItf, 0, 0);
                nbReinitBackMsg += 1;
            }
            else if (blocked && leftItf != nullptr and leftItf->isConnected())
            {
                sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), leftItf, 0, 0);
            }
            else
            {
                if (leftItf != parent && leftItf != nullptr and leftItf->isConnected())
                {
                    sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), leftItf, 0, 0);
                    nbTmnBackMsg += 1;
                }
            }
        }
    }
    else
    {
        if (reinit && leftItf != parent && leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Reinit Tmn 1 Message", new MessageOf<ReinitData>(REINITPIX_MSG_ID, rData), leftItf, 0, 0);
            nbReinitBackMsg += 1;
        }
        else if (blocked && leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Tmn Blocked Msg", new Message(BLOCKED_MSG_ID), leftItf, 0, 0);
        }
        else
        {
            if (leftItf != parent && leftItf != nullptr and leftItf->isConnected())
            {
                sendMessage("Tmn 1 Message", new MessageOf<TmnData>(TMN1_MSG_ID, data), leftItf, 0, 0);
                nbTmnBackMsg += 1;
            }
        }
    }
}

void TetrisCode::myTmn1Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (update < msgData.nbupdate && (tmn != 1 || rotation != msgData.rotation || position != msgData.position || color != msgData.color))
    {
        tmn = 1;
        update = msgData.nbupdate;
        rotation = msgData.rotation;
        position = msgData.position;
        nbReinit = msgData.nbReinit;
        nbFree = msgData.nbFree;
        color = msgData.color;
        parent = sender;
        nbTmnBackMsg = 0;
        module->setColor(Colors[color]);
        sendTmn1(false, false);
        if (nbTmnBackMsg == 0 && parent != nullptr && parent->isConnected())
        {
            sendMessage("Tmn Back Message Parent", new MessageOf<int>(TMNBACK_MSG_ID, update), parent, 0, 0);
        }
    }
    else if (update == msgData.nbupdate)
    {
        sendMessage("Tmn Back Message", new MessageOf<int>(TMNBACK_MSG_ID, update), sender, 0, 0);
    }
};

void TetrisCode::myRestartTmn1Func(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnData> *msg = static_cast<MessageOf<TmnData> *>(_msg.get());
    TmnData msgData = *msg->getData();
    if (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)
    {
        parent = nullptr;
        leaderBlockCode = this;
        tmn = 1;
        update = msgData.nbupdate;
        nbReinit = msgData.nbReinit;
        nbFree = msgData.nbFree;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        goingRight = msgData.goingR;
        goingLeft = msgData.goingL;
        turnCK = msgData.rotCW;
        turnCounterCK = msgData.rotCCW;
        module->setColor(Colors[color]);
        sendTmn1(false, false);
    }
    //Normally the TOP_RIGHT_CORNER recieved it first, so the BOTTOM_RIGHT_CORNER should be under it on the same column
    else
    {
        P2PNetworkInterface *i = nullptr;
        if (sender == topItf)
        {
            i = bottomItf;
        }
        else if (sender == bottomItf)
        {
            i = topItf;
        }
        else if (sender == rightItf)
        {
            i = leftItf;
        }
        else if (sender == leftItf)
        {
            i = rightItf;
        }
        if (i != nullptr && i->isConnected())
        {
            sendMessage("Restart Tmn 1 Message", new MessageOf<TmnData>(START_TMN1_MSG_ID, msgData), i, 0, 0);
        }
    }
}

void TetrisCode::verifTmn1()
{
    verifications.clear();
    if (movement == DOWN)
    {
        //to go down, pixels have to be verified. These needed verification are stored into the vector verifications.
        //With any other tetramino, these verifications depend on the rotation of the tetramino.
        //If the deciding pixel can do the verification itself, it does it (it's not the case here).
        nbFree += 1;
        //creating the needed verifications
        verifications.push_back(freeAnswer(3, SOUTH));
        verifications.push_back(freeAnswer(4, SOUTH));
        //sending the first verification. It has to be the last verification pushed back into the vector.
        isFreeData data = isFreeData(nbFree, 4, SOUTH);
        sendVerifTmn1(false, data);
    }
    else if (movement == GO_RIGHT)
    {
        nbFree += 1;
        verifications.push_back(freeAnswer(2, EAST));
        verifications.push_back(freeAnswer(4, EAST));
        isFreeData data = isFreeData(nbFree, 4, EAST);
        sendVerifTmn1(false, data);
    }
    else if (movement == GO_LEFT)
    {
        nbFree += 1;
        verifications.push_back(freeAnswer(3, WEST));
        verifications.push_back(freeAnswer(1, WEST));
        isFreeData data = isFreeData(nbFree, 1, WEST);
        sendVerifTmn1(false, data);
    }
}

void TetrisCode::sendVerifTmn1(bool answer, isFreeData data)
{
    //The verifications are not sent oustide of the tetramino, so recipients have to be calculated according to the shape of the tetramino.

    P2PNetworkInterface *i = itf[westId];
    if (((position != 1 && position != 3) || !westBool) && i != nullptr && i->isConnected())
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
    if (((position != 4 && position != 2) || !eastBool) && i != nullptr && i->isConnected())
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
    if (((position != 1 && position != 2) || !northBool) && i != nullptr && i->isConnected())
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
    if (((position != 3 && position != 4) || !southBool) && i != nullptr && i->isConnected())
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