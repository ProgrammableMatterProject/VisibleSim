#include "tetrisCode.hpp"
#include "utils/random.h"
#include <algorithm>
#include <unistd.h>

//The program has been split into several files, to be easier to work with
#include "coords.cpp"
#include "tmn1.cpp"
#include "tmn2.cpp"
#include "tmn3.cpp"
#include "tmn4.cpp"
#include "tmn5.cpp"
#include "tmn6.cpp"
#include "tmn7.cpp"

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

    // Registers a callback (myTmnBackFunc) to the message of type E
    addMessageEventFunc2(TMNBACK_MSG_ID,
                         std::bind(&TetrisCode::myTmnBackMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myReinitPixMsgFunc) to the message of type E
    addMessageEventFunc2(REINITPIX_MSG_ID,
                         std::bind(&TetrisCode::myReinitPixMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myReinitBackMsgFunc) to the message of type E
    addMessageEventFunc2(REINITBACK_MSG_ID,
                         std::bind(&TetrisCode::myReinitBackMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRestartTmn1Func) to the message of type E
    addMessageEventFunc2(START_TMN1_MSG_ID,
                         std::bind(&TetrisCode::myRestartTmn1Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRestartTmn2Func) to the message of type E
    addMessageEventFunc2(START_TMN2_MSG_ID,
                         std::bind(&TetrisCode::myRestartTmn2Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRestartTmn3Func) to the message of type E
    addMessageEventFunc2(START_TMN3_MSG_ID,
                         std::bind(&TetrisCode::myRestartTmn3Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRestartTmn4Func) to the message of type E
    addMessageEventFunc2(START_TMN4_MSG_ID,
                         std::bind(&TetrisCode::myRestartTmn4Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRestartTmn5Func) to the message of type E
    addMessageEventFunc2(START_TMN5_MSG_ID,
                         std::bind(&TetrisCode::myRestartTmn5Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRestartTmn6Func) to the message of type E
    addMessageEventFunc2(START_TMN6_MSG_ID,
                         std::bind(&TetrisCode::myRestartTmn6Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRestartTmn7Func) to the message of type E
    addMessageEventFunc2(START_TMN7_MSG_ID,
                         std::bind(&TetrisCode::myRestartTmn7Func, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myIsFreeMsgFunc) to the message of type E
    addMessageEventFunc2(ISFREE_MSG_ID,
                         std::bind(&TetrisCode::myIsFreeMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myIFreeMsgId) to the message of type E
    addMessageEventFunc2(FREEMSG_ID,
                         std::bind(&TetrisCode::myIFreeMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBackFreeMsgFunc) to the message of type E
    addMessageEventFunc2(BACKFREEMSG_ID,
                         std::bind(&TetrisCode::myBackFreeMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBFreeMsgFunc) to the message of type E
    addMessageEventFunc2(BFMSG_ID,
                         std::bind(&TetrisCode::myBFreeMsgFunc, this,
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

void TetrisCode::myNewTmnMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (msgData > nbTmn)
    {
        nbTmn = msgData;
        nbFBack = 0;
        if (appear_module)
        {
            stringstream strstm;
            strstm << "NEW Tetramino !";
            scheduler->trace(strstm.str(), module->blockId, MAGENTA);
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
    parent = nullptr; //The module is the root of the tetramino, it has no parent
    position = 1;
    update = 1;
    nbReinit = 0;
    int r = (int)rand();
    tmn = r % 7 + 1;
    r = (int)rand();
    rotation = r % 4 + 1;
    //Some tetramino would exceed the set
    if (tmn == 2 && rotation == SOUTH)
    {
        position = 3;
    }
    while (color == NO_COLOR)
    {
        r = (int)rand();
        color = r % 9;
    }
    module->setColor(Colors[color]);
    if (tmn == 1)
    {
        sendTmn1(false, NO_MVT);
    }
    else if (tmn == 2)
    {
        sendTmn2(false, NO_MVT);
    }
    else if (tmn == 3)
    {
        sendTmn3(false, NO_MVT);
    }
    else if (tmn == 4)
    {
        sendTmn4(false, NO_MVT);
    }
    else if (tmn == 5)
    {
        sendTmn5(false, NO_MVT);
    }
    else if (tmn == 6)
    {
        sendTmn6(false, NO_MVT);
    }
    else if (tmn == 7)
    {
        sendTmn7(false, NO_MVT);
    }
}

void TetrisCode::myTmnBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (msgData == update)
    {
        nbTmnBackMsg -= 1;
    }
    if (nbTmnBackMsg == 0)
    {
        //Once all modules recieved their new position, the update of the tetramino can be done again.
        //The module that starts the update of the tetramino is on the bottom of the pixel so that it can send the position 1 to the future position 1
        //the update is started by verifying if the wanted movement is possible (for now, DOWN is the only possible movement).
        if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
        {
            if (tmn == 1)
            {
                verifTmn1(DOWN);
            }
            else if (tmn == 2)
            {
                verifTmn2(DOWN);
            }
            else if (tmn == 3)
            {
                verifTmn3(DOWN);
            }
            else if (tmn == 4)
            {
                verifTmn4(DOWN);
            }
            else if (tmn == 5)
            {
                verifTmn5(DOWN);
            }
            else if (tmn == 6)
            {
                verifTmn6(DOWN);
            }
            else if (tmn == 7)
            {
                verifTmn7(DOWN);
            }
        }
        else if (parent != nullptr && parent->isConnected())
        {
            sendMessage("Tmn Back Message Parent", new MessageOf<int>(TMNBACK_MSG_ID, update), parent, 0, 0);
        }
    }
}

void TetrisCode::myReinitPixMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<ReinitData> *msg = static_cast<MessageOf<ReinitData> *>(_msg.get());
    ReinitData msgData = *msg->getData();
    if (msgData.tmn == tmn && msgData.id > nbReinit)
    {
        //The message that a reinitialization may be needed is spread through the current tetramino by
        //the function `sendTmn*(true,msgData.movement)`. Then the module calculates if it needs a
        //reinitialization (init = true if yes)

        nbReinit = msgData.id;
        parent = sender;
        init = false;
        if (msgData.tmn == 1)
        {
            sendTmn1(true, msgData.movement);
            //the first tetramino doesn't rotate, it goes down no matter what
            //the pixels that were in positions 1 and 2 don't belong to the tetramino anymore, they have to be re-initialized
            if (position == 1 || position == 2)
            {
                init = true;
            }
        }
        else if (msgData.tmn == 2)
        {
            sendTmn2(true, msgData.movement);
            if (msgData.movement == DOWN)
            {
                //When the tetramino 2 goes down, if it is upright, only the pixel 2 doesn't belong to the tetramino anymore.
                if (rotation == NORTH && position == 2)
                {
                    init = true;
                }
                else if (rotation == WEST || rotation == EAST)
                {
                    init = true;
                }
                else if (rotation == SOUTH && position == 4)
                {
                    init = true;
                }
            }
            else if ((msgData.movement == ROT_CK || msgData.movement == ROT_COUNTER_CK) && position != 1)
            {
                init = true;
            }
        }
        else if (msgData.tmn == 3)
        {
            sendTmn3(true, msgData.movement);
            if (msgData.movement == DOWN)
            {
                if (rotation == NORTH && (position == 2 || position == 4))
                {
                    init = true;
                }
                else if (rotation == WEST && position != 3)
                {
                    init = true;
                }
                else if (rotation == SOUTH && (position == 3 || position == 4))
                {
                    init = true;
                }
                else if (rotation == EAST && position != 4)
                {
                    init = true;
                }
            }
            else if ((msgData.movement == ROT_CK || msgData.movement == ROT_COUNTER_CK) && position != 1)
            {
                init = true;
            }
        }
        else if (msgData.tmn == 4)
        {
            sendTmn4(true, msgData.movement);
            if (msgData.movement == DOWN)
            {
                if (rotation == NORTH && (position == 2 || position == 4))
                {
                    init = true;
                }
                else if (rotation == WEST && position != 4)
                {
                    init = true;
                }
                else if (rotation == SOUTH && (position == 3 || position == 4))
                {
                    init = true;
                }
                else if (rotation == EAST && position != 3)
                {
                    init = true;
                }
            }
            else if ((msgData.movement == ROT_CK || msgData.movement == ROT_COUNTER_CK) && position != 1)
            {
                init = true;
            }
        }
        else if (msgData.tmn == 5)
        {
            sendTmn5(true, msgData.movement);
            if (msgData.movement == DOWN)
            {
                if (rotation == NORTH && position != 1)
                {
                    init = true;
                }
                else if (rotation == WEST && (position == 3 || position == 4))
                {
                    init = true;
                }
                else if (rotation == SOUTH && position != 3)
                {
                    init = true;
                }
                else if (rotation == EAST && (position == 2 || position == 3))
                {
                    init = true;
                }
            }
            else if (msgData.movement == ROT_CK && position == 2)
            {
                init = true;
            }
            else if (msgData.movement == ROT_COUNTER_CK && position == 4)
            {
                init = true;
            }
        }
        else if (msgData.tmn == 6)
        {
            sendTmn6(true, msgData.movement);
            if (msgData.movement == DOWN)
            {
                if (rotation == NORTH && (position == 2 || position == 3))
                {
                    init = true;
                }
                else if (rotation == WEST && position != 1)
                {
                    init = true;
                }
                else if (rotation == SOUTH && (position == 1 || position == 4))
                {
                    init = true;
                }
                else if (rotation == EAST && position != 3)
                {
                    init = true;
                }
            }
            else if (msgData.movement == ROT_CK && (position == 2 || position == 4))
            {
                init = true;
            }
            else if (msgData.movement == ROT_COUNTER_CK && (position == 3 || position == 4))
            {
                init = true;
            }
        }
        else if (msgData.tmn == 7)
        {
            sendTmn7(true, msgData.movement);
            if (msgData.movement == DOWN)
            {
                if (rotation == NORTH && (position == 2 || position == 3))
                {
                    init = true;
                }
                else if (rotation == WEST && position != 3)
                {
                    init = true;
                }
                else if (rotation == SOUTH && (position == 1 || position == 4))
                {
                    init = true;
                }
                else if (rotation == EAST && position != 1)
                {
                    init = true;
                }
            }
            else if (msgData.movement == ROT_CK && (position == 3 || position == 4))
            {
                init = true;
            }
            else if (msgData.movement == ROT_COUNTER_CK && (position == 2 || position == 4))
            {
                init = true;
            }
        }

        if (nbReinitBackMsg == 0 && parent != nullptr && parent->isConnected())
        {
            sendMessage("Reinit Back Message Parent", new MessageOf<int>(REINITBACK_MSG_ID, nbReinit), parent, 0, 0);
            // parent = nullptr;

            //Reinitialization if needed
            if (init)
            {
                tmn = NO_TMN;
                rotation = NO_ROTATION;
                position = NO_POSITION;
                color = NO_COLOR;
                update = 0;
                init = false;
                nbBackMsg = 0;
                nbFBack = 0;
                nbFree = 0;
                nbReinit = 0;
                nbReinitBackMsg = 0;
                nbTmnBackMsg = 0;
                module->setColor(Colors[color]);
            }
        }
    }
    else if (msgData.tmn == tmn && msgData.id == nbReinit)
    {
        sendMessage("Reinit Back Message", new MessageOf<int>(REINITBACK_MSG_ID, nbReinit), sender, 0, 0);
    }
}

void TetrisCode::myReinitBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (msgData == nbReinit)
    {
        nbReinitBackMsg -= 1;
    }
    if (nbReinitBackMsg == 0)
    {
        //The module that starts the update of the tetramino is on the bottom of the pixel so that it can send the position 1 to the future position 1
        if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
        {
            TmnData data = TmnData(update, rotation, position, color, nbReinit, nbFBack);
            data.nbupdate += 1;
            if (tmn == 1)
            {
                if (bottomItf != nullptr && bottomItf->isConnected())
                {
                    sendMessage("Update Tmn 1 Message", new MessageOf<TmnData>(START_TMN1_MSG_ID, data), bottomItf, 0, 0);
                }
            }
            else if (tmn == 2)
            {
                if (bottomItf != nullptr && bottomItf->isConnected())
                {
                    sendMessage("Update Tmn 2 Message", new MessageOf<TmnData>(START_TMN2_MSG_ID, data), bottomItf, 0, 0);
                }
            }
            else if (tmn == 3)
            {
                if (bottomItf != nullptr && bottomItf->isConnected())
                {
                    sendMessage("Update Tmn 3 Message", new MessageOf<TmnData>(START_TMN3_MSG_ID, data), bottomItf, 0, 0);
                }
            }
            else if (tmn == 4)
            {
                if (bottomItf != nullptr && bottomItf->isConnected())
                {
                    sendMessage("Update Tmn 4 Message", new MessageOf<TmnData>(START_TMN4_MSG_ID, data), bottomItf, 0, 0);
                }
            }
            else if (tmn == 5)
            {
                if (bottomItf != nullptr && bottomItf->isConnected())
                {
                    sendMessage("Update Tmn 5 Message", new MessageOf<TmnData>(START_TMN5_MSG_ID, data), bottomItf, 0, 0);
                }
            }
            else if (tmn == 6)
            {
                if (bottomItf != nullptr && bottomItf->isConnected())
                {
                    sendMessage("Update Tmn 6 Message", new MessageOf<TmnData>(START_TMN6_MSG_ID, data), bottomItf, 0, 0);
                }
            }
            else if (tmn == 7)
            {
                if (bottomItf != nullptr && bottomItf->isConnected())
                {
                    sendMessage("Update Tmn 7 Message", new MessageOf<TmnData>(START_TMN7_MSG_ID, data), bottomItf, 0, 0);
                }
            }

            //Reinitialization if needed : the deciding module never recieved the reinitpixmsg (it sent it)
            //so it never calculated if it needs reinitialization
            if (tmn == 1 ||
                ((tmn == 2 || tmn == 3 || tmn == 4) && (rotation == EAST || rotation == WEST)) ||
                (tmn == 5 && rotation == SOUTH) ||
                (tmn == 6 && (rotation == SOUTH || rotation == EAST)) ||
                (tmn == 7 && (rotation == SOUTH || rotation == WEST)))
            {
                tmn = NO_TMN;
                rotation = NO_ROTATION;
                position = NO_POSITION;
                color = NO_COLOR;
                update = 0;
                init = false;
                nbBackMsg = 0;
                nbReinit = 0;
                nbReinitBackMsg = 0;
                nbFBack = 0;
                nbFree = 0;
                module->setColor(Colors[color]);
            }
        }
        else if (parent != nullptr && parent->isConnected())
        {
            sendMessage("Reinit Back Message Parent", new MessageOf<int>(REINITBACK_MSG_ID, nbReinit), parent, 0, 0);
            // parent = nullptr;
            //Reinitialization if needed
            if (init)
            {
                tmn = NO_TMN;
                rotation = NO_ROTATION;
                position = NO_POSITION;
                color = NO_COLOR;
                update = 0;
                init = false;
                nbBackMsg = 0;
                nbFBack = 0;
                nbFree = 0;
                nbReinit = 0;
                nbReinitBackMsg = 0;
                module->setColor(Colors[color]);
            }
        }
    }
}

void TetrisCode::updateOfTmn()
{
    usleep(1000000);

    stringstream strstm;
    strstm << "UPDATE OF THE Tetramino";
    scheduler->trace(strstm.str(), module->blockId, GREEN);
    nbReinit += 1;
    parent = nullptr;

    //If the modules don't belong to the tetramino after the update, they won't be updated by the function `sendTmn1(false,NO_MVT)`
    //That's why we need to reinitialize them 'by hand', by spreading through the current tetramino the message that a reinitialization
    //may be needed. When modules recieve the message, they spread it to their neighbors, and reinitialize themselves if needed.
    if (tmn == 1)
    {
        sendTmn1(true, DOWN);
    }
    if (tmn == 2)
    {
        sendTmn2(true, DOWN);
    }
    if (tmn == 3)
    {
        sendTmn3(true, DOWN);
    }
    if (tmn == 4)
    {
        sendTmn4(true, DOWN);
    }
    if (tmn == 5)
    {
        sendTmn5(true, DOWN);
    }
    if (tmn == 6)
    {
        sendTmn6(true, DOWN);
    }
    if (tmn == 7)
    {
        sendTmn7(true, DOWN);
    }
}

void TetrisCode::myIsFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<isFreeData> *msg = static_cast<MessageOf<isFreeData> *>(_msg.get());
    isFreeData msgData = *msg->getData();

    if (msgData.id > nbFree)
    {
        nbFree = msgData.id;
        bool b = false;
        P2PNetworkInterface *i = nullptr;
        if (msgData.direction == SOUTH)
        {
            b = roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        }
        else if (msgData.direction == EAST)
        {
            b = roleInPixel == TOP_RIGHT_CORNER || roleInPixel == RIGHT_BORDER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE;
        }
        else if (msgData.direction == WEST)
        {
            b = roleInPixel == TOP_LEFT_CORNER || roleInPixel == LEFT_BORDER || roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        }
        else if (msgData.direction == NORTH)
        {
            b = roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER || roleInPixel == ALONE;
        }

        if (position != msgData.position || !b) //if this module has to spread the verification
        {
            if (tmn == 1)
            {
                sendVerifTmn1(false, msgData);
            }
            else if (tmn == 2)
            {
                sendVerifTmn2(false, msgData);
            }
            else if (tmn == 3)
            {
                sendVerifTmn3(false, msgData);
            }
            else if (tmn == 4)
            {
                sendVerifTmn4(false, msgData);
            }
            else if (tmn == 5)
            {
                sendVerifTmn5(false, msgData);
            }
            else if (tmn == 6)
            {
                sendVerifTmn6(false, msgData);
            }
            else if (tmn == 7)
            {
                sendVerifTmn7(false, msgData);
            }
        }
        else // if this module can ask the answer directly
        {
            if (msgData.direction == SOUTH)
            {
                i = bottomItf;
            }
            else if (msgData.direction == WEST)
            {
                i = leftItf;
            }
            else if (msgData.direction == EAST)
            {
                i = rightItf;
            }
            if (i != nullptr && i->isConnected())
            {
                sendMessage("Asking Free Message", new Message(FREEMSG_ID), i, 0, 0);
            }
            else //if there is no module, it means that it is not free -> the answer is false, directly
            {
                nbFBack = nbFree + 1;
                isFreeData data = isFreeData(nbFBack, position, msgData.direction, false);
                if (tmn == 1)
                {
                    sendVerifTmn1(true, data);
                }
                else if (tmn == 2)
                {
                    sendVerifTmn2(true, data);
                }
                else if (tmn == 3)
                {
                    sendVerifTmn3(true, data);
                }
                else if (tmn == 4)
                {
                    sendVerifTmn4(true, data);
                }
                else if (tmn == 5)
                {
                    sendVerifTmn5(true, data);
                }
                else if (tmn == 6)
                {
                    sendVerifTmn6(true, data);
                }
                else if (tmn == 7)
                {
                    sendVerifTmn7(true, data);
                }
            }
        }
    }
}

//when a module recieves this message, it means that it belongs to the verified pixel (and not to the tetramino that asks the verification)
void TetrisCode::myIFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    //if the module doesn't belong to any tetramino, it is free
    if (tmn == NO_TMN && sender != nullptr && sender->isConnected())
    {
        sendMessage("Answer Free Message", new MessageOf<bool>(BFMSG_ID, true), sender, 0, 0);
    }
    else if (sender != nullptr && sender->isConnected())
    {
        sendMessage("Answer Free Message", new MessageOf<bool>(BFMSG_ID, false), sender, 0, 0);
    }
}

void TetrisCode::myBackFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<isFreeData> *msg = static_cast<MessageOf<isFreeData> *>(_msg.get());
    isFreeData msgData = *msg->getData();

    if (msgData.id > nbFBack && position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
    {
        if (verifications.size() > 0) //this is only to prevent an error at the following line
        {
            freeAnswer f = verifications.at(verifications.size() - 1);

            //the answer can be sent several times : test of direction and position needed, even if normally the test of msgData.id should be enough
            if (msgData.direction == f.direction && msgData.position == f.position)
            {
                nbFBack = msgData.id;
                if (msgData.answer == FREE) // if the answer is free, the verifications can continue.
                //Otherwise, the tetramino is stuck, and an other one has to be started.
                {
                    verifications.pop_back();
                    if (verifications.size() == 0) 
                    {
                        //if there isn't any verification left, it means that all pixels were free -> the update of the tetramino can go on.
                        updateOfTmn();
                    }
                    else //send the next verification
                    {
                        freeAnswer f = verifications.at(verifications.size() - 1);
                        nbFree += 1;
                        isFreeData data = isFreeData(nbFree, f.position, f.direction);
                        if (tmn == 1)
                        {
                            sendVerifTmn1(false, data);
                        }
                        else if (tmn == 2)
                        {
                            sendVerifTmn2(false, data);
                        }
                        else if (tmn == 3)
                        {
                            sendVerifTmn3(false, data);
                        }
                        else if (tmn == 4)
                        {
                            sendVerifTmn4(false, data);
                        }
                        else if (tmn == 5)
                        {
                            sendVerifTmn5(false, data);
                        }
                        else if (tmn == 6)
                        {
                            sendVerifTmn6(false, data);
                        }
                        else if (tmn == 7)
                        {
                            sendVerifTmn7(false, data);
                        }
                    }
                }
                else // if one verification is false, the movement cannot be done. A new tetramino is started
                {
                    nbTmn += 1;
                    sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<int>(NEWTMNMSG_ID, nbTmn), 0, 0, 0);
                }
            }
        }
        else
        {
            stringstream strstm;
            strstm << "ERROR : verifications vector empty";
            scheduler->trace(strstm.str(), module->blockId, RED);
        }
    }
    else if (msgData.id > nbFBack) // if this module is not the deciding module, it has to spread the answer.
    {
        nbFBack = msgData.id;
        if (tmn == 1)
        {
            sendVerifTmn1(true, msgData);
        }
        else if (tmn == 2)
        {
            sendVerifTmn2(true, msgData);
        }
        else if (tmn == 3)
        {
            sendVerifTmn3(true, msgData);
        }
        else if (tmn == 4)
        {
            sendVerifTmn4(true, msgData);
        }
        else if (tmn == 5)
        {
            sendVerifTmn5(true, msgData);
        }
        else if (tmn == 6)
        {
            sendVerifTmn6(true, msgData);
        }
        else if (tmn == 7)
        {
            sendVerifTmn7(true, msgData);
        }
    }
}

void TetrisCode::myBFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<bool> *msg = static_cast<MessageOf<bool> *>(_msg.get());
    bool msgData = *msg->getData();

    if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)) //if it is a direct verification by the deciding module
    {
        if (msgData) //if the verified pixel is free
        {
            if (verifications.size() == 0) 
            {
                //if there isn't any verification left, it means that all pixels were free -> the update of the tetramino can go on.
                updateOfTmn();
            }
            else //the next verification is sent.
            {
                freeAnswer f = verifications.at(verifications.size() - 1);
                nbFree += 1;
                isFreeData data = isFreeData(nbFree, f.position, f.direction);
                if (tmn == 1)
                {
                    sendVerifTmn1(false, data);
                }
                else if (tmn == 2)
                {
                    sendVerifTmn2(false, data);
                }
                else if (tmn == 3)
                {
                    sendVerifTmn3(false, data);
                }
                else if (tmn == 4)
                {
                    sendVerifTmn4(false, data);
                }
                else if (tmn == 5)
                {
                    sendVerifTmn5(false, data);
                }
                else if (tmn == 6)
                {
                    sendVerifTmn6(false, data);
                }
                else if (tmn == 7)
                {
                    sendVerifTmn7(false, data);
                }
            }
        }
        else //if the verified pixel isn't free, the tetramino is stuck : a new tetramino has to be created.
        {
            nbTmn += 1;
            sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<int>(NEWTMNMSG_ID, nbTmn), 0, 0, 0);
        }
    }
    else //if the module is not the deciding module, it has to send the answer.
    {
        nbFBack = nbFree + 1;
        int direction = 0;

        //the module didnt keep the direction asked by the verification : it is deduced using sender.
        if (sender == topItf)
        {
            direction = NORTH;
        }
        else if (sender == bottomItf)
        {
            direction = SOUTH;
        }
        else if (sender == leftItf)
        {
            direction = WEST;
        }
        else if (sender == rightItf)
        {
            direction = EAST;
        }
        int result = NO_ANSWER;

        //converting the boolean into the int representing the result.
        if (msgData)
        {
            result = FREE;
        }
        else if (!msgData)
        {
            result = OCCUPIED;
        }

        //sending the answer
        isFreeData data = isFreeData(nbFBack, position, direction, result);
        if (tmn == 1)
        {
            sendVerifTmn2(true, data);
        }
        else if (tmn == 2)
        {
            sendVerifTmn2(true, data);
        }
        else if (tmn == 3)
        {
            sendVerifTmn3(true, data);
        }
        else if (tmn == 4)
        {
            sendVerifTmn4(true, data);
        }
        else if (tmn == 5)
        {
            sendVerifTmn5(true, data);
        }
        else if (tmn == 6)
        {
            sendVerifTmn6(true, data);
        }
        else if (tmn == 7)
        {
            sendVerifTmn7(true, data);
        }
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
