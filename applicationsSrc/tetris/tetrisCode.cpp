#include "tetrisCode.hpp"
#include "utils/random.h"
#include "utils.cpp"
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
        console<<"recieved back msg. nb = "<<nbTmnBackMsg<<"\n";
    }
    if (nbTmnBackMsg == 0)
    {
        //The module that starts the update of the tetramino is on the bottom of the pixel so that it can send the position 1 to the future position 1
        if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
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
        else
        {
            sendMessage("Tmn Back Message Parent", new MessageOf<int>(TMNBACK_MSG_ID, update), parent, 0, 0);
            // parent = nullptr;
        }
    }
}

void TetrisCode::myReinitPixMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<ReinitData> *msg = static_cast<MessageOf<ReinitData> *>(_msg.get());
    ReinitData msgData = *msg->getData();
    console<<"recieved reinit "<<msgData.id<<" my reinit nb = "<<nbReinit<<"\n";
    if (msgData.tmn == tmn && msgData.id > nbReinit)
    {
        //The message that a reinitialization may be needed is spread through the current tetramino by
        //the function `sendTmn*(true,msgData.movement)`. Then the module calculates if it needs a
        //reinitialization (init = true if yes)

        console << "recieved reinit, position = " << position << "\n";
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

        if (nbReinitBackMsg == 0)
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
        console << "recieved reinit back msg, nb = " << nbReinitBackMsg << "\n";
    }
    if (nbReinitBackMsg == 0)
    {
        //The module that starts the update of the tetramino is on the bottom of the pixel so that it can send the position 1 to the future position 1
        if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
        {
            // parent = nullptr;
            TmnData data = TmnData(update, rotation, position, color, nbReinit);
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
                module->setColor(Colors[color]);
            }
        }
        else
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
                module->setColor(Colors[color]);
            }
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
