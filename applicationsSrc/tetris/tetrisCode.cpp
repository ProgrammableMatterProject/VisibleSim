#include "tetrisCode.hpp"
#include "utils/random.h"
#include <algorithm>
#include <unistd.h>

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

    // Registers a callback (myFarVerifMsgFunc) to the message of type E
    addMessageEventFunc2(FAR_VERIF_MSG_ID,
                         std::bind(&TetrisCode::myFarVerifMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBackFarVMsgFunc) to the message of type E
    addMessageEventFunc2(BACK_FAR_V_MSG_ID,
                         std::bind(&TetrisCode::myBackFarVMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBlockedMsgFunc) to the message of type E
    addMessageEventFunc2(BLOCKED_MSG_ID,
                         std::bind(&TetrisCode::myBlockedMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myCountBlockedMsgFunc) to the message of type E
    addMessageEventFunc2(COUNT_BCK_MSG_ID,
                         std::bind(&TetrisCode::myCountBlockedMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myAskTmnInfoMsgFunc) to the message of type E
    addMessageEventFunc2(ASK_INFO_MSG_ID,
                         std::bind(&TetrisCode::myAskTmnInfoMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTmnInfoMsgFunc) to the message of type E
    addMessageEventFunc2(TMN_INFO_MSG_ID,
                         std::bind(&TetrisCode::myTmnInfoMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySplitMsgFunc) to the message of type E
    addMessageEventFunc2(SPLIT_MSG_ID,
                         std::bind(&TetrisCode::mySplitMsgFunc, this,
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
    MessageOf<NewTmnData> *msg = static_cast<MessageOf<NewTmnData> *>(_msg.get());
    NewTmnData msgData = *msg->getData();
    if (stage >= msgData.stage && msgData.numbTmn > nbTmn)
    {
        stage = msgData.stage;
        nbTmn = msgData.numbTmn;
        nbFBack = 0;
        nbFree = 0;
        if (appear_module)
        {
            stringstream strstm;
            strstm << "NEW Tetramino !";
            scheduler->trace(strstm.str(), module->blockId, MAGENTA);
            tmnAppearance();
        }
        else
        {
            NewTmnData data = NewTmnData(stage, nbTmn);
            sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<NewTmnData>(NEWTMNMSG_ID, data), 0, 0, 0);
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
    while (color == NO_COLOR || color == 8) //The color at 8th place is too close to white, it is almost invisible
    {
        r = (int)rand();
        color = r % 9;
    }
    module->setColor(Colors[color]);
    console << "deciding module : send tmn " << tmn << " rot = " << rotation << " pos = " << position << "\n";
    if (tmn == 1)
    {
        sendTmn1(false, false);
    }
    else if (tmn == 2)
    {
        sendTmn2(false, false);
    }
    else if (tmn == 3)
    {
        sendTmn3(false, false);
    }
    else if (tmn == 4)
    {
        sendTmn4(false, false);
    }
    else if (tmn == 5)
    {
        sendTmn5(false, false);
    }
    else if (tmn == 6)
    {
        sendTmn6(false, false);
    }
    else if (tmn == 7)
    {
        sendTmn7(false, false);
    }
}

void TetrisCode::myTmnBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnBackData> *msg = static_cast<MessageOf<TmnBackData> *>(_msg.get());
    TmnBackData msgData = *msg->getData();
    if (msgData.stage>=stage && msgData.update == update)
    {
        stage = msgData.stage;
        nbTmnBackMsg -= 1;
    }
    if (nbTmnBackMsg == 0)
    {
        //Once all modules recieved their new position, the update of the tetramino can be done again.
        //The module that starts the update of the tetramino is on the bottom of the pixel so that it can send the position 1 to the future position 1
        //the update is started by verifying if the wanted movement is possible (for now, DOWN is the only possible movement).
        if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
        {
            if (goingDown == false) //if a quicker down translation is asked, the pause is skipped
            {
                usleep(700000);
            }
            else
            {
                goingDown = false;
            }

            if (goingRight)
            {
                movement = GO_RIGHT;
                goingRight = false;
            }
            else if (goingLeft)
            {
                movement = GO_LEFT;
                goingLeft = false;
            }
            else if (turnCK)
            {
                movement = ROT_CK;
                turnCK = false;
            }
            else if (turnCounterCK)
            {
                movement = ROT_COUNTER_CK;
                turnCounterCK = false;
            }
            else
            {
                movement = DOWN;
            }
            if (tmn == 1)
            {
                verifTmn1();
            }
            else if (tmn == 2)
            {
                verifTmn2();
            }
            else if (tmn == 3)
            {
                verifTmn3();
            }
            else if (tmn == 4)
            {
                verifTmn4();
            }
            else if (tmn == 5)
            {
                verifTmn5();
            }
            else if (tmn == 6)
            {
                verifTmn6();
            }
            else if (tmn == 7)
            {
                verifTmn7();
            }
        }
        else if (parent != nullptr && parent->isConnected())
        {
            TmnBackData data = TmnBackData(stage, update);
            sendMessage("Tmn Back Message Parent", new MessageOf<TmnBackData>(TMNBACK_MSG_ID, data), parent, 0, 0);
        }
    }
}

void TetrisCode::myReinitPixMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<ReinitData> *msg = static_cast<MessageOf<ReinitData> *>(_msg.get());
    ReinitData msgData = *msg->getData();
    if (msgData.stage >= stage && msgData.tmn == tmn && msgData.id > nbReinit)
    {
        //The message that a reinitialization may be needed is spread through the current tetramino by
        //the function `sendTmn*(true,msgData.movement)`. Then the module calculates if it needs a
        //reinitialization (init = true if yes)

        stage = msgData.stage;
        nbReinit = msgData.id;
        parent = sender;
        init = false;
        movement = msgData.movement;
        if (msgData.tmn == 1)
        {
            sendTmn1(true, false);
            //the first tetramino doesn't rotate, it goes down no matter what
            //the pixels that were in positions 1 and 2 don't belong to the tetramino anymore, they have to be re-initialized
            if (msgData.movement == DOWN && (position == 1 || position == 2))
            {
                init = true;
            }
            else if (msgData.movement == GO_RIGHT && (position == 1 || position == 3))
            {
                init = true;
            }
            else if (msgData.movement == GO_LEFT && (position == 2 || position == 4))
            {
                init = true;
            }
        }
        else if (msgData.tmn == 2)
        {
            sendTmn2(true, false);
            if (msgData.movement == DOWN || msgData.movement == GO_RIGHT || msgData.movement == GO_LEFT)
            {
                int rot1 = 0;
                int rot2 = 0;
                int rot3 = 0;
                int rot4 = 0;
                if (msgData.movement == DOWN)
                {
                    rot1 = NORTH;
                    rot2 = EAST;
                    rot3 = SOUTH;
                    rot4 = WEST;
                }
                else if (msgData.movement == GO_RIGHT)
                {
                    rot1 = WEST;
                    rot2 = NORTH;
                    rot3 = EAST;
                    rot4 = SOUTH;
                }
                else if (msgData.movement == GO_LEFT)
                {
                    rot1 = EAST;
                    rot2 = SOUTH;
                    rot3 = WEST;
                    rot4 = NORTH;
                }
                //When the tetramino 2 goes down, if it is upright, only the pixel 2 doesn't belong to the tetramino anymore.
                if (rotation == rot1 && position == 2)
                {
                    init = true;
                }
                else if (rotation == rot4 || rotation == rot2)
                {
                    init = true;
                }
                else if (rotation == rot3 && position == 4)
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
            sendTmn3(true, false);
            if (msgData.movement == DOWN || msgData.movement == GO_RIGHT || msgData.movement == GO_LEFT)
            {
                int rot1 = 0;
                int rot2 = 0;
                int rot3 = 0;
                int rot4 = 0;
                if (msgData.movement == DOWN)
                {
                    rot1 = NORTH;
                    rot2 = EAST;
                    rot3 = SOUTH;
                    rot4 = WEST;
                }
                else if (msgData.movement == GO_RIGHT)
                {
                    rot1 = WEST;
                    rot2 = NORTH;
                    rot3 = EAST;
                    rot4 = SOUTH;
                }
                else if (msgData.movement == GO_LEFT)
                {
                    rot1 = EAST;
                    rot2 = SOUTH;
                    rot3 = WEST;
                    rot4 = NORTH;
                }
                if (rotation == rot1 && (position == 2 || position == 4))
                {
                    init = true;
                }
                else if (rotation == rot4 && position != 3)
                {
                    init = true;
                }
                else if (rotation == rot3 && (position == 3 || position == 4))
                {
                    init = true;
                }
                else if (rotation == rot2 && position != 4)
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
            sendTmn4(true, false);
            if (msgData.movement == DOWN || msgData.movement == GO_RIGHT || msgData.movement == GO_LEFT)
            {
                int rot1 = 0;
                int rot2 = 0;
                int rot3 = 0;
                int rot4 = 0;
                if (msgData.movement == DOWN)
                {
                    rot1 = NORTH;
                    rot2 = EAST;
                    rot3 = SOUTH;
                    rot4 = WEST;
                }
                else if (msgData.movement == GO_RIGHT)
                {
                    rot1 = WEST;
                    rot2 = NORTH;
                    rot3 = EAST;
                    rot4 = SOUTH;
                }
                else if (msgData.movement == GO_LEFT)
                {
                    rot1 = EAST;
                    rot2 = SOUTH;
                    rot3 = WEST;
                    rot4 = NORTH;
                }
                if (rotation == rot1 && (position == 2 || position == 4))
                {
                    init = true;
                }
                else if (rotation == rot4 && position != 4)
                {
                    init = true;
                }
                else if (rotation == rot3 && (position == 3 || position == 4))
                {
                    init = true;
                }
                else if (rotation == rot2 && position != 3)
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
            sendTmn5(true, false);
            if (msgData.movement == DOWN || msgData.movement == GO_RIGHT || msgData.movement == GO_LEFT)
            {
                int rot1 = 0;
                int rot2 = 0;
                int rot3 = 0;
                int rot4 = 0;
                if (msgData.movement == DOWN)
                {
                    rot1 = NORTH;
                    rot2 = EAST;
                    rot3 = SOUTH;
                    rot4 = WEST;
                }
                else if (msgData.movement == GO_RIGHT)
                {
                    rot1 = WEST;
                    rot2 = NORTH;
                    rot3 = EAST;
                    rot4 = SOUTH;
                }
                else if (msgData.movement == GO_LEFT)
                {
                    rot1 = EAST;
                    rot2 = SOUTH;
                    rot3 = WEST;
                    rot4 = NORTH;
                }
                if (rotation == rot1 && position != 1)
                {
                    init = true;
                }
                else if (rotation == rot4 && (position == 3 || position == 4))
                {
                    init = true;
                }
                else if (rotation == rot3 && position != 3)
                {
                    init = true;
                }
                else if (rotation == rot2 && (position == 2 || position == 3))
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
            sendTmn6(true, false);
            if (msgData.movement == DOWN || msgData.movement == GO_RIGHT || msgData.movement == GO_LEFT)
            {
                int rot1 = 0;
                int rot2 = 0;
                int rot3 = 0;
                int rot4 = 0;
                if (msgData.movement == DOWN)
                {
                    rot1 = NORTH;
                    rot2 = EAST;
                    rot3 = SOUTH;
                    rot4 = WEST;
                }
                else if (msgData.movement == GO_RIGHT)
                {
                    rot1 = WEST;
                    rot2 = NORTH;
                    rot3 = EAST;
                    rot4 = SOUTH;
                }
                else if (msgData.movement == GO_LEFT)
                {
                    rot1 = EAST;
                    rot2 = SOUTH;
                    rot3 = WEST;
                    rot4 = NORTH;
                }

                if (rotation == rot1 && (position == 2 || position == 3))
                {
                    init = true;
                }
                else if (rotation == rot4 && position != 1)
                {
                    init = true;
                }
                else if (rotation == rot3 && (position == 1 || position == 4))
                {
                    init = true;
                }
                else if (rotation == rot2 && position != 3)
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
            sendTmn7(true, false);
            if (msgData.movement == DOWN || msgData.movement == GO_RIGHT || msgData.movement == GO_LEFT)
            {
                int rot1 = 0;
                int rot2 = 0;
                int rot3 = 0;
                int rot4 = 0;
                if (msgData.movement == DOWN)
                {
                    rot1 = NORTH;
                    rot2 = EAST;
                    rot3 = SOUTH;
                    rot4 = WEST;
                }
                else if (msgData.movement == GO_RIGHT)
                {
                    rot1 = WEST;
                    rot2 = NORTH;
                    rot3 = EAST;
                    rot4 = SOUTH;
                }
                else if (msgData.movement == GO_LEFT)
                {
                    rot1 = EAST;
                    rot2 = SOUTH;
                    rot3 = WEST;
                    rot4 = NORTH;
                }
                if (rotation == rot1 && (position == 2 || position == 3))
                {
                    init = true;
                }
                else if (rotation == rot2 && position != 1)
                {
                    init = true;
                }
                else if (rotation == rot3 && (position == 1 || position == 4))
                {
                    init = true;
                }
                else if (rotation == rot4 && position != 3)
                {
                    init = true;
                }
            }
            else if (msgData.movement == ROT_CK && position != 1)
            {
                init = true;
            }
            else if (msgData.movement == ROT_COUNTER_CK && position != 1)
            {
                init = true;
            }
        }

        if (nbReinitBackMsg == 0 && parent != nullptr && parent->isConnected())
        {
            ReinitBackData data = ReinitBackData(stage, nbReinit);
            sendMessage("Reinit Back Message Parent", new MessageOf<ReinitBackData>(REINITBACK_MSG_ID, data), parent, 0, 0);

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
        ReinitBackData data = ReinitBackData(stage, nbReinit);
        sendMessage("Reinit Back Message", new MessageOf<ReinitBackData>(REINITBACK_MSG_ID, data), sender, 0, 0);
    }
}

void TetrisCode::myReinitBackMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<ReinitBackData> *msg = static_cast<MessageOf<ReinitBackData> *>(_msg.get());
    ReinitBackData msgData = *msg->getData();

    if (msgData.stage >= stage && msgData.nbReinit == nbReinit)
    {
        stage = msgData.stage;
        nbReinitBackMsg -= 1;
    }
    if (nbReinitBackMsg == 0)
    {
        //The module that starts the update of the tetramino is on the bottom of the pixel so that it can send the position 1 to the future position 1
        if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
        {
            if (movement == DOWN || movement == GO_LEFT || movement == GO_RIGHT)
            {
                TmnData data = TmnData(stage, update, rotation, position, color, nbReinit, nbFBack, goingRight, goingLeft, turnCK, turnCounterCK);
                data.nbupdate += 1;
                P2PNetworkInterface *i = nullptr;
                leaderBlockCode = nullptr;
                if (movement == DOWN)
                {
                    i = bottomItf;
                }
                else if (movement == GO_RIGHT)
                {
                    i = rightItf;
                }
                else if (movement == GO_LEFT)
                {
                    i = leftItf;
                }
                else
                {
                    i = bottomItf;
                }
                if (tmn == 1)
                {
                    if (i != nullptr && i->isConnected())
                    {
                        sendMessage("Update Tmn 1 Message", new MessageOf<TmnData>(START_TMN1_MSG_ID, data), i, 0, 0);
                    }
                }
                else if (tmn == 2)
                {
                    if (i != nullptr && i->isConnected())
                    {
                        sendMessage("Update Tmn 2 Message", new MessageOf<TmnData>(START_TMN2_MSG_ID, data), i, 0, 0);
                    }
                }
                else if (tmn == 3)
                {
                    if (i != nullptr && i->isConnected())
                    {
                        sendMessage("Update Tmn 3 Message", new MessageOf<TmnData>(START_TMN3_MSG_ID, data), i, 0, 0);
                    }
                }
                else if (tmn == 4)
                {
                    if (i != nullptr && i->isConnected())
                    {
                        sendMessage("Update Tmn 4 Message", new MessageOf<TmnData>(START_TMN4_MSG_ID, data), i, 0, 0);
                    }
                }
                else if (tmn == 5)
                {
                    if (i != nullptr && i->isConnected())
                    {
                        sendMessage("Update Tmn 5 Message", new MessageOf<TmnData>(START_TMN5_MSG_ID, data), i, 0, 0);
                    }
                }
                else if (tmn == 6)
                {
                    if (i != nullptr && i->isConnected())
                    {
                        sendMessage("Update Tmn 6 Message", new MessageOf<TmnData>(START_TMN6_MSG_ID, data), i, 0, 0);
                    }
                }
                else if (tmn == 7)
                {
                    if (i != nullptr && i->isConnected())
                    {
                        sendMessage("Update Tmn 7 Message", new MessageOf<TmnData>(START_TMN7_MSG_ID, data), i, 0, 0);
                    }
                }

                //Reinitialization if needed : the deciding module never recieved the reinitpixmsg (it sent it)
                //so it never calculated if it needs reinitialization
                // int rot1 = 0; // non used in the end
                int rot2 = 0;
                int rot3 = 0;
                int rot4 = 0;
                if (movement == DOWN)
                {
                    // rot1 = NORTH;
                    rot2 = EAST;
                    rot3 = SOUTH;
                    rot4 = WEST;
                }
                else if (movement == GO_RIGHT)
                {
                    // rot1 = WEST;
                    rot2 = NORTH;
                    rot3 = EAST;
                    rot4 = SOUTH;
                }
                else if (movement == GO_LEFT)
                {
                    //rot1 = EAST;
                    rot2 = SOUTH;
                    rot3 = WEST;
                    rot4 = NORTH;
                }
                if ((tmn == 1 && (movement == GO_RIGHT || movement == DOWN)) ||
                    ((tmn == 2 || tmn == 3 || tmn == 4) && (rotation == rot2 || rotation == rot4)) ||
                    (tmn == 5 && rotation == rot3) ||
                    (tmn == 6 && (rotation == rot3 || rotation == rot2)) ||
                    (tmn == 7 && (rotation == rot3 || rotation == rot4)))
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
                    nbTmnBackMsg = 0;
                    module->setColor(Colors[color]);
                }
            }
            else if (movement == ROT_CK || movement == ROT_COUNTER_CK)
            {
                update += 1;
                if (movement == ROT_CK)
                {
                    if (rotation == NORTH)
                    {
                        rotation = EAST;
                    }
                    else if (rotation == EAST)
                    {
                        rotation = SOUTH;
                    }
                    else if (rotation == SOUTH)
                    {
                        rotation = WEST;
                    }
                    else if (rotation == WEST)
                    {
                        rotation = NORTH;
                    }
                }
                else if (movement == ROT_COUNTER_CK)
                {
                    if (rotation == NORTH)
                    {
                        rotation = WEST;
                    }
                    else if (rotation == EAST)
                    {
                        rotation = NORTH;
                    }
                    else if (rotation == SOUTH)
                    {
                        rotation = EAST;
                    }
                    else if (rotation == WEST)
                    {
                        rotation = SOUTH;
                    }
                }
                if (tmn == 1)
                {
                    sendTmn1(false, false);
                }
                else if (tmn == 2)
                {
                    sendTmn2(false, false);
                }
                else if (tmn == 3)
                {
                    sendTmn3(false, false);
                }
                else if (tmn == 4)
                {
                    sendTmn4(false, false);
                }
                else if (tmn == 5)
                {
                    sendTmn5(false, false);
                }
                else if (tmn == 6)
                {
                    sendTmn6(false, false);
                }
                else if (tmn == 7)
                {
                    sendTmn7(false, false);
                }
            }
        }
        else if (parent != nullptr && parent->isConnected())
        {
            ReinitBackData data = ReinitBackData(stage, nbReinit);
            sendMessage("Reinit Back Message Parent", new MessageOf<ReinitBackData>(REINITBACK_MSG_ID, data), parent, 0, 0);
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
}

void TetrisCode::updateOfTmn()
{
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
        sendTmn1(true, false);
    }
    if (tmn == 2)
    {
        sendTmn2(true, false);
    }
    if (tmn == 3)
    {
        sendTmn3(true, false);
    }
    if (tmn == 4)
    {
        sendTmn4(true, false);
    }
    if (tmn == 5)
    {
        sendTmn5(true, false);
    }
    if (tmn == 6)
    {
        sendTmn6(true, false);
    }
    if (tmn == 7)
    {
        sendTmn7(true, false);
    }
}

void TetrisCode::myIsFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<isFreeData> *msg = static_cast<MessageOf<isFreeData> *>(_msg.get());
    isFreeData msgData = *msg->getData();

    if (msgData.stage >= stage && msgData.id > nbFree)
    {
        stage = msgData.stage;
        nbFree = msgData.id;
        bool b = false;
        P2PNetworkInterface *i = nullptr;
        if (msgData.direction == SOUTH) //to avoid that the verification is made twice, only the top right corner and the bottom lefet corner do the verifications.
        {
            i = bottomItf;
            b = roleInPixel == BOTTOM_LEFT_CORNER /*|| roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_RIGHT_CORNER */ || roleInPixel == ALONE;
        }
        else if (msgData.direction == EAST)
        {
            i = rightItf;
            b = roleInPixel == TOP_RIGHT_CORNER /*|| roleInPixel == RIGHT_BORDER || roleInPixel == BOTTOM_RIGHT_CORNER */ || roleInPixel == ALONE;
        }
        else if (msgData.direction == WEST)
        {
            i = leftItf;
            b = roleInPixel == /*TOP_LEFT_CORNER || roleInPixel == LEFT_BORDER || roleInPixel ==*/BOTTOM_LEFT_CORNER || roleInPixel == ALONE;
        }
        else if (msgData.direction == NORTH)
        {
            i = topItf;
            b = roleInPixel == TOP_RIGHT_CORNER /*|| roleInPixel == TOP_BORDER || roleInPixel == TOP_LEFT_CORNER */ || roleInPixel == ALONE;
        }

        if (position != msgData.position || !b) //if this module has to spread the verification
        {
            console << "sending verification : nb = " << msgData.id << " pos = " << msgData.position << " dir = " << msgData.direction << "\n";
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
            if (i != nullptr && i->isConnected())
            {
                sendMessage("Asking Free Message", new MessageOf<int>(FREEMSG_ID, stage), i, 0, 0);
            }
            else //if there is no module, it means that it is not free -> the answer is false, directly
            {
                nbFBack = nbFree + 1;
                isFreeData data = isFreeData(stage, nbFBack, position, msgData.direction, false);
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
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (msgData >= stage)
    {
        stage = msgData;
        //if the module doesn't belong to any tetramino, it is free
        if (tmn == NO_TMN && sender != nullptr && sender->isConnected())
        {
            BackFreeData data = BackFreeData(stage,true);
            sendMessage("Answer Free Message", new MessageOf<BackFreeData>(BFMSG_ID, data), sender, 0, 0);
        }
        else if (sender != nullptr && sender->isConnected())
        {
            BackFreeData data = BackFreeData(stage,false);
            sendMessage("Answer Free Message", new MessageOf<BackFreeData>(BFMSG_ID, data), sender, 0, 0);
        }
    }
}

void TetrisCode::myBackFreeMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<isFreeData> *msg = static_cast<MessageOf<isFreeData> *>(_msg.get());
    isFreeData msgData = *msg->getData();

    if (msgData.stage >= stage && position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
    {
        stage = msgData.stage;
        console << "recieved verification answer : pos = " << msgData.position << " dir = " << msgData.direction << " answer = " << msgData.answer << "\n";
        if (verifications.size() > 0) //if the vector is empty, all verification alreday have been recieved
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
                        isFreeData data = isFreeData(stage, nbFree, f.position, f.direction);
                        console << "sending new verification : pos = " << f.position << " dir = " << f.direction << " nb = " << nbFree << "\n";
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
                    verifications.clear();
                    if (movement != DOWN) //if the movement cannot be done, the tetramino may still be able to go down
                    {
                        movement = DOWN;
                        if (tmn == 1)
                        {
                            verifTmn1();
                        }
                        else if (tmn == 2)
                        {
                            verifTmn2();
                        }
                        else if (tmn == 3)
                        {
                            verifTmn3();
                        }
                        else if (tmn == 4)
                        {
                            verifTmn4();
                        }
                        else if (tmn == 5)
                        {
                            verifTmn5();
                        }
                        else if (tmn == 6)
                        {
                            verifTmn6();
                        }
                        else if (tmn == 7)
                        {
                            verifTmn7();
                        }
                    }
                    else
                    {
                        blocked = true;
                        if (tmn == 1)
                        {
                            sendTmn1(false, true);
                        }
                        else if (tmn == 2)
                        {
                            sendTmn2(false, true);
                        }
                        else if (tmn == 3)
                        {
                            sendTmn3(false, true);
                        }
                        else if (tmn == 4)
                        {
                            sendTmn4(false, true);
                        }
                        else if (tmn == 5)
                        {
                            sendTmn5(false, true);
                        }
                        else if (tmn == 6)
                        {
                            sendTmn6(false, true);
                        }
                        else if (tmn == 7)
                        {
                            sendTmn7(false, true);
                        }
                        if (leftItf != nullptr && leftItf->isConnected())
                        {
                            BlockedData data = BlockedData(stage, nbLinesReinit, 1);
                            sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), leftItf, 0, 0);
                        }
                        if (rightItf != nullptr && rightItf->isConnected())
                        {
                            BlockedData data = BlockedData(stage, nbLinesReinit, 1);
                            sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), rightItf, 0, 0);
                        }
                        nbTmn += 1;
                        NewTmnData data = NewTmnData(stage, nbTmn);
                        sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<NewTmnData>(NEWTMNMSG_ID, data), 0, 0, 0);
                    }
                }
            }
        }
    }
    else if (msgData.stage >= stage && msgData.id > nbFBack) // if this module is not the deciding module, it has to spread the answer.
    {
        stage = msgData.stage;
        nbFBack = msgData.id;
        console << " send back verification : nb = " << msgData.id << " pos = " << msgData.position << " dir = " << msgData.direction << " answer = " << msgData.answer << "\n";
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
    MessageOf<BackFreeData> *msg = static_cast<MessageOf<BackFreeData> *>(_msg.get());
    BackFreeData msgData = *msg->getData();

    if (msgData.stage >= stage && position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)) //if it is a direct verification by the deciding module
    {
        stage = msgData.stage;
        if (msgData.answer) //if the verified pixel is free
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
                isFreeData data = isFreeData(stage, nbFree, f.position, f.direction);
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
            if (movement != DOWN) //if the movement cannot be done, the tetramino may still be able to go down
            {
                movement = DOWN;
                if (tmn == 1)
                {
                    verifTmn1();
                }
                else if (tmn == 2)
                {
                    verifTmn2();
                }
                else if (tmn == 3)
                {
                    verifTmn3();
                }
                else if (tmn == 4)
                {
                    verifTmn4();
                }
                else if (tmn == 5)
                {
                    verifTmn5();
                }
                else if (tmn == 6)
                {
                    verifTmn6();
                }
                else if (tmn == 7)
                {
                    verifTmn7();
                }
            }
            else
            {
                blocked = true;
                if (tmn == 1)
                {
                    sendTmn1(false, true);
                }
                else if (tmn == 2)
                {
                    sendTmn2(false, true);
                }
                else if (tmn == 3)
                {
                    sendTmn3(false, true);
                }
                else if (tmn == 4)
                {
                    sendTmn4(false, true);
                }
                else if (tmn == 5)
                {
                    sendTmn5(false, true);
                }
                else if (tmn == 6)
                {
                    sendTmn6(false, true);
                }
                else if (tmn == 7)
                {
                    sendTmn7(false, true);
                }
                if (leftItf != nullptr && leftItf->isConnected())
                {
                    BlockedData data = BlockedData(stage, nbLinesReinit, 1);
                    sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), leftItf, 0, 0);
                }
                if (rightItf != nullptr && rightItf->isConnected())
                {
                    BlockedData data = BlockedData(stage, nbLinesReinit, 1);
                    sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), rightItf, 0, 0);
                }
                nbTmn += 1;
                NewTmnData data = NewTmnData(stage, nbTmn);
                sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<NewTmnData>(NEWTMNMSG_ID, data), 0, 0, 0);
            }
        }
    }
    else if (msgData.stage >= stage) //if the module is not the deciding module, it has to send the answer.
    {
        stage = msgData.stage;
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
        if (msgData.answer)
        {
            result = FREE;
        }
        else if (!msgData.answer)
        {
            result = OCCUPIED;
        }

        //sending the answer
        isFreeData data = isFreeData(stage, nbFBack, position, direction, result);
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

void TetrisCode::sendFarVerif(farVerif verif, P2PNetworkInterface *sender)
{
    if (verif.rotation == NORTH)
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
    else if (verif.rotation == EAST)
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
    else if (verif.rotation == SOUTH)
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
    else if (verif.rotation == WEST)
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

    int cur_dir = verif.directions.at(verif.current_dir);
    P2PNetworkInterface *i = itf[northId];
    if (cur_dir == NORTH)
    {
        if (northBool)
        {
            if (i != nullptr && i->isConnected())
            {
                verif.current_dir += 1;
                sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
            }
            else if (sender != nullptr && sender->isConnected()) //if the target module isn't connected, it means that it is not free
            {
                verif.answer = OCCUPIED;
                nbFBack = nbFree + 1;
                verif.id = nbFBack;
                sendMessage("Far Verif Back", new MessageOf<farVerif>(BACK_FAR_V_MSG_ID, verif), sender, 0, 0);
            }
            else if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)) //if sender == nullptr, mybe this module is directly the deciding module : it has to start the next verification.
            {
                farVerifications.pop_back();
                if (farVerifications.size() == 0)
                {
                    //if there isn't any verification left, it means that all pixels were free -> the update of the tetramino can go on.
                    updateOfTmn();
                }
                else //send the next verification
                {
                    farVerif v = farVerifications.back();
                    nbFree += 1;
                    v.id = nbFree;
                    sendFarVerif(v, nullptr);
                }
            }
        }
        else if (i != nullptr && i->isConnected())
        {
            sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
        }
    }
    else if (cur_dir == EAST)
    {
        i = itf[eastId];
        if (eastBool)
        {
            if (i != nullptr && i->isConnected())
            {
                verif.current_dir += 1;
                sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
            }
            else if (sender != nullptr && sender->isConnected()) //if the target module isn't connected, it means that it is not free
            {
                verif.answer = OCCUPIED;
                nbFBack = nbFree + 1;
                verif.id = nbFBack;
                sendMessage("Far Verif Back", new MessageOf<farVerif>(BACK_FAR_V_MSG_ID, verif), sender, 0, 0);
            }
            else if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)) //if sender == nullptr, mybe this module is directly the deciding module : it has to start the next verification.
            {
                farVerifications.pop_back();
                if (farVerifications.size() == 0)
                {
                    //if there isn't any verification left, it means that all pixels were free -> the update of the tetramino can go on.
                    updateOfTmn();
                }
                else //send the next verification
                {
                    farVerif v = farVerifications.back();
                    nbFree += 1;
                    v.id = nbFree;
                    sendFarVerif(v, nullptr);
                }
            }
        }
        else if (i != nullptr && i->isConnected())
        {
            sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
        }
    }
    else if (cur_dir == SOUTH)
    {
        i = itf[southId];
        if (southBool)
        {
            if (i != nullptr && i->isConnected())
            {
                verif.current_dir += 1;
                sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
            }
            else if (sender != nullptr && sender->isConnected()) //if the target module isn't connected, it means that it is not free
            {
                verif.answer = OCCUPIED;
                nbFBack = nbFree + 1;
                verif.id = nbFBack;
                sendMessage("Far Verif Back", new MessageOf<farVerif>(BACK_FAR_V_MSG_ID, verif), sender, 0, 0);
            }
            else if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)) //if sender == nullptr, mybe this module is directly the deciding module : it has to start the next verification.
            {
                farVerifications.pop_back();
                if (farVerifications.size() == 0)
                {
                    //if there isn't any verification left, it means that all pixels were free -> the update of the tetramino can go on.
                    updateOfTmn();
                }
                else //send the next verification
                {
                    farVerif v = farVerifications.back();
                    nbFree += 1;
                    v.id = nbFree;
                    sendFarVerif(v, nullptr);
                }
            }
        }
        else if (i != nullptr && i->isConnected())
        {
            sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
        }
    }
    else if (cur_dir == WEST)
    {
        i = itf[westId];
        if (westBool)
        {
            if (i != nullptr && i->isConnected())
            {
                verif.current_dir += 1;
                sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
            }
            else if (sender != nullptr && sender->isConnected()) //if the target module isn't connected, it means that it is not free
            {
                verif.answer = OCCUPIED;
                nbFBack = nbFree + 1;
                verif.id = nbFBack;
                sendMessage("Far Verif Back", new MessageOf<farVerif>(BACK_FAR_V_MSG_ID, verif), sender, 0, 0);
            }
            else if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)) //if sender == nullptr, mybe this module is directly the deciding module : it has to start the next verification.
            {
                farVerifications.pop_back();
                if (farVerifications.size() == 0)
                {
                    //if there isn't any verification left, it means that all pixels were free -> the update of the tetramino can go on.
                    updateOfTmn();
                }
                else //send the next verification
                {
                    farVerif v = farVerifications.back();
                    nbFree += 1;
                    v.id = nbFree;
                    sendFarVerif(v, nullptr);
                }
            }
        }
        else if (i != nullptr && i->isConnected())
        {
            sendMessage("Far Verif", new MessageOf<farVerif>(FAR_VERIF_MSG_ID, verif), i, 0, 0);
        }
    }
}

void TetrisCode::myFarVerifMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<farVerif> *msg = static_cast<MessageOf<farVerif> *>(_msg.get());
    farVerif msgData = *msg->getData();
    if (msgData.stage >= stage && msgData.id > nbFree)
    {
        stage = msgData.stage;
        nbFree = msgData.id;
        if (tmn == PIXEL_NON_VALID) // If the module belongs to a non valid pixel, the rotation isn't possible.
        {
            msgData.answer = OCCUPIED;
            msgData.current_dir -= 1;
            nbBackMsg = nbFree + 1;
            if (sender != nullptr && sender->isConnected())
            {
                sendMessage("Far Verif Back", new MessageOf<farVerif>(BACK_FAR_V_MSG_ID, msgData), sender, 0, 0);
            }
        }
        else if (msgData.current_dir >= msgData.directions.size()) // if the current direction is out of bounds, this module belongs to the verified pixel
        {
            if (tmn == NO_TMN)
            {
                msgData.answer = FREE;
            }
            else
            {
                msgData.answer = OCCUPIED;
            }
            msgData.current_dir -= 2;
            nbBackMsg = nbFree + 1;
            if (sender != nullptr && sender->isConnected())
            {
                sendMessage("Far Verif Back", new MessageOf<farVerif>(BACK_FAR_V_MSG_ID, msgData), sender, 0, 0);
            }
        }
        else //otherwise, this module needs to spread the verification in the right direction.
        {
            parent = sender;
            console << "recieves far verif to send : dir = " << msgData.current_dir << "\n";
            sendFarVerif(msgData, sender);
        }
    }
}

void TetrisCode::myBackFarVMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<farVerif> *msg = static_cast<MessageOf<farVerif> *>(_msg.get());
    farVerif msgData = *msg->getData();

    if (msgData.stage >= stage && msgData.id > nbFBack)
    {
        stage = msgData.stage;
        nbFBack = msgData.id;

        if (position == 1 && (roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE)) // if this module is the deciding module
        {

            if (msgData.answer == FREE) // if the answer is free, the verifications can continue.
            //Otherwise, the tetramino is stuck, and an other one has to be started.
            {
                farVerifications.pop_back();
                if (farVerifications.size() == 0)
                {
                    //if there isn't any verification left, it means that all pixels were free -> the update of the tetramino can go on.
                    updateOfTmn();
                }
                else //send the next verification
                {
                    farVerif v = farVerifications.back();
                    nbFree += 1;
                    v.id = nbFree;
                    sendFarVerif(v, nullptr);
                }
            }
            else // if one verification is false, the movement cannot be done. A new tetramino is started
            {
                if (movement != DOWN) //if the movement cannot be done, the tetramino may still be able to go down
                {
                    movement = DOWN;
                    if (tmn == 1)
                    {
                        verifTmn1();
                    }
                    else if (tmn == 2)
                    {
                        verifTmn2();
                    }
                    else if (tmn == 3)
                    {
                        verifTmn3();
                    }
                    else if (tmn == 4)
                    {
                        verifTmn4();
                    }
                    else if (tmn == 5)
                    {
                        verifTmn5();
                    }
                    else if (tmn == 6)
                    {
                        verifTmn6();
                    }
                    else if (tmn == 7)
                    {
                        verifTmn7();
                    }
                }
                else
                {
                    blocked = true;
                    if (tmn == 1)
                    {
                        sendTmn1(false, true);
                    }
                    else if (tmn == 2)
                    {
                        sendTmn2(false, true);
                    }
                    else if (tmn == 3)
                    {
                        sendTmn3(false, true);
                    }
                    else if (tmn == 4)
                    {
                        sendTmn4(false, true);
                    }
                    else if (tmn == 5)
                    {
                        sendTmn5(false, true);
                    }
                    else if (tmn == 6)
                    {
                        sendTmn6(false, true);
                    }
                    else if (tmn == 7)
                    {
                        sendTmn7(false, true);
                    }
                    if (leftItf != nullptr && leftItf->isConnected())
                    {
                        sendMessage("Counting blocked neighbors", new MessageOf<int>(COUNT_BCK_MSG_ID, 1), leftItf, 0, 0);
                    }
                    if (rightItf != nullptr && rightItf->isConnected())
                    {
                        sendMessage("Counting blocked neighbors", new MessageOf<int>(COUNT_BCK_MSG_ID, 1), rightItf, 0, 0);
                    }
                    nbTmn += 1;
                    sendMessageToAllNeighbors("New Tetramino Message", new MessageOf<int>(NEWTMNMSG_ID, nbTmn), 0, 0, 0);
                }
            }
        }
        else //otherwise, this module needs to spread the answer in the right direction.
        {

            if (parent != nullptr && parent->isConnected())
            {
                sendMessage("Far Verif Back", new MessageOf<farVerif>(BACK_FAR_V_MSG_ID, msgData), parent, 0, 0);
            }
        }
    }
}

void TetrisCode::myBlockedMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (msgData >= stage && blocked == false)
    {
        stage = msgData;
        blocked = true;
        blockedLeft = 0;
        blockedRight = 0;
        if (tmn == 1)
        {
            sendTmn1(false, true);
        }
        else if (tmn == 2)
        {
            sendTmn2(false, true);
        }
        else if (tmn == 3)
        {
            sendTmn3(false, true);
        }
        else if (tmn == 4)
        {
            sendTmn4(false, true);
        }
        else if (tmn == 5)
        {
            sendTmn5(false, true);
        }
        else if (tmn == 6)
        {
            sendTmn6(false, true);
        }
        else if (tmn == 7)
        {
            sendTmn7(false, true);
        }
        BlockedData data = BlockedData(stage, nbLinesReinit, 1);
        if (leftItf != nullptr && leftItf->isConnected())
        {
            sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), leftItf, 0, 0);
        }
        if (rightItf != nullptr && rightItf->isConnected())
        {
            sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), rightItf, 0, 0);
        }
    }
}

void TetrisCode::myCountBlockedMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<BlockedData> *msg = static_cast<MessageOf<BlockedData> *>(_msg.get());
    BlockedData msgData = *msg->getData();

    if (msgData.stage >= stage && msgData.i == nbLinesReinit && blocked == true && blockedLeft + blockedRight + 1 != totalBckdModules) // only the blocked modules are counted, and if the line already is full, nothing should happen.
    {
        stage = msgData.stage;
        if (sender == leftItf && msgData.n > blockedLeft)
        {
            blockedLeft = msgData.n;
            //sending the update to both neighbors
            if (rightItf != nullptr && rightItf->isConnected())
            {
                BlockedData data = BlockedData(stage, nbLinesReinit, blockedLeft + 1);
                sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), rightItf, 0, 0);
            }
            if (leftItf != nullptr && leftItf->isConnected())
            {
                BlockedData data = BlockedData(stage, nbLinesReinit, blockedRight + 1);
                sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), leftItf, 0, 0);
            }
        }
        else if (sender == rightItf && msgData.n > blockedRight)
        {
            blockedRight = msgData.n;
            if (leftItf != nullptr && leftItf->isConnected())
            {
                BlockedData data = BlockedData(stage, nbLinesReinit, blockedRight + 1);
                sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), leftItf, 0, 0);
            }
            if (rightItf != nullptr && rightItf->isConnected())
            {
                BlockedData data = BlockedData(stage, nbLinesReinit, blockedLeft + 1);
                sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), rightItf, 0, 0);
            }
        }

        //if the line is full, the module asks for the information of the upper line.
        int v = blockedLeft + blockedRight + 1;
        if (v == totalBckdModules && (roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE) && topItf != nullptr && topItf->isConnected())
        {
            sendMessage("Asking line Tmn Info", new MessageOf<int>(ASK_INFO_MSG_ID, stage), topItf, 0, 0);
        }
    }
}

void TetrisCode::myAskTmnInfoMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (msgData >= stage && (roleInPixel == BOTTOM_LEFT_CORNER || roleInPixel == BOTTOM_BORDER || roleInPixel == BOTTOM_RIGHT_CORNER || roleInPixel == ALONE))
    //if the module has to answer
    {
        stage = msgData;
        TmnInfo data = TmnInfo(stage, tmn, rotation, position, color, blocked, blockedRight, blockedLeft);
        if (bottomItf != nullptr && bottomItf->isConnected())
        {
            sendMessage("Sending Tmn Info", new MessageOf<TmnInfo>(TMN_INFO_MSG_ID, data), bottomItf, 0, 0);
        }
        if (tmn != NO_TMN && topItf != nullptr && topItf->isConnected()) //if the pixel isn't empty, the upper line needs to come down
        {
            sendMessage("Asking line Tmn Info", new MessageOf<int>(ASK_INFO_MSG_ID,stage), topItf, 0, 0);
        }
    }
    else
    {
        //if the module is in contact with the next line, it needs to send the demand only once
        if (roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_BORDER || roleInPixel == TOP_RIGHT_CORNER)
        {
            if (blockedLeft + blockedRight + 1 != totalBckdModules && topItf != nullptr && topItf->isConnected())
            {
                sendMessage("Asking line Tmn Info", new MessageOf<int>(ASK_INFO_MSG_ID, stage), topItf, 0, 0);
            }
        }
        else if (topItf != nullptr && topItf->isConnected())
        {
            sendMessage("Asking line Tmn Info", new MessageOf<int>(ASK_INFO_MSG_ID, stage), topItf, 0, 0);
        }
    }
}

void TetrisCode::myTmnInfoMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<TmnInfo> *msg = static_cast<MessageOf<TmnInfo> *>(_msg.get());
    TmnInfo msgData = *msg->getData();
    if (msgData.stage >= stage)
    {
        stage = msgData.stage;
        nbLinesReinit += 1;
        tmn = msgData.tmn;
        rotation = msgData.rotation;
        position = msgData.position;
        color = msgData.color;
        blocked = msgData.bckd;
        blockedRight = msgData.bckdR;
        blockedLeft = msgData.bckdL;
        update = 0;
        module->setColor(Colors[color]);
        //if the module is not at the bottom of the pixel, it has to spread the new information
        if (roleInPixel != BOTTOM_LEFT_CORNER && roleInPixel != BOTTOM_BORDER && roleInPixel != BOTTOM_RIGHT_CORNER && roleInPixel != ALONE && bottomItf != nullptr && bottomItf->isConnected())
        {
            sendMessage("Sending Tmn Info", new MessageOf<TmnInfo>(TMN_INFO_MSG_ID, msgData), bottomItf, 0, 0);
        }
        if (leftItf != nullptr && leftItf->isConnected())
        {
            BlockedData data = BlockedData(stage, nbLinesReinit, blockedRight + 1);
            sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), leftItf, 0, 0);
        }
        if (rightItf != nullptr && rightItf->isConnected())
        {
            BlockedData data = BlockedData(stage, nbLinesReinit, blockedLeft + 1);
            sendMessage("Counting blocked neighbors", new MessageOf<BlockedData>(COUNT_BCK_MSG_ID, data), rightItf, 0, 0);
        }
        //if the line is full, the module asks for the information of the upper line.
        int v = blockedLeft + blockedRight + 1;
        if (v == totalBckdModules && (roleInPixel == TOP_LEFT_CORNER || roleInPixel == TOP_BORDER || roleInPixel == TOP_RIGHT_CORNER || roleInPixel == ALONE) && topItf != nullptr && topItf->isConnected())
        {
            sendMessage("Asking line Tmn Info", new MessageOf<int>(ASK_INFO_MSG_ID, stage), topItf, 0, 0);
        }
    }
}

void TetrisCode::mySplitMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<Splitdata> *msg = static_cast<MessageOf<Splitdata> *>(_msg.get());
    Splitdata msgData = *msg->getData();

    if (msgData.stage >= stage && ((msgData.direction == EAST && (rightItf == nullptr || !rightItf->isConnected())) || (msgData.direction == WEST && (leftItf == nullptr || !leftItf->isConnected()))))
    //if the same neighbor is also missing
    {
        stage = msgData.stage;
        if (bottomItf != nullptr && bottomItf->isConnected())
        {
            sendMessage("Detected Splitting", new MessageOf<Splitdata>(SPLIT_MSG_ID, msgData), bottomItf, 0, 0);
        }
        else //if this module is the bottom module of the splitting
        {
            stringstream strstm;
            strstm << "SPLIT HANDELED";
            scheduler->trace(strstm.str(), module->blockId, GREEN);
            if (msgData.direction == WEST) // if the set is on the left of the splitting, new cooridinates have to be calculated
            {
                height = 0;
                width = 0;
                maxWidth = 0;
                stage += 1;
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
        int dir = 0;
        uint64_t neighbor = (std::static_pointer_cast<AddNeighborEvent>(pev))->face;
        P2PNetworkInterface *itf = module->getInterface(neighbor);
        if (itf == rightItf)
        {
            dir = EAST;
            console << "right neighbor missing\n";
        }
        else if (itf == leftItf)
        {
            dir = WEST;
            console << "left neighbor missing\n";
        }
        if ((topItf == nullptr || !topItf->isConnected()) && (bottomItf != nullptr && bottomItf->isConnected()))
        {
            sendMessage("Detected Splitting", new MessageOf<int>(SPLIT_MSG_ID, dir), bottomItf, 0, 0);
            module->setColor(BLUE);
        }
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
    case charGoRight:
        if (leaderBlockCode != nullptr)
        {
            leaderBlockCode->rightMvtKeyHandler();
        }
        break;
    case charGoLeft:
        if (leaderBlockCode != nullptr)
        {
            leaderBlockCode->leftMvtKeyHandler();
        }
        break;
    case charGoDown:
        if (leaderBlockCode != nullptr)
        {
            leaderBlockCode->downMvtKeyHandler();
        }
        break;
    case charTurnCK:
        if (leaderBlockCode != nullptr)
        {
            leaderBlockCode->cwRotKeyHandler();
        }
        break;
    case charTurnCCK:
        if (leaderBlockCode != nullptr)
        {
            leaderBlockCode->counterCwRotKeyHandler();
        }
        break;
    }
};

void TetrisCode::rightMvtKeyHandler()
{
    goingRight = true;
    goingLeft = false;
    goingDown = false;
    turnCK = false;
    turnCounterCK = false;
};

void TetrisCode::leftMvtKeyHandler()
{
    goingRight = false;
    goingLeft = true;
    goingDown = false;
    turnCK = false;
    turnCounterCK = false;
};

void TetrisCode::downMvtKeyHandler()
{
    goingDown = true;
    goingRight = false;
    goingLeft = false;
    turnCK = false;
    turnCounterCK = false;
}

void TetrisCode::cwRotKeyHandler()
{
    turnCK = true;
    goingLeft = false;
    goingRight = false;
    goingDown = false;
    turnCounterCK = false;
};

void TetrisCode::counterCwRotKeyHandler()
{
    turnCounterCK = true;
    goingLeft = false;
    goingRight = false;
    goingDown = false;
    turnCK = false;
};