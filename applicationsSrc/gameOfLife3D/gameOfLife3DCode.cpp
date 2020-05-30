#include "gameOfLife3DCode.hpp"
#include "utils/random.h"
#include <algorithm>
#include <unistd.h>

GameOfLife3DCode::GameOfLife3DCode(BlinkyBlocksBlock *host) : BlinkyBlocksBlockCode(host), module(host)
{
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host)
        return;

    // Registers a callback (myTopStatusFunc) to the message of type O
    addMessageEventFunc2(TOPSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopRightStatusFunc) to the message of type P
    addMessageEventFunc2(TOPRIGHTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopRightStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRightStatusFunc) to the message of type H
    addMessageEventFunc2(RIGHTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myRightStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomRightStatusFunc) to the message of type O
    addMessageEventFunc2(BOTTOMRIGHTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomRightStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomStatusFunc) to the message of type M
    addMessageEventFunc2(BOTTOMSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomLeftStatusFunc) to the message of type L
    addMessageEventFunc2(BOTTOMLEFTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomLeftStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myLeftStatusFunc) to the message of type T
    addMessageEventFunc2(LEFTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myLeftStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopLeftStatusFunc) to the message of type T
    addMessageEventFunc2(TOPLEFTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopLeftStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myFrontStatusFunc) to the message of type U
    addMessageEventFunc2(FRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopFrontStatusFunc) to the message of type A
    addMessageEventFunc2(TOPFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopRightFrontStatusFunc) to the message of type N
    addMessageEventFunc2(TOPRIGHTFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopRightFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRightFrontStatusFunc) to the message of type A
    addMessageEventFunc2(RIGHTFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myRightFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomRightFrontStatusFunc) to the message of type O
    addMessageEventFunc2(BOTTOMRIGHTFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomRightFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomFrontStatusFunc) to the message of type T
    addMessageEventFunc2(BOTTOMFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomLeftFrontStatusFunc) to the message of type S
    addMessageEventFunc2(BOTTOMLEFTFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomLeftFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myLeftFrontStatusFunc) to the message of type M
    addMessageEventFunc2(LEFTFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myLeftFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopLeftFrontStatusFunc) to the message of type S
    addMessageEventFunc2(TOPLEFTFRONTSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopLeftFrontStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBackStatusFunc) to the message of type
    addMessageEventFunc2(BACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopBackStatusFunc) to the message of type D
    addMessageEventFunc2(TOPBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopRightBackStatusFunc) to the message of type S
    addMessageEventFunc2(TOPRIGHTBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopRightBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRightBackStatusFunc) to the message of type D
    addMessageEventFunc2(RIGHTBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myRightBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomRightBackStatusFunc) to the message of type M
    addMessageEventFunc2(BOTTOMRIGHTBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomRightBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomBackStatusFunc) to the message of type
    addMessageEventFunc2(BOTTOMBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomLeftBackStatusFunc) to the message of type _
    addMessageEventFunc2(BOTTOMLEFTBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myBottomLeftBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myLeftBackStatusFunc) to the message of type
    addMessageEventFunc2(LEFTBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myLeftBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopLeftBackStatusFunc) to the message of type
    addMessageEventFunc2(TOPLEFTBACKSTATUS_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTopLeftBackStatusFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTimeFunc) to the message of type
    addMessageEventFunc2(TIME_MSG_ID,
                         std::bind(&GameOfLife3DCode::myTimeFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySynchronizedFunc) to the message of type
    addMessageEventFunc2(SYNCHRONIZED_MSG_ID,
                         std::bind(&GameOfLife3DCode::mySynchronizedFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myInitTimeFunc) to the message of type
    addMessageEventFunc2(INITTIME_MSG_ID,
                         std::bind(&GameOfLife3DCode::myInitTimeFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myUpdateFunc) to the message of type
    addMessageEventFunc2(UPDATE_MSG_ID,
                         std::bind(&GameOfLife3DCode::myUpdateFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Initialization of random numbers generator
    srand(Random::getSimulationSeed());
}

void GameOfLife3DCode::startup()
{
    console << "start " << module->blockId << "\n";

    sync_time = false;

    if (randomAliveInit)
    {
        float r = rand();
        if (r < (RAND_MAX * propAlive))
        {
            alive = true;
        }
        else
        {
            alive = false;
        }
    }
    if (alive)
    {
        status = ALIVE;
        module->setColor(RED);
    }
    else
    {
        module->setColor(GOLD);
    }
    topItf = module->getInterface(SCLattice::Direction::Top);
    bottomItf = module->getInterface(SCLattice::Direction::Bottom);
    rightItf = module->getInterface(SCLattice::Direction::Right);
    leftItf = module->getInterface(SCLattice::Direction::Left);
    frontItf = module->getInterface(SCLattice::Direction::Front);
    backItf = module->getInterface(SCLattice::Direction::Back);

    for (int i = 0; i < 26; i++)
    {
        neighborsStatus.push_back(DEAD);
        updatedNeighbors.push_back(false);
    }
    for (int i = 0; i < 6; i++)
    {
        readyNeighbors.push_back(false);
        syncNeighbors.push_back(false);
    }
    checkConnectedNeighbors();
    sendSelfStatus();
}

void GameOfLife3DCode::sendSelfStatus()
{
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(BOTTOMSTATUS_MSG_ID, status), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(TOPSTATUS_MSG_ID, status), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(LEFTSTATUS_MSG_ID, status), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(RIGHTSTATUS_MSG_ID, status), leftItf, 0, 0);
    }
    if (frontItf != nullptr and frontItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(BACKSTATUS_MSG_ID, status), frontItf, 0, 0);
    }
    if (backItf != nullptr and backItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(FRONTSTATUS_MSG_ID, status), backItf, 0, 0);
    }

    if (updatedNeighbors.at(topId) == true)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTSTATUS_MSG_ID, neighborsStatus.at(topId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTSTATUS_MSG_ID, neighborsStatus.at(topId)), leftItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TBk Message", new MessageOf<int>(TOPBACKSTATUS_MSG_ID, neighborsStatus.at(topId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TF Message", new MessageOf<int>(TOPFRONTSTATUS_MSG_ID, neighborsStatus.at(topId)), backItf, 0, 0);
        }
    }

    if (updatedNeighbors.at(topRightId) == true)
    {
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(topRightId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(topRightId)), backItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(rightId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTSTATUS_MSG_ID, neighborsStatus.at(rightId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTSTATUS_MSG_ID, neighborsStatus.at(rightId)), bottomItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status RBk Message", new MessageOf<int>(RIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(rightId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status RF Message", new MessageOf<int>(RIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(rightId)), backItf, 0, 0);
        }
    }

    if (updatedNeighbors.at(bottomRightId) == true)
    {
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(bottomRightId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(bottomRightId)), backItf, 0, 0);
        }
    }

    if (updatedNeighbors.at(bottomId) == true)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTSTATUS_MSG_ID, neighborsStatus.at(bottomId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTSTATUS_MSG_ID, neighborsStatus.at(bottomId)), leftItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtBk Message", new MessageOf<int>(BOTTOMBACKSTATUS_MSG_ID, neighborsStatus.at(bottomId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtF Message", new MessageOf<int>(BOTTOMFRONTSTATUS_MSG_ID, neighborsStatus.at(bottomId)), backItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(bottomLeftId) == true)
    {
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, neighborsStatus.at(bottomLeftId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(bottomLeftId)), backItf, 0, 0);
        }
    }

    if (updatedNeighbors.at(leftId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTSTATUS_MSG_ID, neighborsStatus.at(leftId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTSTATUS_MSG_ID, neighborsStatus.at(leftId)), bottomItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status LBk Message", new MessageOf<int>(LEFTBACKSTATUS_MSG_ID, neighborsStatus.at(leftId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status LF Message", new MessageOf<int>(LEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(leftId)), backItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(topLeftId) == true)
    {
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, neighborsStatus.at(topLeftId)), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(topLeftId)), backItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(frontId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtF Message", new MessageOf<int>(BOTTOMFRONTSTATUS_MSG_ID, neighborsStatus.at(frontId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TF Message", new MessageOf<int>(TOPFRONTSTATUS_MSG_ID, neighborsStatus.at(frontId)), bottomItf, 0, 0);
        }
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status LF Message", new MessageOf<int>(LEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(frontId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status RF Message", new MessageOf<int>(RIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(frontId)), leftItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(topFrontId) == true)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(topFrontId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(topFrontId)), leftItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(RightFrontId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(RightFrontId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(RightFrontId)), bottomItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(BottomFrontId) == true)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(BottomFrontId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, neighborsStatus.at(BottomFrontId)), leftItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(LeftFrontId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(LeftFrontId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, neighborsStatus.at(LeftFrontId)), bottomItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(backId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtBk Message", new MessageOf<int>(BOTTOMBACKSTATUS_MSG_ID, neighborsStatus.at(backId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TBk Message", new MessageOf<int>(TOPBACKSTATUS_MSG_ID, neighborsStatus.at(backId)), bottomItf, 0, 0);
        }
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status LBk Message", new MessageOf<int>(LEFTBACKSTATUS_MSG_ID, neighborsStatus.at(backId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status RBk Message", new MessageOf<int>(RIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(backId)), leftItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(topBackId) == true)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, neighborsStatus.at(topBackId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(topBackId)), leftItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(RightBackId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(RightBackId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(RightBackId)), bottomItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(BottomBackId) == true)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, neighborsStatus.at(BottomBackId)), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, neighborsStatus.at(BottomBackId)), leftItf, 0, 0);
        }
    }
    if (updatedNeighbors.at(LeftBackId) == true)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, neighborsStatus.at(LeftBackId)), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, neighborsStatus.at(LeftBackId)), bottomItf, 0, 0);
        }
    }
};

void GameOfLife3DCode::readyForUpdate()
{
    sendMessageToAllNeighbors("Ready For Update Message", new MessageOf<int>(TIME_MSG_ID, time), 0, 0, 0);
    console << "time = " << time << "\n";
    auto result = std::find(readyNeighbors.begin(), readyNeighbors.end(), false);
    auto ready = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
    if (result == readyNeighbors.end() && ready == updatedNeighbors.end()) // if all neighbors AND THIS are ready to update their status
    {
        console << "SPOT 2\n";
        statusUpdate();
    }
};

void GameOfLife3DCode::statusUpdate()
{
    // stringstream strstm;
    // strstm << "CHECK status : " << neighborsStatus.at(0) << neighborsStatus.at(1) << neighborsStatus.at(2) << neighborsStatus.at(3) << neighborsStatus.at(4) << neighborsStatus.at(5) << neighborsStatus.at(6) << neighborsStatus.at(7) << "\n";
    // scheduler->trace(strstm.str(), module->blockId, GREEN);

    time++;

    int nbNghbAlive = std::count(neighborsStatus.begin(), neighborsStatus.end(), ALIVE);

    auto survive = std::find(RuleToBeBorn.begin(), RuleToBeBorn.end(), nbNghbAlive);
    if (status == DEAD && survive != RuleToBeBorn.end()) //if the module respects the rule to be born, it is born
    {
        status = ALIVE;
    }
    auto beborn = std::find(RuleToSurvive.begin(), RuleToSurvive.end(), nbNghbAlive);
    if (status == ALIVE && beborn == RuleToSurvive.end()) //if the module doesn't respect the rule to survive, it dies
    {
        status = DEAD;
    }
    stringstream strstm;
    strstm << " status update : " << nbNghbAlive << " (t=" << time << ") -> status = " << status;
    scheduler->trace(strstm.str(), module->blockId, GREEN);

    status_updated = true;
    // Reinitialization
    for (int i = 0; i < 6; i++)
    {
        readyNeighbors.at(i) = false;
    }
    for (int i = 0; i < 26; i++)
    {
        updatedNeighbors.at(i) = false;
        neighborsStatus.at(i) = DEAD;
    }

    sendMessageToAllNeighbors("Status Ready Message", new MessageOf<int>(SYNCHRONIZED_MSG_ID, time), 0, 0, 0);
    auto result = std::find(syncNeighbors.begin(), syncNeighbors.end(), false);
    if (result == syncNeighbors.end())
    {
        statusSynchronized();
    }
};

void GameOfLife3DCode::statusSynchronized()
{
    //Reinitalization
    for (int i = 0; i < 6; i++)
    {
        syncNeighbors.at(i) = false;
    }
    if (status == ALIVE)
    {
        module->setColor(RED);
    }
    else if (status == DEAD)
    {
        module->setColor(GOLD);
    }

    status_updated = false;
    usleep(10000);

    stringstream strstm;
    strstm << "status synchronized";
    scheduler->trace(strstm.str(), module->blockId, MAGENTA);
    sendSelfStatus();
    checkConnectedNeighbors();
};

void GameOfLife3DCode::checkConnectedNeighbors()
{
    if (topItf == nullptr or !topItf->isConnected())
    {
        neighborsStatus.at(topId) = ABSENT;
        updatedNeighbors.at(topId) = true;
        readyNeighbors.at(topId / 2) = true;
        syncNeighbors.at(topId / 2) = true;
        console << "top absent \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TBk Message", new MessageOf<int>(TOPBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TF Message", new MessageOf<int>(TOPFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (bottomItf == nullptr or !bottomItf->isConnected())
    {
        neighborsStatus.at(bottomId) = ABSENT;
        updatedNeighbors.at(bottomId) = true;
        readyNeighbors.at(bottomId / 2) = true;
        syncNeighbors.at(bottomId / 2) = true;
        console << "bottom absent \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BtL Message", new MessageOf<int>(BOTTOMLEFTSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BtR Message", new MessageOf<int>(BOTTOMRIGHTSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtBk Message", new MessageOf<int>(BOTTOMBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtF Message", new MessageOf<int>(BOTTOMFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (rightItf == nullptr or !rightItf->isConnected())
    {
        neighborsStatus.at(rightId) = ABSENT;
        updatedNeighbors.at(rightId) = true;
        readyNeighbors.at(rightId / 2) = true;
        syncNeighbors.at(rightId / 2) = true;
        console << "right absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status RBk Message", new MessageOf<int>(RIGHTBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status RF Message", new MessageOf<int>(RIGHTFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (leftItf == nullptr or !leftItf->isConnected())
    {
        neighborsStatus.at(leftId) = ABSENT;
        updatedNeighbors.at(leftId) = true;
        readyNeighbors.at(leftId / 2) = true;
        syncNeighbors.at(leftId / 2) = true;
        console << "left absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status LBk Message", new MessageOf<int>(LEFTBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status LF Message", new MessageOf<int>(LEFTFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (frontItf == nullptr or !frontItf->isConnected())
    {
        neighborsStatus.at(frontId) = ABSENT;
        updatedNeighbors.at(frontId) = true;
        readyNeighbors.at(frontId / 2) = true;
        syncNeighbors.at(frontId / 2) = true;
        console << "front absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtF Message", new MessageOf<int>(BOTTOMFRONTSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TF Message", new MessageOf<int>(TOPFRONTSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status LF Message", new MessageOf<int>(LEFTFRONTSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status RF Message", new MessageOf<int>(RIGHTFRONTSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (backItf == nullptr or !backItf->isConnected())
    {
        neighborsStatus.at(backId) = ABSENT;
        updatedNeighbors.at(backId) = true;
        readyNeighbors.at(backId / 2) = true;
        syncNeighbors.at(backId / 2) = true;
        console << "back absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtBk Message", new MessageOf<int>(BOTTOMBACKSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TBk Message", new MessageOf<int>(TOPBACKSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status LBk Message", new MessageOf<int>(LEFTBACKSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status RBk Message", new MessageOf<int>(RIGHTBACKSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(topRightId) = ABSENT;
        updatedNeighbors.at(topRightId) = true;
        console << "TR absent \n";
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(topLeftId) = ABSENT;
        updatedNeighbors.at(topLeftId) = true;
        console << "TL absent \n";
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(bottomRightId) = ABSENT;
        updatedNeighbors.at(bottomRightId) = true;
        console << "BtR absent \n";
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(bottomLeftId) = ABSENT;
        updatedNeighbors.at(bottomLeftId) = true;
        console << "BtL absent \n";
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, ABSENT), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, ABSENT), backItf, 0, 0);
        }
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(topId) == ABSENT)
    {
        neighborsStatus.at(topFrontId) = ABSENT;
        updatedNeighbors.at(topFrontId) = true;
        console << "TF absent \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(bottomId) == ABSENT)
    {
        neighborsStatus.at(BottomFrontId) = ABSENT;
        updatedNeighbors.at(BottomFrontId) = true;
        console << "BtF absent \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(RightFrontId) = ABSENT;
        updatedNeighbors.at(RightFrontId) = true;
        console << "RF absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(LeftFrontId) = ABSENT;
        updatedNeighbors.at(LeftFrontId) = true;
        console << "LF absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(topId) == ABSENT)
    {
        neighborsStatus.at(topBackId) = ABSENT;
        updatedNeighbors.at(topBackId) = true;
        console << "TBk absent \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(bottomId) == ABSENT)
    {
        neighborsStatus.at(BottomBackId) = ABSENT;
        updatedNeighbors.at(BottomBackId) = true;
        console << "BkBt absent \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(RightBackId) = ABSENT;
        updatedNeighbors.at(RightBackId) = true;
        console << "RBk absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(LeftBackId) = ABSENT;
        updatedNeighbors.at(LeftBackId) = true;
        console << "LBk absent \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(topRightFrontId) = ABSENT;
        updatedNeighbors.at(topRightFrontId) = true;
        console << "TRF absent \n";
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(TopLeftFrontId) = ABSENT;
        updatedNeighbors.at(TopLeftFrontId) = true;
        console << "TLF absent \n";
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(BottomRightFrontId) = ABSENT;
        updatedNeighbors.at(BottomRightFrontId) = true;
        console << "BtRF absent \n";
    }
    if (neighborsStatus.at(frontId) == ABSENT && neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(BottomLeftFrontId) = ABSENT;
        updatedNeighbors.at(BottomLeftFrontId) = true;
        console << "BtLF absent \n";
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(topRightBackId) = ABSENT;
        updatedNeighbors.at(topRightBackId) = true;
        console << "TRBk absent \n";
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(TopLeftBackId) = ABSENT;
        updatedNeighbors.at(TopLeftBackId) = true;
        console << "TLBk absent \n";
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(BottomRightBackId) = ABSENT;
        updatedNeighbors.at(BottomRightBackId) = true;
        console << "BtRBk absent \n";
    }
    if (neighborsStatus.at(backId) == ABSENT && neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(BottomLeftBackId) = ABSENT;
        updatedNeighbors.at(BottomLeftBackId) = true;
        console << "BtLBk absent \n";
    }

    auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
    if (result == updatedNeighbors.end())
    {
        readyForUpdate();
    }

    // console << "CHECK status : " << neighborsStatus.at(0) << neighborsStatus.at(1) << neighborsStatus.at(2) << neighborsStatus.at(3) << neighborsStatus.at(4) << neighborsStatus.at(5) << neighborsStatus.at(6) << neighborsStatus.at(7) << "\n";
    // console << "CHECK updated : " << updatedNeighbors.at(0) << updatedNeighbors.at(1) << updatedNeighbors.at(2) << updatedNeighbors.at(3) << updatedNeighbors.at(4) << updatedNeighbors.at(5) << updatedNeighbors.at(6) << updatedNeighbors.at(7) << "\n";
};

void GameOfLife3DCode::myTopStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topId) == false)
    {
        console << "T status \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TBk Message", new MessageOf<int>(TOPBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TF Message", new MessageOf<int>(TOPFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        neighborsStatus.at(topId) = msgData;
        updatedNeighbors.at(topId) = true;
        sendSelfStatus();
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopRightStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topRightId) == false)
    {
        console << "TR status \n";
        neighborsStatus.at(topRightId) = msgData;
        updatedNeighbors.at(topRightId) = true;
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myRightStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(rightId) == false)
    {
        console << "R status \n";
        neighborsStatus.at(rightId) = msgData;
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status RBk Message", new MessageOf<int>(RIGHTBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status RF Message", new MessageOf<int>(RIGHTFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        updatedNeighbors.at(rightId) = true;
        sendMessage("Status Message", new MessageOf<int>(LEFTSTATUS_MSG_ID, status), sender, 0, 0);
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomRightStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(bottomRightId) == false)
    {
        console << "BtR status \n";
        neighborsStatus.at(bottomRightId) = msgData;
        updatedNeighbors.at(bottomRightId) = true;
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(bottomId) == false)
    {
        console << "Bt status \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtBk Message", new MessageOf<int>(BOTTOMBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtF Message", new MessageOf<int>(BOTTOMFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        neighborsStatus.at(bottomId) = msgData;
        updatedNeighbors.at(bottomId) = true;
        sendMessage("Status Message", new MessageOf<int>(TOPSTATUS_MSG_ID, status), sender, 0, 0);
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomLeftStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(bottomLeftId) == false)
    {
        console << "BtL status \n";
        neighborsStatus.at(bottomLeftId) = msgData;
        updatedNeighbors.at(bottomLeftId) = true;
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status BtLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status BtLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myLeftStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(leftId) == false)
    {
        console << "L status \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status LBk Message", new MessageOf<int>(LEFTBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status LF Message", new MessageOf<int>(LEFTFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        neighborsStatus.at(leftId) = msgData;
        updatedNeighbors.at(leftId) = true;
        sendMessage("Status Message", new MessageOf<int>(RIGHTSTATUS_MSG_ID, status), sender, 0, 0);
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopLeftStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topLeftId) == false)
    {
        console << "TL status \n";
        neighborsStatus.at(topLeftId) = msgData;
        updatedNeighbors.at(topLeftId) = true;
        if (frontItf != nullptr and frontItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, msgData), frontItf, 0, 0);
        }
        if (backItf != nullptr and backItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, msgData), backItf, 0, 0);
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(frontId) == false)
    {
        console << "F status \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtF Message", new MessageOf<int>(BOTTOMFRONTSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TF Message", new MessageOf<int>(TOPFRONTSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status LF Message", new MessageOf<int>(LEFTFRONTSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status RF Message", new MessageOf<int>(RIGHTFRONTSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(frontId) = msgData;
        updatedNeighbors.at(frontId) = true;
        sendMessage("Status Message", new MessageOf<int>(BACKSTATUS_MSG_ID, status), sender, 0, 0);
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topFrontId) == false)
    {
        console << "TF status \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(topFrontId) = msgData;
        updatedNeighbors.at(topFrontId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopRightFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topRightFrontId) == false)
    {
        neighborsStatus.at(topRightFrontId) = msgData;
        updatedNeighbors.at(topRightFrontId) = true;
        console << "TRF status \n";
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myRightFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(RightFrontId) == false)
    {
        console << "RF status \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TRF Message", new MessageOf<int>(TOPRIGHTFRONTSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        neighborsStatus.at(RightFrontId) = msgData;
        updatedNeighbors.at(RightFrontId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomRightFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(BottomRightFrontId) == false)
    {
        neighborsStatus.at(BottomRightFrontId) = msgData;
        updatedNeighbors.at(BottomRightFrontId) = true;
        console << "BtRF status \n";
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(BottomFrontId) == false)
    {
        console << "BtF status \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BRF Message", new MessageOf<int>(BOTTOMRIGHTFRONTSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(BottomFrontId) = msgData;
        updatedNeighbors.at(BottomFrontId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomLeftFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(BottomLeftFrontId) == false)
    {
        neighborsStatus.at(BottomLeftFrontId) = msgData;
        updatedNeighbors.at(BottomLeftFrontId) = true;
        console << "BtLF status \n";
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myLeftFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(LeftFrontId) == false)
    {
        console << "LF status \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtLF Message", new MessageOf<int>(BOTTOMLEFTFRONTSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TLF Message", new MessageOf<int>(TOPLEFTFRONTSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        neighborsStatus.at(LeftFrontId) = msgData;
        updatedNeighbors.at(LeftFrontId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopLeftFrontStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(TopLeftFrontId) == false)
    {
        neighborsStatus.at(TopLeftFrontId) = msgData;
        updatedNeighbors.at(TopLeftFrontId) = true;
        console << "TLF status \n";
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(backId) == false)
    {
        console << "Bk status \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtBk Message", new MessageOf<int>(BOTTOMBACKSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TBk Message", new MessageOf<int>(TOPBACKSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status LBk Message", new MessageOf<int>(LEFTBACKSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status RBk Message", new MessageOf<int>(RIGHTBACKSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(backId) = msgData;
        updatedNeighbors.at(backId) = true;
        sendMessage("Status Message", new MessageOf<int>(FRONTSTATUS_MSG_ID, status), sender, 0, 0);
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topBackId) == false)
    {
        console << "TBk status \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(topBackId) = msgData;
        updatedNeighbors.at(topBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopRightBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topRightBackId) == false)
    {
        console << "TRBk status \n";
        neighborsStatus.at(topRightBackId) = msgData;
        updatedNeighbors.at(topRightBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myRightBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(RightBackId) == false)
    {
        console << "RBk status \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TRBk Message", new MessageOf<int>(TOPRIGHTBACKSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        neighborsStatus.at(RightBackId) = msgData;
        updatedNeighbors.at(RightBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomRightBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(BottomRightBackId) == false)
    {
        console << "BtRBk status \n";
        neighborsStatus.at(BottomRightBackId) = msgData;
        updatedNeighbors.at(BottomRightBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(BottomBackId) == false)
    {
        console << "BtBk status \n";
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BRBk Message", new MessageOf<int>(BOTTOMRIGHTBACKSTATUS_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(BottomBackId) = msgData;
        updatedNeighbors.at(BottomBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myBottomLeftBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(BottomLeftBackId) == false)
    {
        console << "BtLBk status \n";
        neighborsStatus.at(BottomLeftBackId) = msgData;
        updatedNeighbors.at(BottomLeftBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myLeftBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(LeftBackId) == false)
    {
        console << "LBk status \n";
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BtLBk Message", new MessageOf<int>(BOTTOMLEFTBACKSTATUS_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TLBk Message", new MessageOf<int>(TOPLEFTBACKSTATUS_MSG_ID, msgData), bottomItf, 0, 0);
        }
        neighborsStatus.at(LeftBackId) = msgData;
        updatedNeighbors.at(LeftBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTopLeftBackStatusFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(TopLeftBackId) == false)
    {
        console << "TLBk status \n";
        neighborsStatus.at(TopLeftBackId) = msgData;
        updatedNeighbors.at(TopLeftBackId) = true;
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLife3DCode::myTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    console << "RECIEVED READY MSG from ";
    if (sender == topItf)
    {
        console << "top\n";
    }
    else if (sender == bottomItf)
    {
        console << "bottom\n";
    }
    else if (sender == rightItf)
    {
        console << "right\n";
    }
    else if (sender == leftItf)
    {
        console << "left\n";
    }
    else if (sender == frontItf)
    {
        console << "front\n";
    }
    else if (sender == backItf)
    {
        console << "back\n";
    }
    else
    {
        console << "who ?\n";
    }
    if (msgData >= time - 1) //The neighbor's ready
    {
        if (sender == topItf)
        {
            readyNeighbors.at(topId / 2) = true;
            console << " top ready \n";
        }
        else if (sender == bottomItf)
        {
            readyNeighbors.at(bottomId / 2) = true;
            console << " bottom ready \n";
        }
        else if (sender == rightItf)
        {
            readyNeighbors.at(rightId / 2) = true;
            console << " right ready \n";
        }
        else if (sender == leftItf)
        {
            readyNeighbors.at(leftId / 2) = true;
            console << " left ready \n";
        }
        else if (sender == frontItf)
        {
            readyNeighbors.at(frontId / 2) = true;
            console << " front ready \n";
        }
        else if (sender == backItf)
        {
            readyNeighbors.at(backId / 2) = true;
            console << " back ready \n";
        }
    }
    checkConnectedNeighbors();
    // console << " ready " << readyNeighbors.at(0) << " " << readyNeighbors.at(1) << " " << readyNeighbors.at(2) << " " << readyNeighbors.at(3) << " self-time " << time << " sent time " << msgData << "\n";
    auto result = std::find(readyNeighbors.begin(), readyNeighbors.end(), false);
    auto ready = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
    if (result == readyNeighbors.end() && ready == updatedNeighbors.end()) // if all neighbors AND THIS are ready to update their status
    {
        console << "SPOT 1\n";
        statusUpdate();
    }
};

void GameOfLife3DCode::mySynchronizedFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (msgData >= time - 1)
    {
        if (sender == topItf)
        {
            syncNeighbors.at(topId / 2) = true;
            console << " top synchronized \n";
        }
        else if (sender == bottomItf)
        {
            syncNeighbors.at(bottomId / 2) = true;
            console << " bottom synchronized \n";
        }
        else if (sender == rightItf)
        {
            syncNeighbors.at(rightId / 2) = true;
            console << " right synchronized \n";
        }
        else if (sender == leftItf)
        {
            syncNeighbors.at(leftId / 2) = true;
            console << " left synchronized \n";
        }
        else if (sender == frontItf)
        {
            syncNeighbors.at(frontId / 2) = true;
            console << " front synchronized \n";
        }
        else if (sender == backItf)
        {
            syncNeighbors.at(backId / 2) = true;
            console << " back synchronized \n";
        }
    }
    auto result = std::find(syncNeighbors.begin(), syncNeighbors.end(), false);
    if (result == syncNeighbors.end() && status_updated) //if all neighbors are ready to send & recieve status
    {
        statusSynchronized();
    }
};

void GameOfLife3DCode::myInitTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (sync_time == false)
    {
        time = msgData;
        sync_time = true;
        // sendMessageToAllNeighbors("Update sync time Message", new MessageOf<int>(UPDATE_MSG_ID, time), 0, 0, 0);
    }
    stringstream strstm;
    strstm << "Init time = " << time;
    scheduler->trace(strstm.str(), module->blockId, ORANGE);
};

void GameOfLife3DCode::myUpdateFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (nb_updates < msgData)
    {
        nb_updates = msgData;
        sync_time = false;
        stringstream strstm;
        strstm << "UPDATE : " << nb_updates;
        scheduler->trace(strstm.str(), module->blockId, MAGENTA);
        sendMessageToAllNeighbors("Update Message", new MessageOf<int>(UPDATE_MSG_ID, nb_updates), 0, 0, 0);
        checkConnectedNeighbors();
        sendSelfStatus();
    }
};

void GameOfLife3DCode::processLocalEvent(std::shared_ptr<Event> pev)
{
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    BlinkyBlocksBlockCode::processLocalEvent(pev);

    switch (pev->eventType)
    {
    case EVENT_ADD_NEIGHBOR:
    {
        stringstream strstm;
        strstm << "New neighbor";
        scheduler->trace(strstm.str(), module->blockId, GOLD);
        sendMessageToAllNeighbors("Init Time New Neighbor Message", new MessageOf<int>(INITTIME_MSG_ID, time), 0, 0, 0);
        sendSelfStatus();
        // if (status_updated)
        // {
        //     sendMessageToAllNeighbors("Status Ready Message", new MessageOf<int>(SYNCHRONIZED_MSG_ID, time), 0, 0, 0);
        // }
        break;
    }

    case EVENT_REMOVE_NEIGHBOR:
    {
        nb_updates++;
        checkConnectedNeighbors();
        break;
    }
    break;
    }
};

void GameOfLife3DCode::parseUserBlockElements(TiXmlElement *blockElt)
{
    blockElt->QueryBoolAttribute("alive", &alive);
}

bool GameOfLife3DCode::parseUserCommandLineArgument(int &argc, char **argv[])
{
    /* Reading the command line */
    if ((argc > 0) && ((*argv)[0][0] == '-'))
    {
        switch ((*argv)[0][1])
        {

        // Single character example: -b
        case 'I':
        {
            cout << "-I option provided: random initialization" << endl;
            randomAliveInit = true;
            return true;
        }
        break;

        default:
            cerr << "Unrecognized command line argument: " << (*argv)[0] << endl;
        }
    }

    return false;
}
