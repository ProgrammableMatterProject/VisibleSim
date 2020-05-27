#include "gameOfLifeCode.hpp"
#include "utils/random.h"
#include <vector>
#include <algorithm>
#include <unistd.h>

GameOfLifeCode::GameOfLifeCode(BlinkyBlocksBlock *host) : BlinkyBlocksBlockCode(host), module(host)
{
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host)
        return;

    // Registers a callback (myTopRightLivesFunc) to the message of type O
    addMessageEventFunc2(TOPRIGHTLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myTopRightLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myRightLivesFunc) to the message of type G
    addMessageEventFunc2(RIGHTLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myRightLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomRightLivesFunc) to the message of type T
    addMessageEventFunc2(BOTTOMRIGHTLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myBottomRightLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomLivesFunc) to the message of type O
    addMessageEventFunc2(BOTTOMLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myBottomLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myBottomLeftLivesFunc) to the message of type M
    addMessageEventFunc2(BOTTOMLEFTLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myBottomLeftLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myLeftLivesFunc) to the message of type V
    addMessageEventFunc2(LEFTLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myLeftLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopLeftLivesFunc) to the message of type L
    addMessageEventFunc2(TOPLEFTLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myTopLeftLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTopLivesFunc) to the message of type _
    addMessageEventFunc2(TOPLIVES_MSG_ID,
                         std::bind(&GameOfLifeCode::myTopLivesFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myTimeFunc) to the message of type I
    addMessageEventFunc2(TIME_MSG_ID,
                         std::bind(&GameOfLifeCode::myTimeFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySynchronizedFunc) to the message of type E
    addMessageEventFunc2(SYNCHRONIZED_MSG_ID,
                         std::bind(&GameOfLifeCode::mySynchronizedFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySynchronizedFunc) to the message of type ?
    addMessageEventFunc2(ASK_INIT_TIME_MSG_ID,
                         std::bind(&GameOfLifeCode::myAskInitTimeFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySynchronizedFunc) to the message of type ?
    addMessageEventFunc2(INIT_TIME_MSG_ID,
                         std::bind(&GameOfLifeCode::myInitTimeFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (mySynchronizedFunc) to the message of type ?
    addMessageEventFunc2(UPDATE_MSG_ID,
                         std::bind(&GameOfLifeCode::myUpdateFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Initialization of random numbers generator
    srand(Random::getSimulationSeed());
}

void GameOfLifeCode::startup()
{
    console << "start " << module->blockId << "\n";
    //initialization of time
    sendMessageToAllNeighbors("Ask for Time", new Message(ASK_INIT_TIME_MSG_ID), 0, 0, 0);

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
        module->setColor(COLOR_ALIVE);
    }
    else
    {
        module->setColor(COLOR_DEAD);
    }
    topItf = module->getInterface(SCLattice::Direction::Top);
    bottomItf = module->getInterface(SCLattice::Direction::Bottom);
    rightItf = module->getInterface(SCLattice::Direction::Right);
    leftItf = module->getInterface(SCLattice::Direction::Left);

    initialization();
}

void GameOfLifeCode::initialization()
{
    for (int i = 0; i < 8; i++)
    {
        neighborsStatus.push_back(DEAD);
        updatedNeighbors.push_back(false);
    }
    for (int i = 0; i < 4; i++)
    {
        readyNeighbors.push_back(false);
        syncNeighbors.push_back(false);
    }
    checkConnectedNeighbors();
    sendSelfStatus();
};

void GameOfLifeCode::sendSelfStatus()
{
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(BOTTOMLIVES_MSG_ID, status), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(TOPLIVES_MSG_ID, status), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(LEFTLIVES_MSG_ID, status), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected())
    {
        sendMessage("Status Message", new MessageOf<int>(RIGHTLIVES_MSG_ID, status), leftItf, 0, 0);
    }
};

void GameOfLifeCode::readyForUpdate()
{
    sendMessageToAllNeighbors("Ready For Update Message", new MessageOf<int>(TIME_MSG_ID, time), 0, 0, 0);
};

void GameOfLifeCode::statusUpdate()
{
    console << " status update \n";

    stringstream strstm;
    strstm << "CHECK status : " << neighborsStatus.at(0) << neighborsStatus.at(1) << neighborsStatus.at(2) << neighborsStatus.at(3) << neighborsStatus.at(4) << neighborsStatus.at(5) << neighborsStatus.at(6) << neighborsStatus.at(7) << "\n";
    scheduler->trace(strstm.str(), module->blockId, GREEN);

    time++;

    int nbNghbAlive = std::count(neighborsStatus.begin(), neighborsStatus.end(), ALIVE);

    auto survive = std::find(RuleToBeBorn.begin(),RuleToBeBorn.end(),nbNghbAlive);
    if (status == DEAD && survive != RuleToBeBorn.end()) //if the module respects the rule to be born, it is born
    {
        status = ALIVE;
    }
    auto beborn = std::find(RuleToSurvive.begin(),RuleToSurvive.end(),nbNghbAlive);
    if (status == ALIVE && beborn == RuleToSurvive.end()) //if the module doesn't respect the rule to survive, it dies
    {
        status = DEAD;
    }
    // Reinitialization
    for (int i = 0; i < 4; i++)
    {
        readyNeighbors.at(i) = false;
    }
    for (int i = 0; i < 8; i++)
    {
        updatedNeighbors.at(i) = false;
        neighborsStatus.at(i) = DEAD;
    }
    usleep(10000);

    sendMessageToAllNeighbors("Status Ready Message", new MessageOf<int>(SYNCHRONIZED_MSG_ID, time), 0, 0, 0);
};

void GameOfLifeCode::statusSynchronized()
{
    //Reinitalization
    for (int i = 0; i < 4; i++)
    {
        syncNeighbors.at(i) = false;
    }
    if (status == ALIVE)
    {
        module->setColor(COLOR_ALIVE);
    }
    else if (status == DEAD)
    {
        module->setColor(COLOR_DEAD);
    }
    console << "status synchronized : \n";
    sendSelfStatus();
    checkConnectedNeighbors();
};

void GameOfLifeCode::checkConnectedNeighbors()
{
    if (topItf == nullptr or !topItf->isConnected())
    {
        neighborsStatus.at(topId) = ABSENT;
        updatedNeighbors.at(topId) = true;
        readyNeighbors.at(topId / 2) = true;
        syncNeighbors.at(topId / 2) = true;
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTLIVES_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTLIVES_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (bottomItf == nullptr or !bottomItf->isConnected())
    {
        neighborsStatus.at(bottomId) = ABSENT;
        updatedNeighbors.at(bottomId) = true;
        readyNeighbors.at(bottomId / 2) = true;
        syncNeighbors.at(bottomId / 2) = true;
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTLIVES_MSG_ID, ABSENT), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTLIVES_MSG_ID, ABSENT), leftItf, 0, 0);
        }
    }
    if (rightItf == nullptr or !rightItf->isConnected())
    {
        neighborsStatus.at(rightId) = ABSENT;
        updatedNeighbors.at(rightId) = true;
        readyNeighbors.at(rightId / 2) = true;
        syncNeighbors.at(rightId / 2) = true;
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTLIVES_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTLIVES_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
    }
    if (leftItf == nullptr or !leftItf->isConnected())
    {
        neighborsStatus.at(leftId) = ABSENT;
        updatedNeighbors.at(leftId) = true;
        readyNeighbors.at(leftId / 2) = true;
        syncNeighbors.at(leftId / 2) = true;
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTLIVES_MSG_ID, ABSENT), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTLIVES_MSG_ID, ABSENT), bottomItf, 0, 0);
        }
    }
    if (neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(topRightId) = ABSENT;
        updatedNeighbors.at(topRightId) = true;
    }
    if (neighborsStatus.at(topId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(topLeftId) = ABSENT;
        updatedNeighbors.at(topLeftId) = true;
    }
    if (neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(rightId) == ABSENT)
    {
        neighborsStatus.at(bottomRightId) = ABSENT;
        updatedNeighbors.at(bottomRightId) = true;
    }
    if (neighborsStatus.at(bottomId) == ABSENT && neighborsStatus.at(leftId) == ABSENT)
    {
        neighborsStatus.at(bottomLeftId) = ABSENT;
        updatedNeighbors.at(bottomLeftId) = true;
    }

    // console << "CHECK status : " << neighborsStatus.at(0) << neighborsStatus.at(1) << neighborsStatus.at(2) << neighborsStatus.at(3) << neighborsStatus.at(4) << neighborsStatus.at(5) << neighborsStatus.at(6) << neighborsStatus.at(7) << "\n";
    // console << "CHECK updated : " << updatedNeighbors.at(0) << updatedNeighbors.at(1) << updatedNeighbors.at(2) << updatedNeighbors.at(3) << updatedNeighbors.at(4) << updatedNeighbors.at(5) << updatedNeighbors.at(6) << updatedNeighbors.at(7) << "\n";
};

void GameOfLifeCode::myTopRightLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topRightId) == false)
    {
        neighborsStatus.at(topRightId) = msgData;
        updatedNeighbors.at(topRightId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE TR status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myRightLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(rightId) == false)
    {
        neighborsStatus.at(rightId) = msgData;
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTLIVES_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTLIVES_MSG_ID, msgData), bottomItf, 0, 0);
        }
        updatedNeighbors.at(rightId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE R status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myBottomRightLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(bottomRightId) == false)
    {
        neighborsStatus.at(bottomRightId) = msgData;
        updatedNeighbors.at(bottomRightId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE BR status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myBottomLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(bottomId) == false)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTLIVES_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status BR Message", new MessageOf<int>(BOTTOMRIGHTLIVES_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(bottomId) = msgData;
        updatedNeighbors.at(bottomId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE B status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myBottomLeftLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(bottomLeftId) == false)
    {
        neighborsStatus.at(bottomLeftId) = msgData;
        updatedNeighbors.at(bottomLeftId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE BL status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myLeftLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(leftId) == false)
    {
        if (topItf != nullptr and topItf->isConnected())
        {
            sendMessage("Status BL Message", new MessageOf<int>(BOTTOMLEFTLIVES_MSG_ID, msgData), topItf, 0, 0);
        }
        if (bottomItf != nullptr and bottomItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTLIVES_MSG_ID, msgData), bottomItf, 0, 0);
        }
        neighborsStatus.at(leftId) = msgData;
        updatedNeighbors.at(leftId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE L status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myTopLeftLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topLeftId) == false)
    {
        neighborsStatus.at(topLeftId) = msgData;
        updatedNeighbors.at(topLeftId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE TL status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myTopLivesFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (updatedNeighbors.at(topId) == false)
    {
        if (rightItf != nullptr and rightItf->isConnected())
        {
            sendMessage("Status TL Message", new MessageOf<int>(TOPLEFTLIVES_MSG_ID, msgData), rightItf, 0, 0);
        }
        if (leftItf != nullptr and leftItf->isConnected())
        {
            sendMessage("Status TR Message", new MessageOf<int>(TOPRIGHTLIVES_MSG_ID, msgData), leftItf, 0, 0);
        }
        neighborsStatus.at(topId) = msgData;
        updatedNeighbors.at(topId) = true;
        if (module->blockId == 2 || module->blockId == 10)
        {
            console << "UPDATE T status = " << msgData << "\n";
        }
        auto result = std::find(updatedNeighbors.begin(), updatedNeighbors.end(), false);
        if (result == updatedNeighbors.end())
        {
            readyForUpdate();
        }
    }
};

void GameOfLifeCode::myTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (msgData >= time) //The neighbor's ready
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
    }
    checkConnectedNeighbors();
    // console << " ready " << readyNeighbors.at(0) << " " << readyNeighbors.at(1) << " " << readyNeighbors.at(2) << " " << readyNeighbors.at(3) << " self-time " << time << " sent time " << msgData << "\n";
    auto result = std::find(readyNeighbors.begin(), readyNeighbors.end(), false);
    if (result == readyNeighbors.end()) // if all neighbors are ready to update their status
    {
        statusUpdate();
    }
};

void GameOfLifeCode::mySynchronizedFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (msgData >= time)
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
    }
    auto result = std::find(syncNeighbors.begin(), syncNeighbors.end(), false);
    if (result == syncNeighbors.end()) //if all neighbors are ready to send & recieve status
    {
        statusSynchronized();
    }
};

void GameOfLifeCode::myAskInitTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    console << "recieved demand for time\n";
    sendMessage("Init Time Message", new MessageOf<int>(INIT_TIME_MSG_ID, time), sender, 0, 0);
};

void GameOfLifeCode::myInitTimeFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (!sync_time)
    {
        time = msgData;
        sync_time = true ;
        sendMessageToAllNeighbors("Update sync time Message", new MessageOf<int>(UPDATE_MSG_ID, time), 0, 0, 0);
    }
};

void GameOfLifeCode::myUpdateFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
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

void GameOfLifeCode::processLocalEvent(std::shared_ptr<Event> pev)
{
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    BlinkyBlocksBlockCode::processLocalEvent(pev);

    switch (pev->eventType)
    {
        // case EVENT_ADD_NEIGHBOR: {
        // break;
        // }

    case EVENT_REMOVE_NEIGHBOR:
    {
        stringstream strstm;
        strstm << "###### NEIGHBOR LEFT #############################";
        scheduler->trace(strstm.str(), module->blockId, RED);
        nb_updates++;
        // sendMessageToAllNeighbors("Update Message", new MessageOf<int>(UPDATE_MSG_ID, nb_updates), 0, 0, 0);
        // init();

        checkConnectedNeighbors();
        break;
    }
    break;
    }
};

void GameOfLifeCode::parseUserBlockElements(TiXmlElement *blockElt)
{

    blockElt->QueryBoolAttribute("alive", &alive);

    if (alive)
    {
        stringstream strstm;
        strstm << " is alive because it has been init with alive = "
               << alive << "!";
        scheduler->trace(strstm.str(), module->blockId, CYAN);
    }

    // (pour parsing cf.  simulatorCore/src/base/simulator.cpp)
}

bool GameOfLifeCode::parseUserCommandLineArgument(int &argc, char **argv[])
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

            //     // Composite argument example: --foo 13
            // case '-': {
            //     string varg = string((*argv)[0] + 2); // argv[0] without "--"
            //     if (varg == string("foo")) { //
            //         int fooArg;
            //         try {
            //             fooArg = stoi((*argv)[1]);
            //             argc--;
            //             (*argv)++;
            //         } catch(std::logic_error&) {
            //             stringstream err;
            //             err << "foo must be an integer. Found foo = " << argv[1] << endl;
            //             throw CLIParsingError(err.str());
            //         }

            //         cout << "--foo option provided with value: " << fooArg << endl;
            //     } else return false;

            //     return true;
            // }

        default:
            cerr << "Unrecognized command line argument: " << (*argv)[0] << endl;
        }
    }

    return false;
}
