#include "rainbowCode.hpp"
#include <array>

RainbowCode::RainbowCode(BlinkyBlocksBlock *host) : BlinkyBlocksBlockCode(host), module(host)
{
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host)
        return;

    // Registers a callback (myDistanceMessageFunc) to the message of type I
    addMessageEventFunc2(LAYERMESSAGE_MSG_ID,
                         std::bind(&RainbowCode::myLayerMessageFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
    // Registers a callback (myUpdateMessageFunc) to the message of type II
    addMessageEventFunc2(UPDATEMESSAGE_MSG_ID,
                         std::bind(&RainbowCode::myUpdateMessageFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
}

void RainbowCode::startup()
{
    console << "start " << module->blockId << "\n";
    P2PNetworkInterface *topItf = module->getInterface(SCLattice::Direction::Top);
    P2PNetworkInterface *bottomItf = module->getInterface(SCLattice::Direction::Bottom);
    P2PNetworkInterface *backItf = module->getInterface(SCLattice::Direction::Back);
    P2PNetworkInterface *frontItf = module->getInterface(SCLattice::Direction::Front);
    P2PNetworkInterface *rightItf = module->getInterface(SCLattice::Direction::Right);
    P2PNetworkInterface *leftItf = module->getInterface(SCLattice::Direction::Left);
    listItf = {topItf, bottomItf, backItf, frontItf, rightItf, leftItf};
    sendLayer();
    module->setColor(Colors[layer % NB_COLORS]);
}
void RainbowCode::myLayerMessageFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (msgData > layer) //To converge, update is done only if the layer sent is higher than the current layer
    {
        layer = msgData;
        sendLayer();
        module->setColor(Colors[layer % NB_COLORS]);
    }
}
void RainbowCode::myUpdateMessageFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{
    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (nb_update < msgData)
    {
        console << " update = " << nb_update << "\n";
        nb_update = msgData;
        sendMessageToAllNeighbors("Update Message", new MessageOf<int>(UPDATEMESSAGE_MSG_ID, nb_update), 1000000, 0, 1, sender);
        layer = 0;
        startup();
    }
}
void RainbowCode::sendLayer()
{
    P2PNetworkInterface *topItf = listItf.at(0);
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Layer Message", new MessageOf<int>(LAYERMESSAGE_MSG_ID, layer + 1), topItf, 1000000, 0);
    }
    P2PNetworkInterface *bottomItf = listItf.at(1);
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("Layer Message", new MessageOf<int>(LAYERMESSAGE_MSG_ID, layer - 1), bottomItf, 1000000, 0);
    }
    for (int i = 2; i < 6; i++)
    {
        P2PNetworkInterface *itf = listItf.at(i);
        if (itf != nullptr and itf->isConnected())
        {
            sendMessage("Layer Message", new MessageOf<int>(LAYERMESSAGE_MSG_ID, layer), itf, 1000000, 0);
        }
    }
}
void RainbowCode::processLocalEvent(EventPtr pev)
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
        layer = 0;
        nb_update += 1;
        console << "update nb : " << nb_update << "\n";
        sendMessageToAllNeighbors("Update Message",
                                  new MessageOf<int>(UPDATEMESSAGE_MSG_ID, nb_update), 0, 0, 0);
        startup();

        break;
    }

    case EVENT_REMOVE_NEIGHBOR:
    {
        // Do something when a neighbor is removed from an interface of the module
        layer = 0;
        nb_update += 1;
        console << "update nb : " << nb_update << "\n";
        sendMessageToAllNeighbors("Update Message",
                                  new MessageOf<int>(UPDATEMESSAGE_MSG_ID, nb_update), 0, 0, 0);
        startup();
        break;
    }
    }
};
