#include "tetrisCode.hpp"

TetrisCode::TetrisCode(BlinkyBlocksBlock *host) : BlinkyBlocksBlockCode(host), module(host)
{
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host)
        return;

    // Registers a callback (myHeightMsgFunc) to the message of type E
    addMessageEventFunc2(HEIGHTMSG_MSG_ID,
                         std::bind(&TetrisCode::myHeightMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));

    // Registers a callback (myWidthMsgFunc) to the message of type D
    addMessageEventFunc2(WIDTHMSG_MSG_ID,
                         std::bind(&TetrisCode::myWidthMsgFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
}

void TetrisCode::startup()
{
    console << "start " << module->blockId << "\n";

    P2PNetworkInterface *topItf = module->getInterface(SCLattice::Direction::Top);
    P2PNetworkInterface *bottomItf = module->getInterface(SCLattice::Direction::Bottom);
    P2PNetworkInterface *rightItf = module->getInterface(SCLattice::Direction::Right);
    P2PNetworkInterface *leftItf = module->getInterface(SCLattice::Direction::Left);

    sendHeight();
    sendWidth();

    module->setColor(Colors[(height+width)%NB_COLORS]);
}

void TetrisCode::sendHeight()
{
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height + 1), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected() and height > 0) // negative height is not send
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height - 1), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected())
    {
        sendMessage("Height Message", new MessageOf<int>(HEIGHTMSG_MSG_ID, height), leftItf, 0, 0);
    }
}

void TetrisCode::sendWidth()
{
    if (topItf != nullptr and topItf->isConnected())
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width), topItf, 0, 0);
    }
    if (bottomItf != nullptr and bottomItf->isConnected())
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width), bottomItf, 0, 0);
    }
    if (rightItf != nullptr and rightItf->isConnected())
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width + 1), rightItf, 0, 0);
    }
    if (leftItf != nullptr and leftItf->isConnected() and width > 0) // negative width is not send
    {
        sendMessage("Width Message", new MessageOf<int>(WIDTHMSG_MSG_ID, width - 1), leftItf, 0, 0);
    }
}
void TetrisCode::myHeightMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();
    if (height < msgData)
    {
        height = msgData;
        module->setColor(Colors[(height+width)%NB_COLORS]);
        sendHeight();
    }
};

void TetrisCode::myWidthMsgFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender)
{

    MessageOf<int> *msg = static_cast<MessageOf<int> *>(_msg.get());
    int msgData = *msg->getData();

    if (width < msgData)
    {
        width = msgData;
        module->setColor(Colors[(height+width)%NB_COLORS]);
        sendWidth();
    }
};

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
