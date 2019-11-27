#include "simpleReconfC2DCode.h"

Color myTabColors[8]={RED,ORANGE,YELLOW,GREEN,CYAN,BLUE,MAGENTA,GREY};

using namespace Catoms2D;

void SimpleReconfC2DCode::startup() {
    addMessageEventFunc2(GO_MSG_ID,
                         std::bind(&SimpleReconfC2DCode::handleGoMessage, this,
                                   std::placeholders::_1, std::placeholders::_2));

    catom = static_cast<Catoms2DBlock*>(hostBlock);

    catom->setColor(GREY);
    layer = catom->position[2];

    verticalLineToHorizontalLineStartup();
}

void SimpleReconfC2DCode::onMotionEnd() {
    if (catom->position[2] > 0)
        rotateCWAfterCheck();
    else sendGoMessageToNextModule(layer - 1);
}

void SimpleReconfC2DCode::verticalLineToHorizontalLineStartup() {
    if (not catom->hasANeighbor(HLattice::TopRight)
        and not catom->hasANeighbor(HLattice::TopLeft)) {
        rotateCWAfterCheck();
    }
}

void SimpleReconfC2DCode::sendGoMessageToNextModule(int msgLayer) {
    // Send to left neighbor until reaching the right tip of the growing horizontal line
    //  then send messages up the vertical line
    P2PNetworkInterface *itf;
    stringstream info; info << "Go(" << msgLayer << ")";
    if (catom->hasANeighbor(HLattice::Left)) {
        itf = catom->getInterface(HLattice::Left);
        sendMessage(info.str().c_str(),
                    new MessageOf<int>(GO_MSG_ID, msgLayer), itf, 100, 200);
    } else {
        if (not catom->hasANeighbor(HLattice::TopRight)
            and not catom->hasANeighbor(HLattice::TopLeft))
            return; // terminate1

        if (catom->hasANeighbor(HLattice::TopRight)) {
            itf = catom->getInterface(HLattice::TopRight);
        } else {
            itf = catom->getInterface(HLattice::TopLeft);
        }

        sendMessage(info.str().c_str(),
                    new MessageOf<int>(GO_MSG_ID, msgLayer), itf, 100, 200);
    }
}

void SimpleReconfC2DCode::rotateCWAfterCheck() {
    if (catom->canRotate(RelativeDirection::Direction::CW))
        catom->setColor(GREEN);
    else
        catom->setColor(RED);

    catom->rotate(RelativeDirection::Direction::CW);
}

void SimpleReconfC2DCode::handleGoMessage(MessagePtr anonMsg, P2PNetworkInterface*sender) {
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(anonMsg.get());
    int rcvd_layer = *msg->getData();

    console << " received Go " << rcvd_layer << " from " << sender->getConnectedBlockId();

    if (not catom->hasANeighbor(HLattice::TopRight)
        and not catom->hasANeighbor(HLattice::TopLeft)
        and layer == rcvd_layer) {
        rotateCWAfterCheck();
    } else {
        sendGoMessageToNextModule(rcvd_layer);
    }
}
