#include "myAppSCCode.hpp"

Cell3DPosition goalPosition;

MyAppSCCode::MyAppSCCode(SlidingCubesBlock *host) : SlidingCubesBlockCode(host), module(host) {
    // @warning Do not remove block below, as a blockcode with a NULL host might be created
    //  for command line parsing
    if (not host) return;
    addMessageEventFunc2(ELECT_MSG_ID,
                         std::bind(&MyAppSCCode::myElectFunc, this,
                                   std::placeholders::_1, std::placeholders::_2));
}

void MyAppSCCode::startup() {
    console << "start " << module->blockId << "\n";

    for (int i=0; i<6; i++) {
        auto p2p =module->getInterface(SCLattice2::Direction(i));
        if (p2p->isConnected()) {
            cout << i << ":" << p2p->getConnectedBlockBId() << endl;
        }
    }

    if (isLeader) { // Master id is 1
        setColor(RED);
        tryToMove();
    }
}

void MyAppSCCode::myElectFunc(std::shared_ptr<Message> _msg, P2PNetworkInterface *sender) {
    console << "rec. Elect() from " << sender->getConnectedBlockId() << "\n";
    // search a neighbor in (x,y-1,z)
    Cell3DPosition prevPos(module->position[0], module->position[1] - 1, module->position[2]);
    auto p2p=findNeighborAt(prevPos);
    if (p2p) {
        setColor(BLUE);
        sendMessage(new Message(ELECT_MSG_ID), p2p, 200000, 0);
    } else {
        // if not try to move
        setColor(YELLOW);
        if (!tryToMove()) {
            console << "the end!\n";
        }
    }
}

P2PNetworkInterface *MyAppSCCode::findNeighborAt(const Cell3DPosition &pos) {
    int i = 0;
    while (i<6 && (!module->getInterface(SCLattice2::Direction(i))->isConnected() ||
           module->getInterface(SCLattice2::Direction(i))->connectedInterface->hostBlock->position != pos)) {
        i++;
    }
    //cout << "found=" << i << endl;
    if (i < 6) {
        return module->getInterface(SCLattice2::Direction(i));
    }
    return nullptr;
}


void MyAppSCCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("leader");
    isLeader = (attr?Simulator::extractBoolFromString(attr):false);
    if (isLeader) std::cout << getId() << " is leader!" << std::endl; // complete with your code
}

void MyAppSCCode::onMotionEnd() {
    // complete with your code
    console << " End of motion to " << module->position << "\n";
    cout << "module: " << getId() << endl;
/*    for (int i=0; i<6; i++) {
        auto p2p =module->getInterface(SCLattice2::Direction(i));
        if (p2p->isConnected()) {
            cout << i << ":" << p2p->getConnectedBlockBId() << endl;
        }
    }*/
    if (module->position==goalPosition) {
        setColor(GREEN);
        return;
    }
    if (!tryToMove()) {
        // search a neighbor in (x,y-1,z)
        Cell3DPosition prevPos(module->position[0], module->position[1] - 1, module->position[2]);
        auto p2p=findNeighborAt(prevPos);
        if (p2p) {
            setColor(ORANGE);
            sendMessage(new Message(ELECT_MSG_ID), p2p, 200000, 0);
        }
    }
}

bool MyAppSCCode::tryToMove() {
    auto dir=goalPosition-module->position;
    cout << getId() << " tries at " << module->position << ", dir=" << dir << endl;
    Cell3DPosition nextPos;
    if (dir[1]!=0) {
        nextPos = module->position + Cell3DPosition(0, (dir[1]>0?1:-1), 0);
        cout << "try " << nextPos << endl;
        if (module->canMoveTo(nextPos)) {
            cout << "------------------------\nmove to " << nextPos << endl;
            module->moveTo(nextPos);
            return true;
        }
        // try 1 higher
        auto dirPos= nextPos;
        nextPos = dirPos+Cell3DPosition(0,0,1);
        cout << "try " << nextPos << endl;
        if (module->canMoveTo(nextPos)) {
            cout << "------------------------\nmove to " << nextPos << endl;
            module->moveTo(nextPos);
            return true;
        }
        nextPos = dirPos+Cell3DPosition(0,0,-1);
        cout << "try " << nextPos << endl;
        if (module->canMoveTo(nextPos)) {
            cout << "------------------------\nmove to " << nextPos << endl;
            module->moveTo(nextPos);
            return true;
        }

    }

    if (dir[0]!=0) {
        nextPos = module->position + Cell3DPosition((dir[0] > 0 ? 1 : -1), 0, 0);
        cout << "try " << nextPos << endl;
        if (module->canMoveTo(nextPos)) {
            cout << "------------------------\nmove to " << nextPos << endl;
            module->moveTo(nextPos);
            return true;
        }
        // try 1 higher
        auto dirPos= nextPos;
        nextPos = dirPos+Cell3DPosition(0,0,1);
        cout << "try " << nextPos << endl;
        if (module->canMoveTo(nextPos)) {
            cout << "------------------------\nmove to " << nextPos << endl;
            module->moveTo(nextPos);
            return true;
        }
        nextPos = dirPos+Cell3DPosition(0,0,-1);
        cout << "try " << nextPos << endl;
        if (module->canMoveTo(nextPos)) {
            cout << "------------------------\nmove to " << nextPos << endl;
            module->moveTo(nextPos);
            return true;
        }

    }
    return false;
}

void MyAppSCCode::parseUserElements(TiXmlDocument *config) {
    TiXmlNode *vs = config->FirstChild("vs");
    if (!vs) return;
    TiXmlNode *node = vs->FirstChild("goal");
    if (!node) return;
    TiXmlElement *element = node->ToElement();
    const char *attr = element->Attribute("position");
    if (attr) {
        goalPosition=Simulator::extractCell3DPositionFromString(attr);
        std::cout << "goalPosition = " << goalPosition << std::endl;
    } else {
        goalPosition.set(1,5,0);
    }
}

void MyAppSCCode::onGlDraw() {
    static const float thick=0.8;
    static const float color[4]={2.2f,0.2f,0.2f,1.0f};
    const Cell3DPosition& gs = lattice->gridSize;
    const Vector3D gl = lattice->gridScale;
    glDisable(GL_TEXTURE);
    glMaterialfv(GL_FRONT,GL_AMBIENT_AND_DIFFUSE,color);
    glPushMatrix();
    glNormal3f(0,0,1);
    glScalef(gl[0],gl[1],gl[2]);
    glTranslatef(goalPosition[0],goalPosition[1],goalPosition[2]-0.49);
    glBegin(GL_QUAD_STRIP);
    for (int i=0; i<=36; i++) {
        double cs=0.5*cos(i*M_PI/18);
        double ss=0.5*sin(i*M_PI/18);
        glVertex3f(thick*cs,thick*ss,0);
        glVertex3f(cs,ss,0);
    }
    glEnd();
    glPopMatrix();
 }