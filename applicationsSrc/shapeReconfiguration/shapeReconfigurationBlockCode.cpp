#include "shapeReconfigurationBlockCode.hpp"

using namespace Hexanodes;

ShapeReconfigurationBlockCode::ShapeReconfigurationBlockCode(HexanodesBlock *host) : HexanodesBlockCode(host) {
    
    if (not host) return;

    addMessageEventFunc2(DISTANCE_BROADCAST_MSG_ID,
                         std::bind(&ShapeReconfigurationBlockCode::myDistanceBroadcastFunction, this,
                                   std::placeholders::_1, std::placeholders::_2));
                                
    addMessageEventFunc2(DISTANCE_FORECAST_MSG_ID,
                         std::bind(&ShapeReconfigurationBlockCode::myDistanceAcknowledgeFunction, this,
                                   std::placeholders::_1, std::placeholders::_2));

    addMessageEventFunc2(LEADER_BROADCAST_MSG_ID,
                         std::bind(&ShapeReconfigurationBlockCode::myLeaderBroadcastFunction, this,
                                   std::placeholders::_1, std::placeholders::_2));

    module = static_cast<HexanodesBlock*>(hostBlock);
    
    
}

void ShapeReconfigurationBlockCode::startup() {
    if (target && target->isInTarget(module->position)) {
       module->setColor(GREEN);
       isInPosition = true;
    }
    else {  
        module->setColor(RED);     
        isInPosition = false;
    }
    initialPosition = module->position;

    if (isLeader) {
        console << "Nb of obstacles: " << Hexanodes::getWorld()->obstacles.size() << "\n";
        for(int i=0; i<Hexanodes::getWorld()->obstacles.size(); i++){
            console << "Obstacle " << i << " is at position: " << Hexanodes::getWorld()->obstacles.at(i).pt[0] << ", " << Hexanodes::getWorld()->obstacles.at(i).pt[1] << ", " << Hexanodes::getWorld()->obstacles.at(i).pt[2] << ", \n";
        }
        myNextLeafId = module->blockId;
        myCurrentRound = 1;
        myDistance = 0;
        isLeader = false;
        myNbWaitedAnswers = sendMessageToAllNeighbors("Sending my distance and current round number. ", new MessageOf<pair<int,int>>(DISTANCE_BROADCAST_MSG_ID, make_pair(myDistance, myCurrentRound)),1000,100,0);
        
	}
}

void ShapeReconfigurationBlockCode::myDistanceBroadcastFunction(std::shared_ptr<Message> _msg, P2PNetworkInterface* sender) {
    MessageOf<pair<int, int>>* msg = static_cast<MessageOf<pair<int, int>>*>(_msg.get());
    pair<int,int> msgData = *msg->getData();
    console << "I received a distance d = " <<  msgData.first  << " from " << sender->getConnectedBlockId() << "\n";
    console << "I received a current round of: " <<  msgData.second << "\n";

    if(myParent == nullptr || myCurrentRound < msgData.second){
        myChildren.clear();
        myDistance = msgData.first + 1;
        myMaxDistanceToLeaf = myDistance;
        myCurrentRound = msgData.second;
        myNextLeafId = module->blockId;
        myParent = sender;
        myNbWaitedAnswers = sendMessageToAllNeighbors("Sending my distance and current round number. ", new MessageOf<pair<int, int>>(DISTANCE_BROADCAST_MSG_ID, make_pair(myDistance, myCurrentRound)), 1000, 0, 1, sender); 
      
        if(myNbWaitedAnswers == 0){
            if(isInPosition || CheckIfStuckOnClockwise())
                sendMessage("Sending an acknowledge message to my parent. ", new MessageOf<pair<string, pair<int, int>>>(DISTANCE_FORECAST_MSG_ID, make_pair("Parent", make_pair(0, myNextLeafId))),myParent,1000,100);
            else
                sendMessage("Sending an acknowledge message to my parent. ", new MessageOf<pair<string, pair<int, int>>>(DISTANCE_FORECAST_MSG_ID, make_pair("Parent", make_pair(myMaxDistanceToLeaf, myNextLeafId))),myParent,1000,100);
        }  
    }
    else{
        if(isInPosition || CheckIfStuckOnClockwise())
            sendMessage("Sending an acknowledge message to my sender. ", new MessageOf<pair<string, pair<int, int>>>(DISTANCE_FORECAST_MSG_ID, make_pair("Sender", make_pair(0, myNextLeafId))),sender,1000,100);
        else
            sendMessage("Sending an acknowledge message to my sender. ", new MessageOf<pair<string, pair<int, int>>>(DISTANCE_FORECAST_MSG_ID, make_pair("Sender", make_pair(myMaxDistanceToLeaf, myNextLeafId))),sender,1000,100);
    }

}

// 25.11.2021
bool ShapeReconfigurationBlockCode::CheckIfStuckOnClockwise(){
    HexanodesWorld *wrl = Hexanodes::getWorld();
    vector<HexanodesMotion*> tab = wrl->getAllMotionsForModule(module);
    vector<HexanodesMotion*>::const_iterator ci = tab.begin();
    while (ci!=tab.end() && (*ci)->direction!=CurrentDirection) {
        ci++;
    }
    if (ci!=tab.end()) {
        Cell3DPosition destination = (*ci)->getFinalPos(module->position);
        if(target && containsPosition(Hexanodes::getWorld()->obstacles, destination))
            return true;
    }
    else
        return true;
    return false;
}

void ShapeReconfigurationBlockCode::myDistanceAcknowledgeFunction(std::shared_ptr<Message> _msg, P2PNetworkInterface* sender){
    MessageOf<pair<string, pair<int, int>>>* msg = static_cast<MessageOf<pair<string, pair<int, int>>>*>(_msg.get());
    pair<string, pair<int, int>> msgg = *msg->getData();
    string str = msgg.first;
    pair<int, int> msgData = msgg.second;

    console << "I received an acknowledgement from: " << sender->getConnectedBlockId() << " with a distance to leaf d  = " << msgData.first << "\n";
    myNbWaitedAnswers--;

    if(str == "Parent")
        myChildren.push_back(sender);

    if(myMaxDistanceToLeaf < msgData.first){
        myMaxDistanceToLeaf = msgData.first;
        myNextLeafId = msgData.second;
    }     

    if(myNbWaitedAnswers == 0){
        if((isInPosition || CheckIfStuckOnClockwise() )&& myMaxDistanceToLeaf == myDistance )
            myMaxDistanceToLeaf = 0;
        if(myParent!=nullptr)
            sendMessage("Sending an acknowledge message to my parent. ", new MessageOf<pair<string, pair<int, int>>>(DISTANCE_FORECAST_MSG_ID, make_pair("Parent", make_pair(myMaxDistanceToLeaf, myNextLeafId))),myParent,1000,100);
        else{
            console << "I am the root and I have received all the messages. \n";
            console << "Now let's send a message to find the leader which is robot number " << myNextLeafId << " and make it move. \n";
            sendMessageToAllNeighbors("Sending the leader Id to my neighbors. ", new MessageOf<int>(LEADER_BROADCAST_MSG_ID, myNextLeafId),1000,100,0);
        }
    }
    
}

bool ShapeReconfigurationBlockCode::containsPosition(vector<Cell3DPosition> vectPos, Cell3DPosition pos){
    for(int i=0; i<vectPos.size(); i++)
        if(vectPos.at(i).pt[0] == pos.pt[0] && vectPos.at(i).pt[1] == pos.pt[1] && vectPos.at(i).pt[2] == pos.pt[2])
            return true;
    return false;
}

void ShapeReconfigurationBlockCode::myLeaderBroadcastFunction(std::shared_ptr<Message> _msg, P2PNetworkInterface* sender) {
    
    MessageOf<int>* msg = static_cast<MessageOf<int>*>(_msg.get());
    int msgData = *msg->getData();
    console << "I received a message leader Id = " <<  msgData  << " from " << sender->getConnectedBlockId() << "\n";

    if(module->blockId == msgData && isInMotion == false){
        isInMotion = true;
        isLeader = true;
        console << "I am the new leader !! Now I can start moving. \n";


        HexanodesWorld *wrl = Hexanodes::getWorld();
        vector<HexanodesMotion*> tab = wrl->getAllMotionsForModule(module);
        vector<HexanodesMotion*>::const_iterator ci = tab.begin();
        while (ci!=tab.end() && (*ci)->direction!=CurrentDirection) {
            ci++;
        }
        if (ci!=tab.end() && !isInPosition) {
            auto orient = (*ci)->getFinalOrientation(module->orientationCode);
            Cell3DPosition destination = (*ci)->getFinalPos(module->position);
            if(target && containsPosition(Hexanodes::getWorld()->obstacles, destination))
                robotAtObstacle = true;
            else
                scheduler->schedule(new HexanodesMotionStartEvent(scheduler->now(), module, destination, orient));
        }
    }
    else{
        for (unsigned int i=0; i<myChildren.size(); i++)
            sendMessage("Sending the leader Id to my child.", new MessageOf<int>(LEADER_BROADCAST_MSG_ID, msgData), myChildren.at(i),1000,100);
    }

}


void ShapeReconfigurationBlockCode::onMotionEnd() {
   
    nbMotions++;

    HexanodesWorld *wrl = Hexanodes::getWorld();
    vector<HexanodesMotion*> tab = wrl->getAllMotionsForModule(module);
    vector<HexanodesMotion*>::const_iterator ci = tab.begin();
    Cell3DPosition destination;
    while (ci != tab.end() && ((*ci)->direction != CurrentDirection)) {
      ci++;
    }
   
    if (ci != tab.end()) {
      destination = (*ci) -> getFinalPos(module->position);
    }

    if(initialPosition.pt[0] == destination.pt[0] && initialPosition.pt[1] == destination.pt[1]  && initialPosition.pt[2] == destination.pt[2] )
    	// console << "No more possible motions!\n";
        CurrentDirection = motionDirectionStatic::CCW;

    // else{
        if(target && containsPosition(Hexanodes::getWorld()->obstacles, destination))
        {
             //bio inspired

            if ((target && target->isInTarget(module->position)) || (target&&target->isInTarget(module->position)&& !target->isInTarget(destination))) {
                isInPosition = true;
                module->setColor(GREEN);
            }
            robotAtObstacle = true;
            myDistance = 0;
            myCurrentRound++;
            myChildren.clear();
            isLeader = false;
            myMaxDistanceToLeaf = 0;
            isInMotion = false;
            myParent = nullptr;
            console << "----------------------------------------------------------------\n";
            console << "Starting new round \n";
            myNbWaitedAnswers = sendMessageToAllNeighbors("Sending my distance and current round number. ", new MessageOf<pair<int,int>>(DISTANCE_BROADCAST_MSG_ID, make_pair(myDistance, myCurrentRound)),1000,100,0);

        }
        else{
            if ((target && target->isInTarget(module->position) && module->getNbNeighbors()>1) || (target&&target->isInTarget(module->position)&& !target->isInTarget(destination))) {
                isInPosition = true;
                module->setColor(GREEN);
            }

            if (isInPosition){
                myDistance = 0;
                myCurrentRound++;
                myChildren.clear();
                isLeader = false;
                myMaxDistanceToLeaf = 0;
                isInMotion = false;
                myParent = nullptr;
                console << "----------------------------------------------------------------\n";
                console << "Starting new round \n";
                myNbWaitedAnswers = sendMessageToAllNeighbors("Sending my distance and current round number. ", new MessageOf<pair<int,int>>(DISTANCE_BROADCAST_MSG_ID, make_pair(myDistance, myCurrentRound)),1000,100,0);
            }
            else{
                vector<HexanodesMotion*> tab = Hexanodes::getWorld()->getAllMotionsForModule(module);
                auto ci=tab.begin();
                
                while (ci!=tab.end() && ((*ci)->direction!=CurrentDirection)) {
                    ci++;
                }

                if (ci!=tab.end()) {
                    Cell3DPosition destination = (*ci)->getFinalPos(module->position);
                    auto orient = (*ci)->getFinalOrientation(module->orientationCode);
                    scheduler->schedule(new HexanodesMotionStartEvent(scheduler->now(), module, destination, orient));

                }
            }
        }
    // }

}

void ShapeReconfigurationBlockCode::processLocalEvent(EventPtr pev) {
    std::shared_ptr<Message> message;
    stringstream info;

    // Do not remove line below
    HexanodesBlockCode::processLocalEvent(pev);

    switch (pev->eventType) {
        case EVENT_ADD_NEIGHBOR: {
            // Do something when a neighbor is added to an interface of the module
            break;
        }

        case EVENT_REMOVE_NEIGHBOR: {
            // Do something when a neighbor is removed from an interface of the module
            break;
        }
    }
}

string ShapeReconfigurationBlockCode::onInterfaceDraw() {
    stringstream trace;
    trace << "Number of modules: "+ to_string(Hexanodes::getWorld()->maxBlockId)<< " and ";
    trace << "Number of motions: " + to_string(nbMotions)<< "\n";
    if(robotAtObstacle)
        trace << msg << "\n";
    return trace.str();
}

void ShapeReconfigurationBlockCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("leader");
    if (attr!=nullptr) {
        console << "--------------------Leader-----------------\n";
        string str(attr);
        if (str=="true" || str=="1" || str=="yes") {
            isLeader=true;
            std::cout << module->blockId << " is Leader!" << std::endl;
        }
    }

    // const char *attr2 = config->Attribute("obstacle");
    // if (attr2!=nullptr) {
    //     console << "--------------------obstacle-----------------\n";
    //     string str2(attr2);
    //     if (str2=="true" || str2=="1" || str2=="yes") {
    //         Cell3DPosition position;
    //         const char *attr3 = config->Attribute("position");
    //         if (attr3 != nullptr) {
    //             string str3(attr3);
    //             int pos1 = str3.find_first_of(','),
    //             pos2 = str3.find_last_of(',');
    //             position.pt[0] = atoi(str3.substr(0,pos1).c_str());
    //             position.pt[1] = atoi(str3.substr(pos1+1,pos2-pos1-1).c_str());
    //             position.pt[2] = atoi(str3.substr(pos2+1,str3.length()-pos1-1).c_str());
    //         } else {    
    //             stringstream error;
    //             error << "position attribute missing for target cell" << "\n";
    //             throw ParsingException(error.str());
    //         }

    //         // Cell3DPosition temPos = Cell3DPosition(position);
    //         obstacles.push_back(position);
    //         console << "--------------------pushing-----------------\n";
    //     }
    // }
}
