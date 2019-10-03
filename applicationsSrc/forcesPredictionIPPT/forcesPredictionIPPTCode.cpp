#include "forcesPredictionIPPTCode.h"

const int messageDelay=50;
const int messageDelayError=5;
const int messageDelayCons=1;

int nofIterations = 1000; // max number of iterations
double globalMass = 61.106/1000; //mass from XML
double globalE = 100; // E from XML // Young modulus MPa
double globalL=40; //length from XML // arm length mm
double globala = 40; //width of the square-cross-section arm  mm //
double globalA=globala*globala; //cross sectional area from XML mm^2
double globalI=pow(globala,4)/12.; // second moment of area from XML mm^4
double globalIz = globalI; //second moment of area
double globalIy = globalI; //second moment of area
double globalNu = 0.3; //Poisson ratio
double globalJ=2.25*pow((globala/2),4); //torsion constant

double globalGrav=9.81; //gravity from XML
double globalBeta=2/3.; //beta from XML

double globalOmega = 2./3; // weight of Jacobi method
double globalMu = 0.1; //friction coefficient
double globalEps = pow(10,-8); // //tolerance
double globalGamma = pow(10,-6); //stiffness reduction multiplier (for unilateral contact)
double globalSupportZ = 0; //Z coordinate of the bottom modules (contacting with the support)

bool ForcesPredictionIPPTCode::parseUserCommandLineArgument(int argc, char *argv[]) {
    /* Reading the command line, make sure that options do not overlap with the
     *  ones from the simulator itself (check with -h)
     */
    if ((argc > 0) && (argv[0][0] == '-')) {
        switch(argv[0][1]) {
            case 'b':
                argc--;
                argv++;

                // example
                cout << "Custom CLI option B enabled" << endl;

                return true;
        }
    }

    return false;
}

void ForcesPredictionIPPTCode::onBlockSelected() {
    // Do something when ctrl->clicking a module from the GUI
    cout << "Module #" << module->blockId << " was clicked at t = "
         << scheduler->now() << endl;
}

void ForcesPredictionIPPTCode::onAssertTriggered() {
    // see VS_ASSERT and VS_ASSERT_MSG in simulatorCore/src/utils.h
    cout << "Module #" << module->blockId << " triggered a VisibleSim assertion at t = "
         << scheduler->now() << endl;
}

/* parse XML files extra data */
/* be carefull, is run only one time by the first module! */
void ForcesPredictionIPPTCode::parseUserElements(TiXmlDocument* config) {
    TiXmlNode *node = config->FirstChild("parameters");

    cerr << "blockId=" << module->blockId << endl;
    TiXmlElement* element = node->ToElement();
    const char *attr= element->Attribute("globalSupportZ");
    //mass of module
    if (attr) {
        string str=attr;
        globalSupportZ = atof(str.c_str());
        cerr << "globalSupportZ= " << globalSupportZ << endl;
    } else {
            OUTPUT << "WARNING No globalSupportZ in XML file" << endl;
    }

    attr= element->Attribute("globalGamma");
    //mass of module
    if (attr) {
        string str=attr;
        globalGamma = atof(str.c_str());
        cerr << "globalGamma= " << globalGamma << endl;
    } else {
            OUTPUT << "WARNING No globalGamma in XML file" << endl;
    }

    attr= element->Attribute("globalEps");
    //mass of module
    if (attr) {
        string str=attr;
        globalEps = atof(str.c_str());
        cerr << "globalEps= " << globalEps << endl;
    } else {
            OUTPUT << "WARNING No globalEps in XML file" << endl;
    }

    attr= element->Attribute("globalMu");
    //mass of module
    if (attr) {
        string str=attr;
        globalMu = atof(str.c_str());
        cerr << "globalMu= " << globalMu << endl;
    } else {
            OUTPUT << "WARNING No globalMu in XML file" << endl;
    }

    attr= element->Attribute("globalOmega");
    if (attr) {
        string str=attr;
        globalOmega = atof(str.c_str());
        cerr << "globalOmega= " << globalOmega << endl;
    } else {
            OUTPUT << "WARNING No globalOmega in XML file" << endl;
    }

    attr= element->Attribute("globalMass");
    if (attr) {
        string str=attr;
        globalMass = atof(str.c_str());
        cerr << "Mass= " << globalMass << endl;
    } else {
            OUTPUT << "WARNING No mass in XML file" << endl;
    }

    attr= element->Attribute("nofIterations");
    if (attr) {
	string str=attr;
	nofIterations = atoi(str.c_str());
	cerr << "maxNofIterations= " << nofIterations << endl;
    } else {
	OUTPUT << "WARNING No maxNofIterations in XML file" << endl;
    }

    attr= element->Attribute("a");
    if (attr) {
        string str=attr;
        globala = atof(str.c_str());
        cerr << "a= " << globala << endl;
    } else {
            OUTPUT << "WARNING No a in XML file" << endl;
    }


    attr= element->Attribute("E");
        if (attr) {
            string str=attr;
            globalE = atof(str.c_str());
            cerr << "E= " << globalE << endl;
    } else {
            OUTPUT << "WARNING No E in XML file" << endl;
    }

    attr= element->Attribute("A");
        if (attr) {
            string str=attr;
            globalA = atof(str.c_str());
            cerr << "A= " << globalA << endl;
    } else {
            OUTPUT << "WARNING No A in XML file" << endl;
    }

    attr= element->Attribute("globalL");
        if (attr) {
            string str=attr;
            globalL = atof(str.c_str());
            cerr << "globalL= " << globalL << endl;
    } else {
            OUTPUT << "WARNING No globalL in XML file" << endl;
    }

    attr= element->Attribute("globalI");
                    if (attr) {
                        string str=attr;
                        globalI = atof(str.c_str());
                        cerr << "globalI= " << globalI << endl;
                } else {
                        OUTPUT << "WARNING No globalI in XML file" << endl;
                }

    attr= element->Attribute("globalBeta");
                    if (attr) {
                        string str=attr;
                        globalBeta = atof(str.c_str());
                        cerr << "globalBeta= " << globalBeta << endl;
                } else {
                        OUTPUT << "WARNING No globalBeta in XML file" << endl;
                }

    attr= element->Attribute("globalIz");
                        if (attr) {
                            string str=attr;
                            globalIz = atof(str.c_str());
                            cerr << "globalIz= " << globalIz << endl;
                    } else {
                            OUTPUT << "WARNING No globalIz in XML file" << endl;
                    }

    attr= element->Attribute("globalIy");
                        if (attr) {
                            string str=attr;
                            globalIy = atof(str.c_str());
                            cerr << "globalIy= " << globalIy << endl;
                    } else {
                            OUTPUT << "WARNING No globalIy in XML file" << endl;
                    }

    attr= element->Attribute("globalNu");
                        if (attr) {
                            string str=attr;
                            globalNu = atof(str.c_str());
                            cerr << "globalNu= " << globalNu << endl;
                    } else {
                            OUTPUT << "WARNING No globalNu in XML file" << endl;
                    }

    attr= element->Attribute("globalJ");
                        if (attr) {
                            string str=attr;
                            globalJ = atof(str.c_str());
                            cerr << "globalJ= " << globalJ << endl;
                    } else {
                            OUTPUT << "WARNING No globalJ in XML file" << endl;
                    }


}

void ForcesPredictionIPPTCode::SetNeighbors(){
    //set 0 for all empty neighbors
    for(int i=0;i<6;i++){
        neighbors[i][0] =0;
        neighbors[i][1] =0;
        tree_child[i]=0;
        aggregationCompleted[i]=true;
    }
    tree_par=0;

    //taking neighbors and adding them to our table

    //p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,0,1));
    P2PNetworkInterface *p2p = module->getInterface(SCLattice::Direction::Top);
    //if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
    if(p2p->getConnectedBlockBId()) {
        neighbors[0][0]=p2p->getConnectedBlockBId();
    } else if(target->isInTarget(module->position+Cell3DPosition(0,0,1))) {
        neighbors[0][1]=2; // virtual module
    }

    //p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,0,-1));
    p2p = module->getInterface(SCLattice::Direction::Bottom);
    //if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
    if(p2p->getConnectedBlockBId()) {
        neighbors[1][0]=p2p->getConnectedBlockBId();
    } else if(target->isInTarget(module->position+Cell3DPosition(0,0,-1))) {
        neighbors[1][1]=2; // virtual module
    }

    //P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(-1,0,0));
    p2p = module->getInterface(SCLattice::Direction::Left);
    //if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
    if(p2p->getConnectedBlockBId()) {
        neighbors[2][0]=p2p->getConnectedBlockBId();
    } else if(target->isInTarget(module->position+Cell3DPosition(-1,0,0))) {
        neighbors[2][1]=2; // virtual module
    }

    //p2p = module->getInterface(Cell3DPosition(1,0,0));
    p2p = module->getInterface(SCLattice::Direction::Right);
    //if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
    if(p2p->getConnectedBlockBId()) {
        neighbors[3][0]=p2p->getConnectedBlockBId();
    } else if(target->isInTarget(module->position+Cell3DPosition(1,0,0))) {
        neighbors[3][1]=2; // virtual module
    }

    //p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,-1,0));
    p2p = module->getInterface(SCLattice::Direction::Front);
    //if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
    if(p2p->getConnectedBlockBId()) {
        neighbors[4][0]=p2p->getConnectedBlockBId();
    } else if(target->isInTarget(module->position+Cell3DPosition(0,-1,0))) {
        neighbors[4][1]=2; // virtual module
    }

    //p2p = module->getP2PNetworkInterfaceByRelPos(Cell3DPosition(0,1,0));
    p2p = module->getInterface(SCLattice::Direction::Back);
    //if(p2p->getConnectedBlockBId()!=-1) { // WARNING p2p->getConnectedBlockBId returns a unsigned int ! and 0 if no block is connected
    if(p2p->getConnectedBlockBId()) {
        neighbors[5][0]=p2p->getConnectedBlockBId();
    } else if(target->isInTarget(module->position+Cell3DPosition(0,1,0))) {
        neighbors[5][1]=2; // virtual module
    }

}

void ForcesPredictionIPPTCode::printNeighbors() {
    OUTPUT << "neighbors for id= " << module->blockId << ": "<< endl;
    for(int i=0;i<6;i++){
        OUTPUT<<neighbors[i][0] << ", ";
    }
    OUTPUT << endl;
    for(int i=0;i<6;i++){
            OUTPUT<<neighbors[i][1] << ", ";
    }
    OUTPUT << endl;
    OUTPUT << endl;
}


void ForcesPredictionIPPTCode::parseUserBlockElements(TiXmlElement* config) {
    cerr << "blockId=" << module->blockId << endl;
    console << "blockId=" << module->blockId << "\n";

    const char *attr = config->Attribute("fixed");
    if (attr) {
        cerr << "fixed =" << attr<< endl;
        isFixed=true;
    }

    attr = config->Attribute("centroid");
    if (attr) {
        cerr << "centroid =" << attr<< endl;
        isCentroid=true; // Warning: be careful to indicate exactly ONE centroid module in the structure
    } else isCentroid=false;

    attr = config->Attribute("myAttribute");
    if (attr) {
        cerr << "myAttribute =" << attr<< endl;
    }
}

void ForcesPredictionIPPTCode::startup() {
    addMessageEventFunc(TREE_MSG,_treeMessage);
    addMessageEventFunc(TREE_CONF_MSG,_treeConfMessage);
    addMessageEventFunc(CM_Q_MSG,_cmQMessage);
    addMessageEventFunc(CM_R_MSG,_cmRMessage);
    addMessageEventFunc(DU_INIT_MSG,_duInitMessage);
    addMessageEventFunc(DU_MSG,_duMessage);
    addMessageEventFunc(DU_COMPLETE_MSG,_duCompleteMessage);
    addMessageEventFunc(SST_Q_MSG,_sstQMessage);
    addMessageEventFunc(SST_R_MSG,_sstRMessage);
    addMessageEventFunc(MST_Q_MSG,_mstQMessage);
    addMessageEventFunc(MST_R_MSG,_mstRMessage);

    console << "---start " << module->blockId << "," << module->color << "----\n";
    //set attributes from xml file
    mass = globalMass;
    E = globalE;
    L = globalL;
    a = globala;
    A = globalA;
    I = globalI;

    Iz =  globalIz;
    Iy = globalIy;
    nu = globalNu;
    J = globalJ;

    grav=globalGrav;
    beta = globalBeta;


    Omega = globalOmega; // weight of Jacobi method
    Mu = globalMu; //friction coefficient
    Eps = globalEps; // //tolerance
    Gamma = globalGamma; //stiffness reduction multiplier (for unilateral contact)
//	supportZ = globalSupportZ; //Z coordinate of the bottom modules (contacting with the support)

//    cmd = new cmData;
    if(module->position[2]==globalSupportZ) isSupport=true;


    //cheking neighbors and adding them to a list
    SetNeighbors();
    //CheckNeighbors();

    //check is module fixed
    if(isFixed) {
        module->setColor(RED);
    }


//	//check is modue support
//	support = isSupport(module);

    //createK11(K11);
    //createK12(K12);

    //setting of the mass force vector
    Fp=orient*grav*mass;
    //printVector(Fp);

/*
    //first step - calculate DU and sends to neighbor (to initiate the procedure)
    if(curIteration == 0) {
        computeDU(true);
        curIteration++;
    }
*/

    console << "isCentroid=" << (int)(isCentroid) << "\n";
    if(isCentroid) {
        bool anyMsgSent=false;
        module->setColor(YELLOW);
        maxIterations=nofIterations;
        for(int i=0;i<6;i++) {
            if(neighbors[i][0]>0) {
                // after sending the messages to neighbors, we wait for confirmations before accepting a neighbor as a child
                P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
                if(p2p) {
                    sendMessage("TREE_MSG",new Message(TREE_MSG),p2p,messageDelay,messageDelayError);
                    anyMsgSent=true;
                    aggregationCompleted[i]=false;
                    tree_child[i]=1; // provisionally accepted as a child (but waiting for a confirmation)
                }
            } else if(neighbors[i][1]==2) {
                // virtual module is always a child
                tree_child[i]=1;
            }
        }
        if(!anyMsgSent) { // the trivial situation in which the centroid has only virtual children
            // !!!!!! we skip that trivial situation -- to be implemented in future
        }
    }
}


////////////////////////////////////////////////////
///////////////// MESSAGES /////////////////////////
////////////////////////////////////////////////////


// build a tree
void ForcesPredictionIPPTCode::treeMessage(P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    console << "treeMessage " << msgFrom << "->" << module->blockId << "\n";

    if(tree_par!=0 || isCentroid) { // module is already a child (has a parrent) or is a centroid
        sendMessage("TREE_CONF_MSG",new MessageOf<int >(TREE_CONF_MSG,0),sender,messageDelay,messageDelayError);
    } else { // there were no earlier requests to be a child
        tree_par=msgFrom;
        bool anyMsgSent=false;
        for(int i=0;i<6;i++) {
            if(neighbors[i][0]>0 && neighbors[i][0]!=tree_par) { // virtual modules are not counted here. They are children by definition.
                P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
                if (p2p) {
                    sendMessage("TREE_MSG",new Message(TREE_MSG),p2p,messageDelay,messageDelayError);
                    aggregationCompleted[i]=false;
                    anyMsgSent=true;
                    tree_child[i]=1; // the node is provisionally accepted as a child
                }
                // after sending the messages to neighbors, we wait for confirmations before accepting a neighbor as a child
            }
        }
        if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then return the confirmation
            sendMessage("TREE_CONF_MSG",new MessageOf<int>(TREE_CONF_MSG,1),sender,messageDelay,messageDelayError);
        }
    }
}

// confirm/reject parent-child connection
void ForcesPredictionIPPTCode::treeConfMessage(const MessageOf<int>*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    int childConfirmed = *msg->getData();

    console << "treeConfMessage(" << childConfirmed <<") " << msgFrom << "->" << module->blockId << "\n";
    bool aggrCompl=true;
    for(int i=0;i<6;i++) {
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
            tree_child[i]=childConfirmed;
        }
        if(!aggregationCompleted[i]) aggrCompl=false;
    }
    if(aggrCompl) {
        if(!isCentroid) {
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par);
            if(p2p) sendMessage("TREE_CONF_MSG",new MessageOf<int>(TREE_CONF_MSG,1),p2p,messageDelay,messageDelayError);
        } else { // centroid initiates the center of mass query for all non-virtual children (!!!the case in which there are no real children is not supported!!!)
            cmd.mX=module->position[0] * mass;
            cmd.mY=module->position[1] * mass;
            cmd.m=mass;
            const double dir[6][3]={{0,0,1},{0,0,-1},{-1,0,0},{1,0,0},{0,-1,0},{0,1,0}}; // 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 
            for(int i=0; i<6; i++) {
                if(tree_child[i]==1) { // if non-virtual child
                    P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
                    if(p2p) {
                        sendMessage("CM_Q_MSG",new Message(CM_Q_MSG),p2p,messageDelay,messageDelayError);
                        aggregationCompleted[i]=false;
                    }
                } else if(neighbors[i][1]==2) {
                    cmd.mX=cmd.mX+(module->position[0]+dir[i][0])*mass;
                    cmd.mY=cmd.mY+(module->position[1]+dir[i][1])*mass;
                    cmd.m=cmd.m+mass;
                }
            }
        }
    }
}

void ForcesPredictionIPPTCode::cmQMessage(P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    console << "CM_Q_Message " << msgFrom << "->" << module->blockId << "\n";

    bool anyMsgSent=false;
    cmd.mX=module->position[0] * mass;
    cmd.mY=module->position[1] * mass;
    cmd.m=mass;
    const double dir[6][3]={{0,0,1},{0,0,-1},{-1,0,0},{1,0,0},{0,-1,0},{0,1,0}}; // 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 
    for(int i=0;i<6;i++) {
        if(tree_child[i]==1) { // if non-virtual child
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
            if (p2p) {
                sendMessage("CM_Q_MSG",new Message(CM_Q_MSG),p2p,messageDelay,messageDelayError);
                aggregationCompleted[i]=false;
                anyMsgSent=true;
            }
            // after sending the messages to neighbors, we wait for confirmations before aggregating the info
        } else if(neighbors[i][1]==2) {
            cmd.mX=cmd.mX+(module->position[0]+dir[i][0])*mass;
            cmd.mY=cmd.mY+(module->position[1]+dir[i][1])*mass;
            cmd.m=cmd.m+mass;
        }
    }
    if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then aggregate the data and return the confirmation
        sendMessage("CM_R_MSG",new MessageOf<cmData >(CM_R_MSG,cmd),sender,messageDelay,messageDelayError);
    }
}

void ForcesPredictionIPPTCode::cmRMessage(const MessageOf<cmData>*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    cmData cmdL = *msg->getData();

    console << "CM_R_Message(" << cmdL.mX << ", " << cmdL.mY << ", " << cmdL.m <<") " << msgFrom << "->" << module->blockId << "\n";
    bool aggrCompl=true;
//    const double dir[6][3]={{0,0,1},{0,0,-1},{-1,0,0},{1,0,0},{0,-1,0},{0,1,0}}; // 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1 
    for(int i=0;i<6;i++) { // aggregation of info from the child
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
            cmd.mX=cmd.mX+cmdL.mX;
            cmd.mY=cmd.mY+cmdL.mY;
            cmd.m=cmd.m+cmdL.m;
        }
        if(!aggregationCompleted[i]) aggrCompl=false;
    }
    if(aggrCompl) {
        if(!isCentroid) {
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par);
            if(p2p) sendMessage("cM_R_MSG",new MessageOf<cmData >(CM_R_MSG,cmd),p2p,messageDelay,messageDelayError);
        } else { // centroid initiates the weighted-Jacobi iterations
            for(int i=0; i<6; i++) {
                if(tree_child[i]) { // if non-virtual child
                    P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
                    if(p2p) {
                        sendMessage("DU_INIT_MSG",new MessageOf<int >(DU_INIT_MSG,maxIterations),p2p,messageDelay,messageDelayError);
                        aggregationCompleted[i]=false;
                    }
                }
            }
        }
    }
}

void ForcesPredictionIPPTCode::duInitMessage(const MessageOf<int>*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    maxIterations = *msg->getData();
    console << "DU_INIT_Message(" << maxIterations << ") " << msgFrom << "->" << module->blockId << "\n";

    for(int i=0;i<6;i++) {
        if(tree_child[i]==1) { // if non-virtual child
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
            if (p2p) {
                sendMessage("DU_INIT_MSG",new MessageOf<int >(DU_INIT_MSG, maxIterations),p2p,messageDelay,messageDelayError);
                aggregationCompleted[i]=false;
            }
        }
    }
    computeDU(true); // initializing call of computeDU
    curIteration++;
}


void ForcesPredictionIPPTCode::duMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    vector<double> msgData = *msg->getData();

    if(curIteration > maxIterations)
        return;
    for(int i=0;i<6;i++){
        if(neighbors[i][0]==msgFrom) {
            OUTPUT << "Iter=" << curIteration  <<  ", ID="<< module->blockId << " received the message from " << msgFrom<< endl;
            printVector(msgData,6,"msgData from "+to_string(msgFrom)+" to "+ to_string(module->blockId));
            neighbors[i][1]=1;
            uq[i]=msgData;
        }
    }
    //checking if there are all messages
    bool ready = true;
    for(int i = 0;i<6;i++ ) {
        if(neighbors[i][0]!=0 && neighbors[i][1]==0)
            ready = false;
    }
    printNeighbors();
    if(ready) {
        OUTPUT << "Calculating du"<< endl;
        computeDU();
        //visualisation
        if(curIteration%100==0) {
            visualization();
            cout << "Current Iteration = "<< curIteration<< endl;
        }
        if(curIteration==maxIterations) {
            if(isSupport) {
                module->setColor(Color(0.0f,0.0f,1.0f));
            }
            visualization();
        }
        curIteration++;
        dup=du;
        if(curIteration > maxIterations) {
            bool aggrCompl=true;
            for(int i=0;i<6;i++) { // aggregation of info from the child
                if(!aggregationCompleted[i]) aggrCompl=false;
            }
            if(aggrCompl) {
                if(!isCentroid) {
                    P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par);
                    if(p2p) sendMessage("DU_COMPLETE_MSG",new Message(DU_COMPLETE_MSG),p2p,messageDelay,messageDelayError);
                } else { // centroid initiates the module-based stability check
                    for(int i=0; i<6; i++) {
                        if(tree_child[i]) { // if non-virtual child
                            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
                            if(p2p) {
                                sendMessage("MST_Q_MSG",new Message(MST_Q_MSG),p2p,messageDelay,messageDelayError);
                                aggregationCompleted[i]=false;
                            }
                        }
                    }
                }
            }
        }
    }
}

void ForcesPredictionIPPTCode::duCompleteMessage(P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();

    console << "DU_COMPLETE_Message " << msgFrom << "->" << module->blockId << "\n";
    bool aggrCompl=true;
    for(int i=0;i<6;i++) { // aggregation of info from the child
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
        }
        if(!aggregationCompleted[i]) aggrCompl=false;
    }
    if(aggrCompl && curIteration > maxIterations) {
        if(!isCentroid) {
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par);
            if(p2p) sendMessage("DU_COMPLETE_MSG",new Message(DU_COMPLETE_MSG),p2p,messageDelay,messageDelayError);
        } else { // centroid initiates the module-based stability check
            for(int i=0; i<6; i++) {
                if(tree_child[i]) { // if non-virtual child
                    P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
                    if(p2p) {
                        sendMessage("MST_Q_MSG",new Message(MST_Q_MSG),p2p,messageDelay,messageDelayError);
                        aggregationCompleted[i]=false;
                    }
                }
            }
        }
    }
}


void ForcesPredictionIPPTCode::sstQMessage(const MessageOf<sstData>*msg,P2PNetworkInterface *sender) {
}

void ForcesPredictionIPPTCode::sstRMessage(const MessageOf<sstData>*msg,P2PNetworkInterface *sender) {
}

void ForcesPredictionIPPTCode::mstQMessage(P2PNetworkInterface *sender) {
}

void ForcesPredictionIPPTCode::mstRMessage(const MessageOf<mstData>*msg,P2PNetworkInterface *sender) {
}



void ForcesPredictionIPPTCode::clearNeighborsMessage() {
    for(int i=0; i<6; i++) {
        if(neighbors[i][1]==1) {
            neighbors[i][1]=0;
        }
    }
}


void _treeMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    cb->treeMessage(sender);
}

void _treeConfMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
    cb->treeConfMessage(msgType,sender);
}

void _cmQMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    cb->cmQMessage(sender);
}

void _cmRMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<cmData>*msgType = (MessageOf<cmData>*)msg.get();
    cb->cmRMessage(msgType,sender);
}

void _duInitMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<int>*msgType = (MessageOf<int>*)msg.get();
    cb->duInitMessage(msgType,sender);
}

void _duMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<vector<double> >*msgType = (MessageOf<vector<double> >*)msg.get();
    cb->duMessage(msgType,sender);
}

void _duCompleteMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    cb->duCompleteMessage(sender);
}

void _sstQMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<sstData>*msgType = (MessageOf<sstData>*)msg.get();
    cb->sstQMessage(msgType,sender);
}

void _sstRMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<sstData>*msgType = (MessageOf<sstData>*)msg.get();
    cb->sstRMessage(msgType,sender);
}

void _mstQMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    cb->mstQMessage(sender);
}

void _mstRMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<mstData>*msgType = (MessageOf<mstData>*)msg.get();
    cb->mstRMessage(msgType,sender);
}


/*
bool ForcesPredictionIPPTCode::isFixed(BlinkyBlocksBlock *modR){
    if(target->isInTarget(modR->position)){
        modR->setColor(Color(0.8f,0.8f,0.6f));
        return true;
    }else
        return false;

}
*/


/*
bool ForcesPredictionIPPTCode::isSupport(BlinkyBlocksBlock *modR){
    if(modR->position[2]==supportZ){
        return true;
    }
    return false;
}
*/


/////////////////////////////////////////////////////////////////////////////
////////// Weighted-Jacobi iterations and visualization /////////////////////
/////////////////////////////////////////////////////////////////////////////

vector< vector<double> > ForcesPredictionIPPTCode::contactStiffnessMatrix(vector<double> &dup) {
    vector< vector<double> > K11d = createK11(1); // stiffness matrix for DOWN direction
    vector< vector<double> > TfrB, TmxB, TmyB, TmzB;
    vector< double > Fd = K11d*dup;
    double fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4], mz=Fd[5]; // elastic predictor

    if(fz<0) { // contact
        double mmax = -fz*a/2;
        double fmax = -Mu*fz;

        // condition for frictional sliding
        if(sqrt(fx*fx+fy*fy)<fmax) { // frictional stick state
            TfrB = IdentityMatrix6();
        } else { // frictionaal slip state
            if(sqrt(fx*fx+fy*fy)<Eps) { // near zero tangential force -> Mu is probably very low -> stiffness matrix for sliding = 0
                TfrB = createTfr(0,0,0,0,0,0);
            } else { // frictional sliding (radial return on the Coulomb friction cone
                TfrB = createTfr(
                                -fy*fy*fz*Mu/pow(fx*fx+fy*fy,3.0/2.0),
                                fx*fy*fz*Mu/pow(fx*fx+fy*fy,3.0/2.0),
                                -fx*Mu/pow(fx*fx+fy*fy,1.0/2.0),
                                fx*fy*fz*Mu/pow(fx*fx+fy*fy,3.0/2.0),
                                -fx*fx*fz*Mu/pow(fx*fx+fy*fy,3.0/2.0),
                                -fy*Mu/pow(fx*fx+fy*fy,1.0/2.0)
                                );
                    }
                }

        // condition for x-tilting (over the y-directed edge (front or back))
        if(fabs(mx) < mmax) { // stable bending
            TmxB = IdentityMatrix6();
        } else { // unstable bending (tilting over the y-directed edge)
            if(fabs(mx)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
                TmxB = createTmx(0);
            } else { // tilting occurs
                TmxB = createTmx(-sign(mx)*a/2.0);
            }
        }

        // condition for y-tilting (over the x-directed edge (left or right))
        if(fabs(my) < mmax) { // stable bending
            TmyB = IdentityMatrix6();
        } else { // unstable bending (tilting over the y-directed edge)
            if(fabs(my)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
                TmyB = createTmy(0);
            } else { // tilting occurs
                TmyB = createTmy(-sign(my)*a/2.0);
            }
        }

        // condition for x-tilting (over the y-directed edge (front or back))
        if(fabs(mz) < mmax) { // stable bending
            TmzB = IdentityMatrix6();
        } else { // unstable bending (tilting over the y-directed edge)
            if(fabs(mz)<Eps) { // near-zero torque -> fz is near zero -> stiffness matrix for x-bending will be zero
                TmzB = createTmz(0);
            } else { // tilting occurs
                TmzB = createTmz(-sign(mz)*Mu);
            }
        }
        K11d=TfrB*TmxB*TmyB*TmzB*K11d;
    } else { // separation
        K11d=Gamma*K11d;
    }
    return K11d;
}

void ForcesPredictionIPPTCode::computeNeighborDU(int i) {

    int di=1-2*(i%2); // switches the sides (up<->down, left<->right, front<->back)
    vector< vector<double> > sumK11 = createK11(i+di);
    vector< double > Fpq = createK12(i+di)*dup;

    if(isSupport && i>1) { // enforce the unilateral contact conditions with the support, located below the module only if the neighbor is on left, right, back or front
        sumK11=sumK11+contactStiffnessMatrix(uq[i]);
    }
    uq[i] = RevD(sumK11)*beta*(Fp-createR(sumK11)*uq[i]-Fpq)+(uq[i]*(1-beta));

    printVector(uq[i],6,"neighbor vct uq["+ to_string(i) +"] id= " + to_string(module->blockId) + ", it "+ to_string(curIteration));
}


void ForcesPredictionIPPTCode::computeDU(bool isInit) {
//    if(module->blockId == 8) console << "------ module 8 -----\n";
    console << "computeDU, module="<< module->blockId <<", iter=" << curIteration << ", maxIter=" << maxIterations << "\n";
    if(isInit) { // initialization call 
        sendMessageToAllNeighbors("DU_MSG",new MessageOf<vector<double> >(DU_MSG,du),messageDelay,messageDelayError,0);
        return;
    }
    //temporary Matrixes
    vector< vector<double> > sumK11 = decltype(sumK11)(vectorSize, vector<double>(vectorSize,0));
    vector< double > Fpq = decltype(Fpq)(vectorSize,0);
    vector< vector<double> > mtmp = decltype(sumK11)(vectorSize, vector<double>(vectorSize,0));
    if(!isFixed) {
        //checking neighbors and creating K11 and K12 matrixes
        for(int i=0;i<6;i++){
            if(neighbors[i][1]==2) computeNeighborDU(i); // compute uq[i] for a virtual module
            if(neighbors[i][1]>0) {
                sumK11=sumK11+createK11(i);
                Fpq = Fpq+(createK12(i)*uq[i]);
                if(module->blockId == 8) {
		    mtmp = createK12(i);
		    printMatrix(mtmp,6,6,"CreateK12("+to_string(i)+")");
		    printVector(uq[i],6,"uq["+to_string(i)+"]");
                }
            }
        }
//		printVector(Fpq,6,"Fpq");
        if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
            sumK11=sumK11+contactStiffnessMatrix(dup);
        }
/*        if(module->blockId == 8) {
            printMatrix(sumK11,6,6,"SumK11");
        }*/
//		printMatrix(sumK11,6,6,"SumK11");
        du = RevD(sumK11)*beta*(Fp-createR(sumK11)*dup-Fpq)+(dup*(1-beta));
/*		vector<double> vtmp = (dup*(1-beta));
        printVector(vtmp,6,"dup*(1-beta)");
        printVector(Fp,6,"Fp");
        printVector(Fpq,6,"Fqp");
        vtmp = (Fp-createR(sumK11)*dup-Fpq);
        printVector(vtmp,6,"(Fp-createR(sumK11)*dup-Fpq)");
        */
        printVector(du,6,"vector du module id= " + to_string(module->blockId) + ", iteration "+ to_string(curIteration));

    }	else { //end isFixed
        du = du*0.;
    }

    //sending message to neighbors with du
//    OUTPUT << "size=" << du.size() << endl;
    sendMessageToAllNeighbors("DU_MSG",new MessageOf<vector<double> >(DU_MSG,du),messageDelay,messageDelayError,0);
    //clearing info about du from neighbors
    clearNeighborsMessage();
}

void ForcesPredictionIPPTCode::visualization() {

    //calculate only if not fixed
    if(isFixed)
        return;

//    double fxMaxV = 20.5*grav*mass; // max force in N (for up and down direction)
//    double fxMaxL = 25.5*grav*mass; // max force in N (for lateral directions)
    double fxMaxV = 11.98; // max force in N (for up and down direction)
    double fxMaxL = 14.97; // max force in N (for lateral directions)

    bMatrix tmpK11 = decltype(tmpK11)(vectorSize, vector<double>(vectorSize,0));
    bMatrix tmpK12 = decltype(tmpK12)(vectorSize, vector<double>(vectorSize,0));

//	bMatrix R = decltype(R)(vectorSize, vector<double>(vectorSize));


    for(int i=0;i<6;i++) {
        if(neighbors[i][0]!=0 and !isFixed) {
            tmpK11 = createK11(i);
            tmpK12 = createK12(i);

            vizTable[i]=createRot(i)*(tmpK11*dup+tmpK12*uq[i]);

            // the torque should be averaged between the neighbours (i.e. it is taken from the middle of the beam)
            int di=1-2*(i%2);
            tmpK11 = createK11(i+di);
            tmpK12 = createK12(i+di);
            vizTable[i][4]=(vizTable[i][4]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[4])/2;
            vizTable[i][5]=(vizTable[i][5]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[5])/2;
        }
    }
    double maxS = 0;
    if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
        vector<double> Fd=contactStiffnessMatrix(dup)*dup;
        double fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4], mz=Fd[5]; // in global coordinates
        vizTable[1][0]=fx;  vizTable[1][1]=fy;  vizTable[1][2]=fz;  vizTable[1][3]=mx;  vizTable[1][4]=my;  vizTable[1][5]=mz;
        if(fz<0) { // if in contact then check the condition on torque
            maxS = min(1.,max(abs(mx)/abs(fz*L/2),abs(my)/abs(fz*L/2)));
        } else { // if no contact then possible lost of balance
            maxS =1.;
        }
    }

    printMatrix(vizTable,6,6,"VizTable "+to_string(module->blockId));

    // chacking the maximum load factor
    for(int i = 0; i<6; i++) {
        double fxMeff=((i<2)?fxMaxV:fxMaxL); // effective value of fxMax depending on the connection's type (vertical or lateral)

        if(neighbors[i][0]!=0 and !isFixed) {

            //set abs values of my and mz
            vizTable[i][4] = abs(vizTable[i][4]);
            vizTable[i][5] = abs(vizTable[i][5]);

            if(vizTable[i][0]>fxMeff)
                vizTable[i][0] = fxMeff;

            double mxMeff=(fxMeff-vizTable[i][0]) * a/2; // effective value of mxMax depending on the connection's type (vertical or lateral)

            if(vizTable[i][0]<0) // this must be executed AFTER the definition of mxMeff
                vizTable[i][0] = 0;

            if(vizTable[i][4]>mxMeff)
                vizTable[i][4] = mxMeff;

            if(vizTable[i][5]>mxMeff)
                vizTable[i][5] = mxMeff;

            maxS=max(vizTable[i][0]/fxMeff, maxS);
            maxS=max(vizTable[i][4]/mxMeff, maxS);
            maxS=max(vizTable[i][5]/mxMeff, maxS);
        }
    }
    OUTPUT << "Module " << module->blockId << " maximum load factor = "<< maxS << endl;

    //set color for module
    //cout << min(2*color,1.) << " " << min(2*(1-color),1.) << endl;
    module->setColor(Color(min(2*maxS,1.),min(2*(1-maxS),1.),0.0));

    if (curIteration==maxIterations && maxS>=1.-Eps) {
        module->setBlinkMode(true);
    }
}




// Auxiliary functions

void ForcesPredictionIPPTCode::printVector(vector<double> &vec, int row,string desc){
    OUTPUT << "*************printVec********************"<< endl;
    OUTPUT << desc << ":";
        for (int i=0;i<row;i++){
            OUTPUT << vec[i]<< "\t";
        }
        OUTPUT <<endl;
}



void ForcesPredictionIPPTCode::printMatrix(vector< vector<double> > &matrix, int row, int col,string desc){
    OUTPUT << "*************printMatrix********************"<< endl;
    OUTPUT << desc<< endl;
    for (int i=0;i<row;i++){
        for(int j=0;j<col;j++){
            OUTPUT << matrix[i][j]<< "\t";
        }
        OUTPUT << endl;
    }
    OUTPUT << endl;

}

vector< vector<double> > ForcesPredictionIPPTCode::IdentityMatrix6(){
    vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize,0));
    //cout << "creating K11 "<<i<< endl;
    for(int k=0;k<vectorSize;k++)
            tmp[k][k]=1;
    return tmp;
}

vector< vector<double> > ForcesPredictionIPPTCode::RevD(vector< vector<double> > &A) {
    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size(),0));
    for(int i=0;i<A.size();i++)
        tmp[i][i] = 1/A[i][i];

    return tmp;

}
vector< vector<double> > ForcesPredictionIPPTCode::createK11(int i) {
    vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
    //cout << "creating K11 "<<i<< endl;
    for(int k=0;k<vectorSize;k++)
        for(int m=0;m<vectorSize;m++){
            //cout << K11(i,k,m)<<"  ";
            tmp[k][m]=K11(i,k,m);
        }
    return tmp;
}

vector< vector<double> > ForcesPredictionIPPTCode::createK12(int i) {
    vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
        //cout << "creating K12 "<<i<< endl;
        for(int k=0;k<vectorSize;k++)
            for(int m=0;m<vectorSize;m++){
                //cout << K12(i,k,m)<<"  ";
                tmp[k][m]=K12(i,k,m);
            }
        return tmp;
}


vector< vector<double> > ForcesPredictionIPPTCode::createTfr(double Txx, double Txy, double Txz,double Tyx, double Tyy, double Tyz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[0][0]=Txx;    t[0][1]=Txy;    t[0][2]=Txz;
    t[1][0]=Tyx;    t[1][1]=Tyy;    t[1][2]=Tyz;
    return t;
}

vector< vector<double> > ForcesPredictionIPPTCode::createTmx(double Txz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[3][3]=0;
    t[3][2]=Txz;
    return t;
}

vector< vector<double> > ForcesPredictionIPPTCode::createTmy(double Txz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[4][4]=0;
    t[4][2]=Txz;
    return t;
}

vector< vector<double> > ForcesPredictionIPPTCode::createTmz(double Txz) {
    vector< vector<double> > t = IdentityMatrix6();
    t[5][5]=0;
    t[5][2]=Txz;
    return t;
}



vector< vector<double> > ForcesPredictionIPPTCode::createRot(int i){
    vector< vector<double> > tmp = decltype(tmp)(vectorSize, vector<double>(vectorSize));
    //cout << "creating K11 "<<i<< endl;
    for(int k=0;k<vectorSize;k++)
        for(int m=0;m<vectorSize;m++){
            //cout << K11(i,k,m)<<"  ";
            tmp[k][m]=Rot(i,k,m);
        }
    return tmp;
}

vector< vector<double> > ForcesPredictionIPPTCode::createD(vector< vector<double> > &A){
    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size(),0));
    for(int i=0; i<A.size();i++){
                tmp[i][i] = A[i][i]; // operation
    }
    return tmp;
}
vector< vector<double> > ForcesPredictionIPPTCode::createR(vector< vector<double> > &A){

    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size(),0));

    for(int i=0; i<A.size();i++) {
        for(int j=0; j<A[0].size();j++) {
            if(i!=j) {
                tmp[i][j] = A[i][j];
            }
        }// operation
    }
    return tmp;
}


void vector2string(const std::vector<bID>&v,string &s) {
    vector<bID>::const_iterator it = v.begin();
    s.clear();
    while (it!=v.end()) {
        s+= to_string(*it) + ",";
        it++;
    }
}


//OPERATORS

// vec * scal
vector<double> operator*(const vector<double> vec, const double  scal){
    vector<double> tmp = decltype(tmp)(vec.size(),0);
    //cout << vec.size();
        for (int i=0;i<vec.size();i++){
            tmp[i] = vec[i]*scal;
        }
    return tmp;
}

// scal * vec
vector<double> operator*(const double  scal, const vector<double> vec){
    vector<double> tmp = decltype(tmp)(vec.size(),0);
    //cout << vec.size();
        for (int i=0;i<vec.size();i++){
            tmp[i] = vec[i]*scal;
        }
    return tmp;
}

// vec + vec
vector<double> operator+(const vector<double> vec1, const vector<double> vec2){
    size_t smax = max(vec1.size(),vec2.size());
    vector<double> tmp = decltype(tmp)(smax,0);
    if (vec1.size()<vec2.size()) {
        for (size_t i=0;i<vec1.size();i++){
                tmp[i] = vec1[i]+vec2[i];
        }
        for (size_t i=vec1.size();i<vec2.size();i++){
                tmp[i] = vec2[i];
        }
    } else {
        for (size_t i=0;i<vec2.size(); i++) {
                tmp[i] = vec1[i]+vec2[i];
        }
        for (size_t i=vec2.size();i<vec1.size();i++) {
                tmp[i] = vec1[i];
        }
    }
    return tmp;
}

// vec - vec
vector<double> operator-(const vector<double> vec1, const vector<double> vec2) {
    size_t smax = max(vec1.size(),vec2.size());
    vector<double> tmp = decltype(tmp)(smax,0);
    if (vec1.size()<vec2.size()) {
        for (size_t i=0;i<vec1.size();i++){
                tmp[i] = vec1[i]-vec2[i];
        }
        for (size_t i=vec1.size();i<vec2.size();i++){
                tmp[i] = -vec2[i];
        }
    } else {
        for (size_t i=0;i<vec2.size();i++){
                tmp[i] = vec1[i]-vec2[i];
        }
        for (size_t i=vec2.size();i<vec1.size();i++){
                tmp[i] = vec1[i];
        }
    }
    return tmp;
}

// - vec
vector<double> operator-(const vector<double> vec){
    vector<double> tmp = decltype(tmp)(vec.size(),0);
    for (size_t i=0;i<vec.size();i++)
        tmp[i] = -vec[i];
    return tmp;
}

// mat * vec
vector<double>  operator*(const vector< vector<double> > A, const vector<double> vec){
    vector<double> tmp = decltype(tmp)(A.size(),0);
    for (int i=0;i<A.size();i++){
            for (int j=0;j<vec.size();j++){
                tmp[i]+=( A[i][j]*vec[j]);
            }
        }
    return tmp;
}

// mat * scal
vector< vector<double> > operator*(const vector< vector<double> > A, const double B){
    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size()));
    for(int i=0; i<A.size();i++){
            for(int j=0;j<A[0].size();j++)	{
                tmp[i][j] = A[i][j] * B;
            }
    }
    return tmp;
}

// scal * mat
vector< vector<double> > operator*(const double B, const vector< vector<double> > A){
    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size()));
    for(int i=0; i<A.size();i++){
            for(int j=0;j<A[0].size();j++)	{
                tmp[i][j] = A[i][j] * B;
            }
    }
    return tmp;
}

// mat * mat
vector< vector<double> > operator*(const vector< vector<double> > A,const vector< vector<double> > B){
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(B[0].size()));
    for(int i=0; i<A.size();i++){
        for(int j=0;j<B[0].size();j++)	{
            for(int k=0;k<B.size();k++){
                result[i][j] += A[i][k] * B[k][j]; // operation
            }
        }
    }
    return result;
}

// mat + mat
vector< vector<double> > operator+(vector< vector<double> > A,vector< vector<double> > B) {
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
    for(int i=0; i<A.size();i++){
            for(int j=0;j<A[0].size();j++)	{
                    result[i][j] = A[i][j] + B[i][j]; // operation
            }
        }
    return result;
}

// mat - mat
vector< vector<double> > operator-(vector< vector<double> > A,vector< vector<double> > B) {
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
    for(int i=0; i<A.size();i++){
            for(int j=0;j<A[0].size();j++)	{
                    result[i][j] = A[i][j] - B[i][j]; // operation
            }
        }
    return result;
}

// - mat
vector< vector<double> > operator-(vector< vector<double> > A) {
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
    for(int i=0; i<A.size();i++){
            for(int j=0;j<A[0].size();j++)	{
                    result[i][j] = -A[i][j]; // operation
            }
        }
    return result;
}
