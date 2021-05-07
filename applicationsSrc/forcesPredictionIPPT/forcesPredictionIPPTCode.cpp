#include "forcesPredictionIPPTCode.h"

const int messageDelay=50;
const int messageDelayError=0;
const int messageDelayCons=1;

int nofIterations = 1000; // max number of iterations
double nextIterationTick=1;
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
//double globalGamma = pow(10,-6); //stiffness reduction multiplier (for unilateral contact)
double globalGamma = pow(10,-4); //stiffness reduction multiplier (for unilateral contact)
double globalSupportZ = 0; //Z coordinate of the bottom modules (contacting with the support)
int globalPreconditioner=PreconditionerNone; // preconditioner type

int globVis=1;

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


    attr = element->Attribute("preconditioner");
    if (attr) {
        cerr << "preconditioner =" << attr<< endl;
        if(strcmp(attr,"Jacobi")==0) {
            globalPreconditioner=PreconditionerJacobi;
            cerr << "indeed, preconditioner =" << attr<< endl;
        }
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

    console << "Neigh:";
    for (int i=0; i<6; i++) {
			console << neighbors[i][0] << "/" <<  neighbors[i][1] << ",";
		}
		console << "\n";

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

    performSST=false;
    attr = config->Attribute("stabilityCheck");
    if (attr) {
        cerr << "stabilityCheck =" << attr<< endl;
        if(strcmp(attr,"simplified")==0)
            performSST=true;
    }

    solver=SolverWeightedJacobi;
    attr = config->Attribute("solver");
    if (attr) {
        cerr << "solver =" << attr<< endl;
        if(strcmp(attr,"CG")==0 || strcmp(attr,"ConjugateGradients")==0)
            solver=SolverConjugateGradients;
    }

    attr = config->Attribute("myAttribute");
    if (attr) {
        cerr << "myAttribute =" << attr<< endl;
    }
}

void ForcesPredictionIPPTCode::startup() {
    /// BEGIN EXAMPLE
    // @JAKUB: An example on how to highlight a specific cell or all cells in a target
    //  Target colors are set by the XML, lattice highlight colors are set in code.
    //  (see color.h for more colors or adding new ones if needed)
    //  -- Pierre
    static bool highlightedTarget = false;
    if (not highlightedTarget) {
        highlightedTarget = true;
        target->highlight();
/*
        lattice->highlightCell(Cell3DPosition(0,0,0)); // YELLOW By default
        lattice->highlightCell(Cell3DPosition(0,0,1), RED);
*/
    }
    /// END EXAMPLE

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

    addMessageEventFunc(CG_INIT_Q_MSG,_CGInitQMessage);
    addMessageEventFunc(CG_INIT_R_MSG,_CGInitRMessage);
    addMessageEventFunc(CG_ALPHA_Q_MSG,_CGAlphaQMessage);
    addMessageEventFunc(CG_ALPHA_R_MSG,_CGAlphaRMessage);
    addMessageEventFunc(CG_BETA_Q_MSG,_CGBetaQMessage);
    addMessageEventFunc(CG_BETA_R_MSG,_CGBetaRMessage);
    addMessageEventFunc(CG_D_MSG,_CGDMessage);
    addMessageEventFunc(CG_DU_MSG,_CGDUMessage);
    addMessageEventFunc(CG_VIS_Q_MSG,_CGVisQMessage);

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
    preconditioner = globalPreconditioner; // CG preconditioner type
    console << "preconditioner=" << preconditioner << "\n";
//	supportZ = globalSupportZ; //Z coordinate of the bottom modules (contacting with the support)

//    cmd = new cmData;
    if(module->position[2]==globalSupportZ) isSupport=true;


    //cheking neighbors and adding them to a list
    SetNeighbors();
    //CheckNeighbors();

    //check is module fixed
    if(isFixed) {
        module->setColor(Color(0.0f,0.0f,1.0f));
    }


//	//check is module support
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

    if(tree_par!=0 || isCentroid) { // module is already a child (has a parent) or is a centroid
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
		//console << "["<< neighbors[0][0] << neighbors[1][0] << neighbors[2][0] << neighbors[3][0] << neighbors[4][0] << neighbors[5][0] << "]" << "\n";
		console << "["<< tree_child[0] << tree_child[1] << tree_child[2] << tree_child[3] << tree_child[4] << tree_child[5] << "]" << "\n";
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
/*            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par);
            if(p2p) sendMessage("TREE_CONF_MSG",new MessageOf<int>(TREE_CONF_MSG,1),p2p,messageDelay,messageDelayError);*/
            TO_PARENT(
            sendMessage("TREE_CONF_MSG",new MessageOf<int>(TREE_CONF_MSG,1),p2p,messageDelay,messageDelayError);
            )
        } else { // centroid initiates the center of mass query for all non-virtual children (!!!the case in which there are no real children is not supported!!!)
            cmd.mX=module->position[0] * L * mass;
            cmd.mY=module->position[1] * L * mass;
            cmd.m=mass;
            const double dir[6][3]={{0,0,1},{0,0,-1},{-1,0,0},{1,0,0},{0,-1,0},{0,1,0}}; // 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1
/*            for(int i=0; i<6; i++) {
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
            } */
            TO_CHILDREN( // non-virtual
                sendMessage("CM_Q_MSG",new Message(CM_Q_MSG),p2p,messageDelay,messageDelayError);
                aggregationCompleted[i]=false;
            )( // virtual
                cmd.mX=cmd.mX+(module->position[0]+dir[i][0])*L*mass;
                cmd.mY=cmd.mY+(module->position[1]+dir[i][1])*L*mass;
                cmd.m=cmd.m+mass;
            )
        }
    }
}

void ForcesPredictionIPPTCode::cmQMessage(P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    console << "CM_Q_Message " << msgFrom << "->" << module->blockId << "\n";

    bool anyMsgSent=false;
    cmd.mX=module->position[0] * L * mass;
    cmd.mY=module->position[1] * L * mass;
    cmd.m=mass;
    const double dir[6][3]={{0,0,1},{0,0,-1},{-1,0,0},{1,0,0},{0,-1,0},{0,1,0}}; // 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1
/*    for(int i=0;i<6;i++) {
        if(tree_child[i]==1) { // if non-virtual child
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
            if (p2p) {
                sendMessage("CM_Q_MSG",new Message(CM_Q_MSG),p2p,messageDelay,messageDelayError);
                aggregationCompleted[i]=false;
                anyMsgSent=true;
            }
            // after sending the messages to neighbors, we wait for confirmations before aggregating the info
        } else if(neighbors[i][1]==2) {
            cmd.mX=cmd.mX+(module->position[0]+dir[i][0]*L)*mass;
            cmd.mY=cmd.mY+(module->position[1]+dir[i][1]*L)*mass;
            cmd.m=cmd.m+mass;
        }
    } */
    TO_CHILDREN( // non-virtual
        sendMessage("CM_Q_MSG",new Message(CM_Q_MSG),p2p,messageDelay,messageDelayError);
        aggregationCompleted[i]=false;
        anyMsgSent=true;
    )( // virtual
        cmd.mX=cmd.mX+(module->position[0]+dir[i][0])*L*mass;
        cmd.mY=cmd.mY+(module->position[1]+dir[i][1])*L*mass;
        cmd.m=cmd.m+mass;
    )
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
/*            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par);
            if(p2p) sendMessage("cM_R_MSG",new MessageOf<cmData >(CM_R_MSG,cmd),p2p,messageDelay,messageDelayError); */
            TO_PARENT(
            sendMessage("CM_R_MSG",new MessageOf<cmData >(CM_R_MSG,cmd),p2p,messageDelay,messageDelayError);
            )
        } else { // centroid initiates the weighted-Jacobi iterations
/*            for(int i=0; i<6; i++) {
                if(tree_child[i]) { // if non-virtual child
                    P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
                    if(p2p) {
                        sendMessage("DU_INIT_MSG",new MessageOf<int >(DU_INIT_MSG,maxIterations),p2p,messageDelay,messageDelayError);
                        aggregationCompleted[i]=false;
                    }
                }
            } */
            cout << "Center of mass: (" << cmd.mX/cmd.m << ", " << cmd.mY/cmd.m << ")" << endl;
            if(performSST) { // perform simplified stability check first
                vector<double> cmdXY{cmd.mX/cmd.m,cmd.mY/cmd.m};
                TO_CHILDREN(
                sendMessage("SST_Q_MSG",new MessageOf<vector<double> >(SST_Q_MSG,cmdXY),p2p,messageDelay,messageDelayError);
                aggregationCompleted[i]=false;
                )()
            } else {
                if( solver==SolverConjugateGradients ) {
                    switch(preconditioner) {
                    case PreconditionerNone:
                        precK = IdentityMatrix6();
                        break;
                    case PreconditionerJacobi:
                        precK = 0*precK;
                        for(int i=0;i<6;i++) {
                            if(neighbors[i][0]>0 || neighbors[i][1]==2) { // sum up K11 for all neighbors (local part only)
                                precK = precK + createK11(i);
                            }
                        }
                        if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
                            precK=precK+contactStiffnessMatrix(CGd);
                        }
                        precK = RevD(precK);
                        break;
                    }
                }
//                printMatrix(precK,6,6,"precK=");

                TO_CHILDREN(
                switch(solver) {
                case SolverWeightedJacobi:
                    sendMessage("DU_INIT_MSG",new MessageOf<int >(DU_INIT_MSG,maxIterations),p2p,messageDelay,messageDelayError);
                    aggregationCompleted[i]=false;
                    break;
                case SolverConjugateGradients:
                    CGd = precK*Fp;
                    CGr = Fp;
                    if(!isFixed) {
                        CGrr=CGr*(precK*CGr);
                    } else {
                        CGrr=0;
                    }
//                    cerr << "centroid 0: r.precK.r=" << CGrr << ", r.r=" << CGr*CGr << "\n";
                    sendMessage("CG_INIT_Q_MSG",new Message(CG_INIT_Q_MSG),p2p,messageDelay,messageDelayError);
                    aggregationCompleted[i]=false;
                    break;
                }
                )(
                switch(solver) {
                case SolverConjugateGradients:
                    // ADD preconditioner support !!!
                    CGrq[i] = Fp;
                    if(!isFixed) CGrr+=CGrq[i]*CGrq[i];
                    break;
                }
                )
            }
        }
    }
}

void ForcesPredictionIPPTCode::duInitMessage(const MessageOf<int>*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    maxIterations = *msg->getData();
    console << "DU_INIT_Message(" << maxIterations << ") " << msgFrom << "->" << module->blockId << "\n";

/*    for(int i=0;i<6;i++) {
        if(tree_child[i]==1) { // if non-virtual child
            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(neighbors[i][0]);
            if (p2p) {
                sendMessage("DU_INIT_MSG",new MessageOf<int >(DU_INIT_MSG, maxIterations),p2p,messageDelay,messageDelayError);
                aggregationCompleted[i]=false;
            }
        }
    }*/
    TO_CHILDREN(
    sendMessage("DU_INIT_MSG",new MessageOf<int >(DU_INIT_MSG, maxIterations),p2p,messageDelay,messageDelayError);
    aggregationCompleted[i]=false;
    )()
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
//            OUTPUT << "Iter=" << curIteration  <<  ", ID="<< module->blockId << " received the message from " << msgFrom<< endl;
//            printVector(msgData,6,"msgData from "+to_string(msgFrom)+" to "+ to_string(module->blockId));
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
//    printNeighbors();
    if(ready) {
        OUTPUT << "Calculating du"<< endl;
        computeDU();
        //visualisation
        if(curIteration%500==0) {
            visualization();
//            if(module->blockId == 6) {
            if(isCentroid) {
						cout << module->blockId << ": It = "<< curIteration<< " Du=(" << du[0] << "," << du[1] << "," << du[2] << "," << du[3] << "," << du[4] << "," << du[5] << ")" <<endl;
            }
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
                    TO_PARENT(
                    sendMessage("DU_COMPLETE_MSG",new Message(DU_COMPLETE_MSG),p2p,messageDelay,messageDelayError);
                    )
                } else { // centroid initiates the model-based stability check
                    TO_CHILDREN(
                    sendMessage("MST_Q_MSG",new Message(MST_Q_MSG),p2p,messageDelay,messageDelayError);
                    aggregationCompleted[i]=false;
                    )()
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
            TO_PARENT(
            sendMessage("DU_COMPLETE_MSG",new Message(DU_COMPLETE_MSG),p2p,messageDelay,messageDelayError);
            )
        } else { // centroid initiates the model-based stability check
            TO_CHILDREN(
            sendMessage("MST_Q_MSG",new Message(MST_Q_MSG),p2p,messageDelay,messageDelayError);
            aggregationCompleted[i]=false;
            )()
        }
    }
}


void ForcesPredictionIPPTCode::sstQMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    vector<double> msgData = *msg->getData();
    console << "SST_Q_Message({" << msgData[0] << ", " << msgData[1] <<"}) " << msgFrom << "->" << module->blockId << "\n";

    bool anyMsgSent=false;

    simplifiedStability(sstd,msgData,-1); // self-stability assessment

    TO_CHILDREN(
        sendMessage("SST_Q_MSG",new MessageOf<vector<double> >(SST_Q_MSG,msgData),p2p,messageDelay,messageDelayError);
        aggregationCompleted[i]=false;
        anyMsgSent=true;
    )( // virtual
        /* get stability info from virtual children */
        sstData sstdn; // data from neighbour
        simplifiedStability(sstdn,msgData,i); // stability assessment for virtual neighbour 'i'
        aggregateSimplifiedStability(sstd,sstdn);
    )
    if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then aggregate the data and return the confirmation
        sendMessage("SST_R_MSG",new MessageOf<sstData >(SST_R_MSG,sstd),sender,messageDelay,messageDelayError);
        console << "*** module: "<< module->blockId << ", simplified stability data: {" << sstd.X <<", " << sstd.Y <<"}\n";
    }
}

void ForcesPredictionIPPTCode::sstRMessage(const MessageOf<sstData>*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    sstData sstdn = *msg->getData();

    console << "SST_R_Message({" << sstdn.X <<", " << sstdn.Y  << "}) " << msgFrom << "->" << module->blockId << "\n";
    bool aggrCompl=true;

    aggregateSimplifiedStability(sstd,sstdn);
    for(int i=0;i<6;i++) { // aggregation of info from the child
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
        }
        if(!aggregationCompleted[i]) aggrCompl=false;
    }
    if(aggrCompl) {
        if(!isCentroid) {
            TO_PARENT(
            sendMessage("SST_R_MSG",new MessageOf<sstData >(SST_R_MSG,sstd),p2p,messageDelay,messageDelayError);
            console << "*** module: "<< module->blockId << ", simplified stability data: {" << sstd.X <<", " << sstd.Y <<"}\n";
            )
        } else { // centroid shows if stability criterion is met
            console << "*** centroid simplified stability data: {" << sstd.X <<", " << sstd.Y << "}\n";
            if (sstd.X!=100) { // if not a full angle then it is an unstable configuration. W show the alert and we do not continue further checks.
                module->setColor(Color(0.52f,0.13f,0.55f)); // purple color
//                module->setBlinkMode(true);
            } else { // possibly stable configuration. We continue with the overloading check and the model-based stability check
                TO_CHILDREN(
                sendMessage("DU_INIT_MSG",new MessageOf<int >(DU_INIT_MSG,maxIterations),p2p,messageDelay,messageDelayError);
                aggregationCompleted[i]=false;
                )()
            }
        }
    }
}

void ForcesPredictionIPPTCode::mstQMessage(P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    console << "MST_Q_Message() " << msgFrom << "->" << module->blockId << "\n";

    bool anyMsgSent=false;

    modelBasedStability(mstd,-1); // self stability assessment

    TO_CHILDREN(
        sendMessage("MST_Q_MSG",new Message(MST_Q_MSG),p2p,messageDelay,messageDelayError);
        aggregationCompleted[i]=false;
        anyMsgSent=true;
    )( // virtual
        /* get stability info from virtual children */
        mstData mstdn; // data from neighbour
        modelBasedStability(mstdn,i); // stability assessment for virtual neighbour 'i'
        aggregateModelBasedStability(mstd,mstdn);
    )
    if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then aggregate the data and return the confirmation
        sendMessage("MST_R_MSG",new MessageOf<mstData >(MST_R_MSG,mstd),sender,messageDelay,messageDelayError);
    }
}

void ForcesPredictionIPPTCode::mstRMessage(const MessageOf<mstData>*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    mstData mstdn = *msg->getData();

    console << "MST_R_Message({" << mstdn.X[0][0] <<", " << mstdn.X[0][1] <<", " << mstdn.X[0][2] <<"}, {" << mstdn.X[1][0] <<", " << mstdn.X[1][1] <<", " << mstdn.X[1][2] <<"}, " << mstdn.nOfPts <<") " << msgFrom << "->" << module->blockId << "\n";
    bool aggrCompl=true;
//    const double dir[6][3]={{0,0,1},{0,0,-1},{-1,0,0},{1,0,0},{0,-1,0},{0,1,0}}; // 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1
    /* TO BE DONE: aggregate data */
		aggregateModelBasedStability(mstd,mstdn);
    for(int i=0;i<6;i++) { // aggregation of info from the child
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
        }
        if(!aggregationCompleted[i]) aggrCompl=false;
    }
    if(aggrCompl) {
        if(!isCentroid) {
/*            P2PNetworkInterface *p2p = module->getP2PNetworkInterfaceByDestBlockId(tree_par);
            if(p2p) sendMessage("cM_R_MSG",new MessageOf<cmData >(CM_R_MSG,cmd),p2p,messageDelay,messageDelayError); */
            TO_PARENT(
            sendMessage("MST_R_MSG",new MessageOf<mstData >(MST_R_MSG,mstd),p2p,messageDelay,messageDelayError);
            console << "*** module: "<< module->blockId << ", model based stability data: {" << mstd.X[0][0] <<", " << mstd.X[0][1] <<", " << mstd.X[0][2] <<"}, {" << mstd.X[1][0] <<", " << mstd.X[1][1] <<", " << mstd.X[1][2] <<"}, " << mstd.nOfPts <<") " << "\n";
            )
        } else { // centroid shows if stability criterion is met
            /* TO BE DONE */
            console << "*** centroid model based stability data: {" << mstd.X[0][0] <<", " << mstd.X[0][1] <<", " << mstd.X[0][2] <<"}, {" << mstd.X[1][0] <<", " << mstd.X[1][1] <<", " << mstd.X[1][2] <<"}, " << mstd.nOfPts <<") " << "\n";
            if (mstd.nOfPts!=3) {
                module->setColor(Color(0.51f,0.13f,0.55f)); // purple color
//                module->setBlinkMode(true);
            }
        }
    }
}




// CG_INIT_Q
void ForcesPredictionIPPTCode::CGInitQMessage(P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
//    console << "CG_INIT_Q_Message " << msgFrom << "->" << module->blockId << "\n";


        switch(preconditioner) {
        case PreconditionerNone:
            precK = IdentityMatrix6();
            break;
        case PreconditionerJacobi:
            precK = 0*precK;
            for(int i=0;i<6;i++) {
                if(neighbors[i][0]>0 || neighbors[i][1]==2) { // sum up K11 for all neighbors (local part only)
                    precK = precK + createK11(i);
                }
            }
            if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
                precK=precK+contactStiffnessMatrix(CGd);
            }
            precK = RevD(precK);
            break;
        }

    CGKd = 0*CGKd;
    bool anyMsgSent=false;
    if(!isFixed) {
        CGd = precK*Fp;
        CGr = Fp;
    } else {
        CGd = 0*Fp;
        CGr = 0*Fp;
    }
    CGrr=CGr*(precK*CGr);
/*    printVector(CGr,6,"CG_INIT_Q debug::: r=");
    printVector(precK*CGr,6,"CG_INIT_Q debug::: precK.r=");
    printMatrix(precK,6,6,"CG_INIT_Q debug::: precK=");
    console << "CG_INIT_Q debug::: r.precK.r=" << CGrr << "\n";*/
    TO_CHILDREN(
    sendMessage("CG_INIT_Q_MSG",new Message(CG_INIT_Q_MSG),p2p,messageDelay,messageDelayError);
    aggregationCompleted[i]=false;
    anyMsgSent=true;
    )( // to virtual children
        // Add preconditioner support
        CGrq[i] = Fp;
        if(!isFixed) CGrr+=CGrq[i]*CGrq[i];
    )
    if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then aggregate the data and return the confirmation
//        console << "CG_INIT_Q: Module " << module->blockId << " returns 1000000*CGrr=" << 1000000*CGrr << "\n";
        sendMessage("CG_INIT_R_MSG",new MessageOf<double >(CG_INIT_R_MSG,CGrr),sender,messageDelay,messageDelayError);
    }
}

// CG_INIT_R
void ForcesPredictionIPPTCode::CGInitRMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    double retrr = *msg->getData();

//    console << "CG_INIT_R_Message("<< retrr <<") " << msgFrom << "->" << module->blockId << "\n";
    CGrr+=retrr;
    bool aggrCompl=true;
    for(int i=0;i<6;i++) { // aggregation of info from the child
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
        }
        if(!aggregationCompleted[i]) aggrCompl=false; // if some child has not yet returned message
    }
    if(aggrCompl) { // if all children has returned
        clearNeighborsMessage();
        if(!isCentroid) {
            TO_PARENT(
//            console << "CG_INIT_R: Module " << module->blockId << " returns CGrr=" << CGrr << "\n";
            sendMessage("CG_INIT_R_MSG",new MessageOf<double >(CG_INIT_R_MSG,CGrr),p2p,messageDelay,messageDelayError);
            )
        } else { // centroid initiates computationa of ALPHA phase of CG
            CGrnrn=CGrr;
            curIteration++;
            CGFpFp=CGrr;
            nextIterationTick=0.1;
            cerr << "*** Iteration " << curIteration << " started. r.r/F.F=" << CGrr/CGFpFp << ", r.r=" << CGrr << "\n";

            for(int i=0;i<6;i++) {
                if(neighbors[i][0]>0) { // compute CGKd for all neighbors (local part only)
                    CGKd = CGKd +((createK11(i)*CGd));
                    }
            }
            if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
                CGKd=CGKd+contactStiffnessMatrix(CGd)*CGd;
            }
            TO_CHILDREN(
            sendMessage("CG_ALPHA_Q_MSG",new MessageOf<double >(CG_ALPHA_Q_MSG,0),p2p,messageDelay,messageDelayError);
            aggregationCompleted[i]=false;
            )( // for virtual neighbours
            // ADD preconditioner support
            int di=1-2*(i%2); // switches the sides (up<->down, left<->right, front<->back)
//            CGdq[i] = CGrq[i] + CG_beta*CGdq[i];
            CGKdq[i] = createK12(i+di)*CGd+createK11(i+di)*CGdq[i];
            CGKd = CGKd + createK12(i)*CGdq[i]+createK11(i)*CGd;
            CGdKd += CGdq[i]*CGKdq[i];
            )
            sendMessageToAllNeighbors("CG_D_MSG",new MessageOf<vector<double> >(CG_D_MSG,CGd),messageDelay,messageDelayError,0);
        }
    }
}

// CG_ALPHA_Q
void ForcesPredictionIPPTCode::CGAlphaQMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    double CG_beta = *msg->getData();

//    console << "CG_ALPHA_Q_Message("<< CG_beta <<") " << msgFrom << "->" << module->blockId << "\n";

    if(!isFixed) {
        CGd = precK*CGr + CG_beta*CGd;
    } else {
        CGd = 0*CGd;
    }
    for(int i=0;i<6;i++) {
        if(neighbors[i][0]>0) { // compute CGKd for all neighbors (local part only)
            CGKd = CGKd +((createK11(i)*CGd));
        }
    }
    if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
        CGKd=CGKd+contactStiffnessMatrix(CGd)*CGd;
    }
    bool anyMsgSent=false;
    TO_CHILDREN(
    sendMessage("CG_ALPHA_Q_MSG",new MessageOf<double >(CG_ALPHA_Q_MSG, CG_beta),p2p,messageDelay,messageDelayError);
    aggregationCompleted[i]=false;
    anyMsgSent=true;
    )( // for virtual neighbours
        // ADD preconditioner support
	int di=1-2*(i%2); // switches the sides (up<->down, left<->right, front<->back)
        CGdq[i] = CGrq[i] + CG_beta*CGdq[i];
        CGKdq[i] = createK12(i+di)*CGd+createK11(i+di)*CGdq[i];
        CGKd = CGKd + createK12(i)*CGdq[i]+createK11(i)*CGd;
        CGdKd += CGdq[i]*CGKdq[i];
    )
    if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then aggregate the data and return the confirmation
        bool aggrCompl=true, ready=true;
        for(int i=0;i<6;i++) {
            if(!aggregationCompleted[i]) aggrCompl=false; // if all childern has returned message
            if(neighbors[i][0]!=0 && neighbors[i][1]==0) ready = false; // if all 'd' arrived from neighbours
        }
        if(aggrCompl && ready) {
            CGdKd += CGd*(CGKd);
            clearNeighborsMessage();
            sendMessage("CG_ALPHA_R_MSG",new MessageOf<double >(CG_ALPHA_R_MSG,CGdKd),sender,messageDelay,messageDelayError);
        }
    }
    sendMessageToAllNeighbors("CG_D_MSG",new MessageOf<vector<double> >(CG_D_MSG,CGd),messageDelay,messageDelayError,0);
}


// CG_D_Message
void ForcesPredictionIPPTCode::CGDMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    vector<double> retd = *msg->getData();

/*    console << "CG_D_Message( ";
    for(int i=0;i<6;i++) console << retd[i] << (i<5?",":" ");
    console <<") " << msgFrom << "->" << module->blockId << "\n";*/

    bool aggrCompl=true, ready=true;
    for(int i=0;i<6;i++) { // aggregation of info from neighbours
        if(neighbors[i][0]==msgFrom) {
            neighbors[i][1]=1;
            if(!isFixed) {
                CGKd = CGKd +(createK12(i)*retd); // *******
            }
        }
        if(!aggregationCompleted[i]) aggrCompl=false; // if all childern has returned message
        if(neighbors[i][0]!=0 && neighbors[i][1]==0) ready = false; // if all 'd' arrived from neighbours
    }
    if(aggrCompl && ready) {
        CGdKd += CGd*(CGKd); // *******
        clearNeighborsMessage();
        if(!isCentroid) {
            TO_PARENT(
            sendMessage("CG_ALPHA_R_MSG",new MessageOf<double >(CG_ALPHA_R_MSG,CGdKd),p2p,messageDelay,messageDelayError);
            )
        } else { // centroid initiates computationa of Beta phase of CG
            double alpha=CGrnrn/CGdKd;
            du = du + alpha*CGd; // **********
/*            console << "!!! centroid: du=( ";
            for(int i=0;i<6;i++) console << du[i] << (i<5?",":" ");
            console << ")\n";
            console << "!!! centroid: Kd=( ";
            for(int i=0;i<6;i++) console << CGKd[i] << (i<5?",":" ");
            console << ")\n";
            console << "!!! centroid: r=( ";
            for(int i=0;i<6;i++) console << CGr[i] << (i<5?",":" ");
            console << ")\n"; */
            CGr = CGr - alpha*(CGKd); // *********
            CGKd = 0*CGKd;
            CGrnrn = CGrr;
            CGrr = CGr*(precK*CGr);
/*            console << "centroid, alpha=" << alpha << ", d.K.d=" << CGdKd << "\n";
            console << "!!! centroid: new r=( ";
            for(int i=0;i<6;i++) console << CGr[i] << (i<5?",":" ");
            console << ")\n";
            console << "centroid, new r.r=" << CGrr << "\n"; */
            CGdKd = 0;
            TO_CHILDREN(
            sendMessage("CG_BETA_Q_MSG",new MessageOf<double >(CG_BETA_Q_MSG, alpha),p2p,messageDelay,messageDelayError);
            aggregationCompleted[i]=false;
            )( // virtual neighbours
            // ADD preconditioner support
            uq[i] = uq[i] + alpha*CGdq[i]; // ********
            CGrq[i] = CGrq[i] - alpha*CGKdq[i]; // ***********
            CGKdq[i] = 0*CGKdq[i];
            CGrr += CGrq[i]*CGrq[i];
            )
        }
    }
}



// CG_ALPHA_R
void ForcesPredictionIPPTCode::CGAlphaRMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    double retdKd = *msg->getData();

//    console << "CG_ALPHA_R_Message("<< retdKd <<") " << msgFrom << "->" << module->blockId << "\n";

    CGdKd += retdKd;
    bool aggrCompl=true, ready=true;
    for(int i=0;i<6;i++) { // aggregation of info from children
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
        }
        if(!aggregationCompleted[i]) aggrCompl=false; // if all childern has returned message
        if(neighbors[i][0]!=0 && neighbors[i][1]==0) ready = false; // if all 'd' arrived from neighbours
    }
    if(aggrCompl && ready) {
        CGdKd += CGd*(CGKd); // *******
        clearNeighborsMessage();
        if(!isCentroid) {
            TO_PARENT(
            sendMessage("CG_ALPHA_R_MSG",new MessageOf<double >(CG_ALPHA_R_MSG,CGdKd),p2p,messageDelay,messageDelayError);
            )
        } else { // centroid initiates computationa of Beta phase of CG
            double alpha=CGrnrn/CGdKd;


        if(module->blockId < 0) {
            console << "----MODULE " << module->blockId  << ": K.d=( ";
            for(int i=0;i<6;i++) console << CGKd[i] << (i<5?",":" ");
            console << ")\n";
            console << "----MODULE " << module->blockId  << ": r=( ";
            for(int i=0;i<6;i++) console << CGr[i] << (i<5?",":" ");
            console << ")\n";
            console << "----MODULE " << module->blockId  << ": d=( ";
            for(int i=0;i<6;i++) console << CGd[i] << (i<5?",":" ");
            console << ")\n";
            console << "----MODULE " << module->blockId  << ": du=( ";
            for(int i=0;i<6;i++) console << du[i] << (i<5?",":" ");
            console << ")\n";
        }


            du = du + alpha*CGd; // **********
/*            console << "!!! centroid: du=( ";
            for(int i=0;i<6;i++) console << du[i] << (i<5?",":" ");
            console << ")\n";
            console << "!!! centroid: Kd=( ";
            for(int i=0;i<6;i++) console << CGKd[i] << (i<5?",":" ");
            console << ")\n";
            console << "!!! centroid: r=( ";
            for(int i=0;i<6;i++) console << CGr[i] << (i<5?",":" ");
            console << ")\n"; */
            CGr = CGr - alpha*(CGKd); // *********8
            CGKd = 0*CGKd;
            CGrnrn = CGrr;
            CGrr = CGr*(precK*CGr);
/*            console << "centroid, alpha=" << alpha << ", d.K.d=" << CGdKd << "\n";
            console << "!!! centroid: new r=( ";
            for(int i=0;i<6;i++) console << CGr[i] << (i<5?",":" ");
            console << ")\n";
            console << "centroid, new r.r=" << CGrr << "\n"; */
            CGdKd = 0;
            TO_CHILDREN(
            sendMessage("CG_BETA_Q_MSG",new MessageOf<double >(CG_BETA_Q_MSG, alpha),p2p,messageDelay,messageDelayError);
            aggregationCompleted[i]=false;
            )( // virtual neighbours
                // ADD preconditioner support
            uq[i] = uq[i] + alpha*CGdq[i]; // ********
            CGrq[i] = CGrq[i] - alpha*(precK*CGKdq[i]); // ***********
            CGKdq[i] = 0*CGKdq[i];
            CGrr += CGrq[i]*CGrq[i];
            )
        }
    }
}

// CG_BETA_Q
void ForcesPredictionIPPTCode::CGBetaQMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    double CG_alpha = *msg->getData();

//    console << "CG_BETA_Q_Message("<< CG_alpha <<") " << msgFrom << "->" << module->blockId << "\n";

    if(module->blockId < 0) {
        console << "----MODULE " << module->blockId  << ": K.d=( ";
        for(int i=0;i<6;i++) console << CGKd[i] << (i<5?",":" ");
        console << ")\n";
        console << "----MODULE " << module->blockId  << ": r=( ";
        for(int i=0;i<6;i++) console << CGr[i] << (i<5?",":" ");
        console << ")\n";
        console << "----MODULE " << module->blockId  << ": d=( ";
        for(int i=0;i<6;i++) console << CGd[i] << (i<5?",":" ");
        console << ")\n";
        console << "----MODULE " << module->blockId  << ": du=( ";
        for(int i=0;i<6;i++) console << du[i] << (i<5?",":" ");
        console << ")\n";
    }
    du = du + CG_alpha*CGd; // **********
/*    console << "!!! module "<< module->blockId <<": du=( ";
    for(int i=0;i<6;i++) console << du[i] << (i<5?",":" ");
    console << ")\n"; */
    if(!isFixed) {
        CGr = CGr - CG_alpha*(CGKd); // *********
    } else {
        CGr = 0*CGr;
    }
    if(module->blockId < 0) {
        console << "----MODULE " << module->blockId  << ": new r=( ";
        for(int i=0;i<6;i++) console << CGr[i] << (i<5?",":" ");
        console << ")\n";
        console << "----MODULE " << module->blockId  << ": new du=( ";
        for(int i=0;i<6;i++) console << du[i] << (i<5?",":" ");
        console << ")\n";
    }
    CGKd = 0*CGKd;
    CGrr = CGr*(precK*CGr);
    CGdKd = 0;
    bool anyMsgSent=false;
    TO_CHILDREN(
    sendMessage("CG_BETA_Q_MSG",new MessageOf<double >(CG_BETA_Q_MSG, CG_alpha),p2p,messageDelay,messageDelayError);
    aggregationCompleted[i]=false;
    anyMsgSent=true;
    )( // for virtual neighbours
    // ADD preconditioning support
    uq[i] = uq[i] + CG_alpha*CGdq[i]; // ********
    CGrq[i] = CGrq[i] - CG_alpha*CGKdq[i]; // ***********
    CGKdq[i] = 0*CGKdq[i];
    CGrr += CGrq[i]*CGrq[i];
    )

    if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then aggregate the data and return the confirmation
        clearNeighborsMessage();
//        console << "CG_BETA_Q: Module " << module->blockId << " returns CGrr=" << CGrr << "\n";
        sendMessage("CG_BETA_R_MSG",new MessageOf<double >(CG_BETA_R_MSG,CGrr),sender,messageDelay,messageDelayError);
    }
}

// CG_BETA_R
void ForcesPredictionIPPTCode::CGBetaRMessage(const MessageOf<double >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    double retrr = *msg->getData();

//    console << "CG_BETA_R_Message("<< retrr <<") " << msgFrom << "->" << module->blockId << "\n";
    CGrr+=retrr;
    bool aggrCompl=true;
    for(int i=0;i<6;i++) { // aggregation of info from the child
        if(neighbors[i][0]==msgFrom) {
            aggregationCompleted[i]=true;
        }
        if(!aggregationCompleted[i]) aggrCompl=false; // if some child has not yet returned message
    }
    if(aggrCompl) { // if all children has returned
//        clearNeighborsMessage();
        if(!isCentroid) {
            TO_PARENT(
//            console << "CG_BETA_R: Module " << module->blockId << " returns CGrr=" << CGrr << "\n";
            sendMessage("CG_BETA_R_MSG",new MessageOf<double >(CG_BETA_R_MSG,CGrr),p2p,messageDelay,messageDelayError);
            )
        } else { // centroid initiates computationa of ALPHA phase of CG (or visualization if converged)
            double CG_beta = CGrr/CGrnrn;
            CGd = precK*CGr + CG_beta*CGd;
            for(int i=0;i<6;i++) {
                if(neighbors[i][0]>0) { // compute CGKd for all neighbors (local part only)
                    CGKd = CGKd +((createK11(i)*CGd));
                }
            }
            if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
                CGKd=CGKd+contactStiffnessMatrix(CGd)*CGd;
            }
            curIteration++;
            if(curIteration % 50 == 0) {
                cerr << "Iteration=" << curIteration << ", r.r/F.F=" << CGrr/CGFpFp << ", r.r=" << CGrr <<"\n";
            }
            if(CGrr/CGFpFp < nextIterationTick) {
                cerr << "+     Iteration Tick="<< curIteration << ", r.r/F.F=" << CGrr/CGFpFp << "\n";
                nextIterationTick = nextIterationTick/10;
            }
            CGrnrn=CGrr;
//            console << "*** Iteration " << curIteration << " started. CGrr="<< CGrr <<"\n";
            if(CGrr/CGFpFp < Eps ) { // if CG_rr == 0 then CG method converged
                curIteration--;
                cerr << "CG iterations = " << curIteration << "\n";
            }
            TO_CHILDREN(
            if(CGrr/CGFpFp < Eps ) { // if CG_rr == 0 then CG method converged
                sendMessage("CG_VIS_Q_MSG",new Message(CG_VIS_Q_MSG),p2p,messageDelay,messageDelayError);
            } else { // next CG iteration starts
                sendMessage("CG_ALPHA_Q_MSG",new MessageOf<double >(CG_ALPHA_Q_MSG, CG_beta),p2p,messageDelay,messageDelayError);
            }
            aggregationCompleted[i]=false;
            )( // virtual chiltren
            CGdq[i] = CGrq[i] + CG_beta*CGdq[i];
            )
            if(CGrr/CGFpFp < Eps ) { // if CG_rr == 0 then CG method converged
                sendMessageToAllNeighbors("CG_DU_MSG",new MessageOf<vector<double> >(CG_DU_MSG,du),messageDelay,messageDelayError,0);
            } else {
                sendMessageToAllNeighbors("CG_D_MSG",new MessageOf<vector<double> >(CG_D_MSG,CGd),messageDelay,messageDelayError,0);
            }
        }
    }
}


// OK receives the solution (displacement du from all neighbours) and runs visualization
void ForcesPredictionIPPTCode::CGDUMessage(const MessageOf<vector<double> >*msg,P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
    vector<double> msgData = *msg->getData();

//    console << "CG_DU_Message( ";
//    for(int i=0;i<6;i++) console << msgData[i] << (i<5?",":" ");
//    console <<") " << msgFrom << "->" << module->blockId << "\n";

    bool ready = true;
    for(int i=0;i<6;i++){
        if(neighbors[i][0]==msgFrom) {
//            OUTPUT << "Iter=" << curIteration  <<  ", ID="<< module->blockId << " received the message from " << msgFrom<< endl;
//            printVector(msgData,6,"msgData from "+to_string(msgFrom)+" to "+ to_string(module->blockId));
            neighbors[i][1]=1;
            uq[i]=msgData;
        }
        if(neighbors[i][0]!=0 && neighbors[i][1]==0)
            ready = false;
    }
//    printNeighbors();
    if(ready) {
        clearNeighborsMessage();
        if(isSupport) {
            module->setColor(Color(0.0f,0.0f,1.0f));
        }
        maxIterations=curIteration;
        dup=du;
/*        cerr << "VIS no. " << globVis << ", module: " << module->blockId << "\n";
        globVis++; */
        visualization();
    }
}

// OK initiates visualisation
void ForcesPredictionIPPTCode::CGVisQMessage(P2PNetworkInterface *sender) {
    bID msgFrom = sender->getConnectedBlockBId();
//    console << "CG_VIS_Q_Message " << msgFrom << "->" << module->blockId << "\n";

    bool anyMsgSent=false;
    TO_CHILDREN(
    sendMessage("CG_VIS_Q_MSG",new Message(CG_VIS_Q_MSG),p2p,messageDelay,messageDelayError);
    aggregationCompleted[i]=false;
    anyMsgSent=true;
    )( // to virtual children
    )
    sendMessageToAllNeighbors("CG_DU_MSG",new MessageOf<vector<double> >(CG_DU_MSG,du),messageDelay,messageDelayError,0);
//    if(!anyMsgSent) { // if there are no neighbors except the parent and virtual modules then aggregate the data and return the confirmation
//        sendMessage("CG_INIT_R_MSG",new MessageOf<double >(CG_INIT_R_MSG,CGrr),sender,messageDelay,messageDelayError);
//    }
}





void ForcesPredictionIPPTCode::clearNeighborsMessage() {
    for(int i=0; i<6; i++) {
        if(neighbors[i][1]==1) {
            neighbors[i][1]=0;
        }
    }
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
    double fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4]; // elastic predictor

    if(fz<=0) { // contact
        double mmax = -fz*a/2;
        double mxh = mx - fy*a/2;
        double myh = my + fx*a/2;

//        TfrB = IdentityMatrix6(); // use the commented version below instead if you want the Coulomb friction
/****** We do not use Coulomb friction in the current version. Just stick state when in contact ****************************
        double fx=Fd[0], fy=Fd[1];
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
***************************************/

        // condition for x-tilting (over the y-directed edge (front or back))
        if(fabs(mxh) >= mmax) { // tilting over y-directed edge
//            K11d[3] = {0, Gamma*K11d[3][1], -sign(mx)*0.5*a*(1-Gamma)*K11d[2][2], Gamma*K11d[3][3], 0, 0};
            K11d[3] = Gamma*K11d[3] + (1-Gamma)*(K11d[1]-sign(mxh)*K11d[2])*a*0.5;
        }

        // condition for y-tilting (over the x-directed edge (left or right))
        if(fabs(myh) >= mmax) { // tilting over x-directed edge
//            K11d[4] = {Gamma*K11d[3][1], 0, -sign(my)*0.5*a*(1-Gamma)*K11d[2][2], 0, Gamma*K11d[3][3], 0};
            K11d[4] = Gamma*K11d[4] + (1-Gamma)*(-K11d[0]-sign(myh)*K11d[2])*a*0.5;
        }
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

    //printVector(uq[i],6,"neighbor vct uq["+ to_string(i) +"] id= " + to_string(module->blockId) + ", it "+ to_string(curIteration));
}


void ForcesPredictionIPPTCode::computeDU(bool isInit) {
//    if(module->blockId == 8) console << "------ module 8 -----\n";
//    console << "computeDU, module="<< module->blockId <<", iter=" << curIteration << ", maxIter=" << maxIterations << "\n";
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
            if(neighbors[i][1]==2) {
		computeNeighborDU(i); // compute uq[i] for a virtual module
//		printVector(uq[i],6,"uq["+to_string(i)+"]");
            }
            if(neighbors[i][1]>0) {
                sumK11=sumK11+createK11(i);
                Fpq = Fpq+(createK12(i)*uq[i]);
                if(module->blockId == 4) { // d ebug info for module 4
            mtmp = createK12(i);
//          printMatrix(mtmp,6,6,"CreateK12("+to_string(i)+")");
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
//        printVector(du,6,"vector du module id= " + to_string(module->blockId) + ", iteration "+ to_string(curIteration));

    }	else { //end isFixed
        du = du*0.;
    }

    if (module->blockId==6) {
			printVector(du,6,"DEBUG6("+to_string(curIteration)+"):");
		}
    //sending message to neighbors with du
//    OUTPUT << "size=" << du.size() << endl;
    sendMessageToAllNeighbors("DU_MSG",new MessageOf<vector<double> >(DU_MSG,du),messageDelay,messageDelayError,0);
    //clearing info about du from neighbors
    clearNeighborsMessage();
}


// ******************** VISUALIZATION ********************
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

            // every vector of load is rotated to the (1,0,0) configuration (i.e., longitudal coordinate aligned with X axis, as if the neighbouring module was located on the right)
            vizTable[i]=createRot(i)*(tmpK11*dup+tmpK12*uq[i]);

            // we take the sum of Y or Z torques (for the connection rotated to X axis) from both modules (which is a torque acting at the middle of the connection)
            int di=1-2*(i%2); // i+di will point to the matrices of opposite direction
            tmpK11 = createK11(i+di);
            tmpK12 = createK12(i+di);
//            vizTable[i][4]=(vizTable[i][4]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[4])/2;
//            vizTable[i][5]=(vizTable[i][5]+(createRot(i+di)*(tmpK11*uq[i]+tmpK12*dup))[5])/2;
            vizTable[i][4]=(vizTable[i][4]-(createRot(i)*(tmpK11*uq[i]+tmpK12*dup))[4])/2;
            vizTable[i][5]=(vizTable[i][5]-(createRot(i)*(tmpK11*uq[i]+tmpK12*dup))[5])/2;
        }
        }
    double maxS = 0;

    if(isSupport) { // enforce the unilateral contact conditions with the support, located below the module
        vector<double> Fd=contactStiffnessMatrix(dup)*dup;
        double fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4], mz=Fd[5]; // in global coordinates
        vizTable[1][0]=-fz;  vizTable[1][1]=fy;  vizTable[1][2]=fx;  vizTable[1][3]=-mz;  vizTable[1][4]=my;  vizTable[1][5]=mx;
/*        if(fz<0) { // if in contact then check the condition on torque
            maxS = min(1.,max(abs(mx)/abs(fz*L/2),abs(my)/abs(fz*L/2)));
        } else { // if no contact then possible lost of balance, but we exclude the node from computing the overload condition
            maxS =0.;
        } */
//        if(fz<0 && abs(my)>abs(fz*L/2)) {
//            cout << "lost of balance. ID=" << module->blockId << ", iteration=" << curIteration << "\n";
//        }
    }

/*    if(module->blockId == 31 ) { // debug message
        printMatrix(vizTable,6,6,"VizTable "+to_string(module->blockId));
    }*/

    // checking the maximum load factor
    
    
    for(int i = 0; i<6; i++) {
        double fxMeff=((i<2)?fxMaxV:fxMaxL); // effective value of fxMax depending on the connection's type (vertical or lateral)

        if(neighbors[i][0]!=0 and !isFixed) {

            //set abs values of my and mz
            vizTable[i][4] = abs(vizTable[i][4]);
            vizTable[i][5] = abs(vizTable[i][5]);

            if(-vizTable[i][0]>fxMeff)
                vizTable[i][0] = -fxMeff;

            double mxMeff=(fxMeff+vizTable[i][0]) * a/2; // effective value of mxMax depending on the connection's type (vertical or lateral) and on co-axial loading

            if(vizTable[i][0]>0) // this must be executed AFTER the definition of mxMeff
                vizTable[i][0] = 0;

            if(vizTable[i][4]>mxMeff)
                vizTable[i][4] = mxMeff;

            if(vizTable[i][5]>mxMeff)
                vizTable[i][5] = mxMeff;

            maxS=max(-vizTable[i][0]/fxMeff, maxS);
            maxS=(mxMeff==0)?1:max(vizTable[i][4]/mxMeff, maxS);
            maxS=(mxMeff==0)?1:max(vizTable[i][5]/mxMeff, maxS);

            if(module->blockId < 0 ) { // debug message
                console << "VIS. module " << module->blockId << ", direction " << i << ", fxMeff=" << fxMeff << ", mxMeff=" << mxMeff << "\n";
            }

        }
    }
//    OUTPUT << "Module " << module->blockId << " maximum load factor = "<< maxS << endl;

    //set color for module
    //cout << min(2*color,1.) << " " << min(2*(1-color),1.) << endl;
    module->setColor(Color(min(2*maxS,1.),min(2*(1-maxS),1.),0.0));

    if (curIteration==maxIterations && maxS>=1.-Eps) {
        module->setBlinkMode(true);
    }
    if ( isCentroid ) {
        World *world = BaseSimulator::getWorld();
//        cerr << "Max block ID=" << world->maxBlockId << "\n";
        cerr.precision(17);
        cerr << ",{ " << curIteration << ", { ";
        for(int i=1; i<=world->maxBlockId; i++) {
            ForcesPredictionIPPTCode *bb = (ForcesPredictionIPPTCode *) world->getBlockById(i)->blockCode;
            cerr << "{" << i << ", " << "{";
            for(int j=0;j<5;j++) cerr << bb->du[j] << ", ";
            cerr << bb->du[5] << "}}";
            if(i==world->maxBlockId) cerr << "} }\n";
            else cerr << ", ";
        }
    }
}



void ForcesPredictionIPPTCode::modelBasedStability(mstData &m, int who) { // who<0 -- self assesment, who>=0 -- assessment for a virtual neighbour
    //calculate only if not fixed
    if(isFixed) {
        m.X[0]={0.,0.,0.};
        m.X[1]={0.,0.,0.};
        m.nOfPts=3; // stable configuration
        return;
    }
    if(!isSupport // if not supported (from below by assumption) then the module will not contribute to stability check
    || !(isSupport && (who>=2 || who<0))) { // even if module is supported but virtual neighbour is not in any of 4 horizontal directions (2,3,4,5) then virtual neighbour will not contribute to stability check
        m.X[0]={0.,0.,0.};
        m.X[1]={0.,0.,0.};
        m.nOfPts=0; // not supported configuration
        return;
    }

    vector<double> Fd;
    if(who<0) Fd=createK11(1)*dup; // for self-assesment
    else {
        computeNeighborDU(who);
        Fd=createK11(1)*uq[who]; // for a virtual neighbour
    }
    double fx=Fd[0], fy=Fd[1], fz=Fd[2], mx=Fd[3], my=Fd[4]; // in global coordinates

    if(fz>0) {
        m.X[0]={0.,0.,0.};
        m.X[1]={0.,0.,0.};
        m.nOfPts=0; // not supported configuration
        return;
    }


    double mxh=mx-fy*L/2;
    double myh=my+fx*L/2;
    int ts=0; // tilting state 0=(no tilting), +-1=(tilting in +-X direction), +-10=(tilting in +-Y direction)
    if(-myh>abs(fz*L/2)) { // tilting in +X direction
        ts+=1;
    } else if(myh>abs(fz*L/2)) { // tilting in -X direction
        ts-=1;
    }
    if(mxh>abs(fz*L/2)) { // tilting in +Y direction
        ts+=10;
    } else if(-mxh>abs(fz*L/2)) { // tilting in -Y direction
        ts-=10;
    }
    static const double dir[6][3]={{0,0,1},{0,0,-1},{-1,0,0},{1,0,0},{0,-1,0},{0,1,0}}; // 0 - up (z+1) 1 - down (z-1) 2 - left x-1 3-right x+1 4-front y-1 5 - back y+1
    double Xc=module->position[0]*L, Yc=module->position[1]*L, Zb=module->position[2]*L-L/2;
    if(who>=0) { // correct the X,Y positions for virtual neighbours
        Xc=Xc+dir[who][0]*L;
        Yc=Yc+dir[who][1]*L;
    }
    switch(ts) {
        case 0: // stable
            m.X[0]={0.,0.,0.};
            m.X[1]={0.,0.,0.};
            m.nOfPts=3; // stable configuration
        break;
        case 1: // +X
            m.X[0]={Xc+L/2,Yc+L/2,Zb};
            m.X[1]={Xc+L/2,Yc-L/2,Zb};
            m.nOfPts=2; // unstable configuration
        break;
        case 11: // +X+Y
            m.X[0]={Xc+L/2,Yc+L/2,Zb};
            m.X[1]={0.,0.,0.};
            m.nOfPts=1; // unstable configuration
        break;
        case 10: // +Y
            m.X[0]={Xc-L/2,Yc+L/2,Zb};
            m.X[1]={Xc+L/2,Yc+L/2,Zb};
            m.nOfPts=2; // unstable configuration
        break;
        case 9: // -X+Y
            m.X[0]={Xc-L/2,Yc+L/2,Zb};
            m.X[1]={0.,0.,0.};
            m.nOfPts=1; // unstable configuration
        break;
        case -1: // -X
            m.X[0]={Xc-L/2,Yc+L/2,Zb};
            m.X[1]={Xc-L/2,Yc-L/2,Zb};
            m.nOfPts=2; // unstable configuration
        break;
        case -11: // -X-Y
            m.X[0]={Xc-L/2,Yc-L/2,Zb};
            m.X[1]={0.,0.,0.};
            m.nOfPts=1; // unstable configuration
        break;
        case -10: // -Y
            m.X[0]={Xc-L/2,Yc-L/2,Zb};
            m.X[1]={Xc+L/2,Yc-L/2,Zb};
            m.nOfPts=2; // unstable configuration
        break;
        case -9: // +X-Y
            m.X[0]={Xc+L/2,Yc-L/2,Zb};
            m.X[1]={0.,0.,0.};
            m.nOfPts=1; // unstable configuration
        break;
    };
    return;
}

void ForcesPredictionIPPTCode::aggregateModelBasedStability(mstData &m,mstData &mn) {
    if(m.nOfPts==3) return; // stable configuration
    if(mn.nOfPts==0) return; // no change is to be done
    if(mn.nOfPts==3) { // stable configuration. Update info
        m.X[0]={0.,0.,0.};
        m.X[1]={0.,0.,0.};
        m.nOfPts=3; // stable configuration
        return;
    }
    if(m.nOfPts==1 && mn.nOfPts==1) {
        if(abs(m.X[0][0]-mn.X[0][0])>Eps || abs(m.X[0][1]-mn.X[0][1])>Eps || abs(m.X[0][2]-mn.X[0][2])>Eps) { // two different points to be aggregated
            m.X[1]=mn.X[0];
            m.nOfPts=2;
        }
        return;
    }
    if(m.nOfPts==0) {
        for(int i=0; i<mn.nOfPts; i++) {
            m.X[i]=mn.X[i];
        }
        m.nOfPts=mn.nOfPts;
        return;
    }
    if(m.nOfPts==1 && mn.nOfPts==2) {
        aggregateModelBasedStability(mn,m);
        m.X[0]=mn.X[0];
        m.X[1]=mn.X[1];
        m.nOfPts=mn.nOfPts;
        return;
    }
    // at this moment of the procedure, there are always exactly 2 different points in m.
    vector<double> v1=m.X[1]-m.X[0];
    for(int i=0;i<mn.nOfPts;i++) {
        vector <double> v2=mn.X[i]-m.X[0];
        vector <double> v3={v1[1]*v2[2]-v1[2]*v2[1], v1[2]*v2[0]-v1[0]*v2[2],v1[0]*v2[1]-v1[1]*v2[0]};
        if( sqrt(NORM2(v3)) > Eps ) { // third non-colinear point found --> configuration is stable
//            console << "TU: " << sqrt(NORM2(v3)) << "\n";
            m.X[0]={0.,0.,0.};
            m.X[1]={0.,0.,0.};
            m.nOfPts=3; // stable configuration
            return;
        } else {
//            console << "TU1: sqrt(NORM2(v3))=" << sqrt(NORM2(v3)) << "\n";
//            console << "TU1: NORM2(v3)=" << NORM2(v3) << "\n";
//            console << "v1=(" << v1[0] << ", " << v1[1] << ", " << v1[2] << ")\n";
//            console << "v2=(" << v2[0] << ", " << v2[1] << ", " << v2[2] << ")\n";
//            console << "v3=(" << v3[0] << ", " << v3[1] << ", " << v3[2] << ")\n";

        }
    }
    return;
}

void ForcesPredictionIPPTCode::simplifiedStability(sstData &m, vector<double> &cm, int who) { // updates the angle for simplified stability. who==-1 is self assesment, who>=0 is assesment for a virtual neighbour 'who'.
    if(!isSupport) { // not a support module
        m.X=-100; // no support case
        return;
    }
    sstData a; // temporary range of angles
    vector<double> dx[]={{L/2,L/2},{L/2,-L/2},{-L/2,L/2},{-L/2,-L/2}};
    vector<double> xm={module->position[0]*L, module->position[1]*L};
    for(int i=0; i<4; i++) { // check at 4 corner nodes in the support of the module
        vector<double> x=(xm+dx[i]-cm); // point showing the direction of the middle of the safe angle
        if(sqrt(x[0]*x[0]+x[1]*x[1])<Eps) { // possibly ill-supported node (center of mass is just above the node)
            continue;
        }
        // angle is expressed as the length on the border of [-1,1]x[-1,1] square between (1,0) point and the point given by the 'x' direction
        switch(((x[0]>0)?1:0)+((x[1]>0)?2:0)) {
            case 0: // --
            a.X=((-x[0]<-x[1])?(6-x[0]/x[1]):(4+x[1]/x[0]));
            break;
            case 1: // +-
            a.X=((x[0]<-x[1])?(6-x[0]/x[1]):(8+x[1]/x[0]));
            break;
            case 2: // -+
            a.X=((-x[0]<x[1])?(2-x[0]/x[1]):(4+x[1]/x[0]));
            break;
            case 3: // ++
            a.X=((x[0]<x[1])?(2-x[0]/x[1]):(x[1]/x[0]));
            break;
        }
        a.Y=a.X+2; // rotate 90 deg anti-clockwise (end)
        a.X=a.X-2; // rotate 90 deg clockwise (begining)
        aggregateSimplifiedStability(m,a);
    }
}

// it is assumed that the angles are at least half-angles (i.e., their sum is always a single angle)
void ForcesPredictionIPPTCode::aggregateSimplifiedStability(sstData &a,sstData &b) { // aggregate simplified stability data (two ranges of angles) and save the sum in 'a'
    if(b.X==-100) return; // zero angle
    if(a.X==-100) { // zero angle
        a.X=b.X;
        a.Y=b.Y;
        return;
    }
    if(a.X==100) return; // full angle
    if(b.X==100) { // if 'b' is full angle
        a.X=100; // 'a' becomes full angle
        return;
    }
    if(a.X>b.X) a.X=b.X;
    if(a.Y<b.Y) a.Y=b.Y;
    if(a.Y-8 >= a.X) { // then the sum is full angle
        a.X=100;
    }
    return;
}

// Auxiliary functions

void ForcesPredictionIPPTCode::printVector(vector<double> vec, int row,string desc){
    console << "*************printVec********************\n";
    console << desc << ": ";
        for (int i=0;i<row;i++){
            console << vec[i]<< "\t";
        }
        console << "\n";
}



void ForcesPredictionIPPTCode::printMatrix(vector< vector<double> > matrix, int row, int col,string desc){
    console << "*************printMatrix********************\n";
    console << desc<< "\n";
    for (int i=0;i<row;i++){
        for(int j=0;j<col;j++){
            console << matrix[i][j]<< "\t";
        }
        console << "\n";
    }
    console << "\n";
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
    for(size_t i=0;i<A.size();i++)
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
    for(size_t i=0; i<A.size();i++){
                tmp[i][i] = A[i][i]; // operation
    }
    return tmp;
}
vector< vector<double> > ForcesPredictionIPPTCode::createR(vector< vector<double> > &A){

    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size(),0));

    for(size_t i=0; i<A.size();i++) {
        for(size_t j=0; j<A[0].size();j++) {
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

// vec * vec
double operator*(const vector<double> vec1, const vector<double> vec2){
    size_t smax = min(vec1.size(),vec2.size());
    double tmp = 0;
    for (size_t i=0;i<smax;i++){
        tmp += vec1[i]*vec2[i];
    }
    return tmp;
}

// vec * scal
vector<double> operator*(const vector<double> vec, const double  scal){
    vector<double> tmp = decltype(tmp)(vec.size(),0);
    //cout << vec.size();
        for (size_t i=0;i<vec.size();i++){
            tmp[i] = vec[i]*scal;
        }
    return tmp;
}

// scal * vec
vector<double> operator*(const double  scal, const vector<double> vec){
    vector<double> tmp = decltype(tmp)(vec.size(),0);
    //cout << vec.size();
        for (size_t i=0;i<vec.size();i++){
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
    for (size_t i=0;i<A.size();i++){
            for (size_t j=0;j<vec.size();j++){
                tmp[i]+=( A[i][j]*vec[j]);
            }
        }
    return tmp;
}

// mat * scal
vector< vector<double> > operator*(const vector< vector<double> > A, const double B){
    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size()));
    for(size_t i=0; i<A.size();i++){
            for(size_t j=0;j<A[0].size();j++)	{
                tmp[i][j] = A[i][j] * B;
            }
    }
    return tmp;
}

// scal * mat
vector< vector<double> > operator*(const double B, const vector< vector<double> > A){
    vector< vector<double> > tmp = decltype(tmp)(A.size(), vector<double>(A[0].size()));
    for(size_t i=0; i<A.size();i++){
            for(size_t j=0;j<A[0].size();j++)	{
                tmp[i][j] = A[i][j] * B;
            }
    }
    return tmp;
}

// mat * mat
vector< vector<double> > operator*(const vector< vector<double> > A,const vector< vector<double> > B){
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(B[0].size()));
    for(size_t i=0; i<A.size();i++){
        for(size_t j=0;j<B[0].size();j++)	{
            for(size_t k=0;k<B.size();k++){
                result[i][j] += A[i][k] * B[k][j]; // operation
            }
        }
    }
    return result;
}

// mat + mat
vector< vector<double> > operator+(vector< vector<double> > A,vector< vector<double> > B) {
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
    for(size_t i=0; i<A.size();i++){
            for(size_t j=0;j<A[0].size();j++)	{
                    result[i][j] = A[i][j] + B[i][j]; // operation
            }
        }
    return result;
}

// mat - mat
vector< vector<double> > operator-(vector< vector<double> > A,vector< vector<double> > B) {
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
    for(size_t i=0; i<A.size();i++){
            for(size_t j=0;j<A[0].size();j++)	{
                    result[i][j] = A[i][j] - B[i][j]; // operation
            }
        }
    return result;
}

// - mat
vector< vector<double> > operator-(vector< vector<double> > A) {
    vector< vector<double> > result = decltype(result)(A.size(), vector<double>(A[0].size()));
    for(size_t i=0; i<A.size();i++){
            for(size_t j=0;j<A[0].size();j++)	{
                    result[i][j] = -A[i][j]; // operation
            }
        }
    return result;
}


/**********************/

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
    MessageOf<vector<double> >*msgType = (MessageOf<vector<double> >*)msg.get();
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


void _CGInitQMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    cb->CGInitQMessage(sender);
}

void _CGInitRMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<double>*msgType = (MessageOf<double>*)msg.get();
    cb->CGInitRMessage(msgType,sender);
}

void _CGAlphaQMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<double>*msgType = (MessageOf<double>*)msg.get();
    cb->CGAlphaQMessage(msgType,sender);
}

void _CGAlphaRMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<double>*msgType = (MessageOf<double>*)msg.get();
    cb->CGAlphaRMessage(msgType,sender);
}

void _CGBetaQMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<double>*msgType = (MessageOf<double>*)msg.get();
    cb->CGBetaQMessage(msgType,sender);
}

void _CGBetaRMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<double>*msgType = (MessageOf<double>*)msg.get();
    cb->CGBetaRMessage(msgType,sender);
}

void _CGDMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<vector<double> >*msgType = (MessageOf<vector<double> >*)msg.get();
    cb->CGDMessage(msgType,sender);
}

void _CGDUMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    MessageOf<vector<double> >*msgType = (MessageOf<vector<double> >*)msg.get();
    cb->CGDUMessage(msgType,sender);
}

void _CGVisQMessage(BlockCode *codebloc,MessagePtr msg, P2PNetworkInterface*sender) {
    ForcesPredictionIPPTCode *cb = (ForcesPredictionIPPTCode*)codebloc;
    cb->CGVisQMessage(sender);
}
