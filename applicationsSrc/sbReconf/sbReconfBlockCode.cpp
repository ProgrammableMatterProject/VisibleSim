/*
 * SbReconfBlockCode.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <sstream>
#include <memory>

#include "sbReconfBlockCode.h"
#include "events/scheduler.h"
#include "motion/translationEvents.h"

/*************************
STEP 0: get map
STEP 1: recieves all ackmapq from neighbors
STEP 2: recieves searchHeadOfTrainMessage
STEP 3: recieves headFoundMessage

**************************/

//#define verbose 1

using namespace std;
using namespace SmartBlocks;

const int time_offset=100;

SbReconfBlockCode::SbReconfBlockCode(SmartBlocksBlock *host):SmartBlocksBlockCode(host) {
    // OUTPUT << "SbReconfBlockCode constructor" << endl;
    scheduler = getScheduler();
    smartBlock = (SmartBlocks::SmartBlocksBlock*)hostBlock;

    targetGrid=NULL;
    lattice = BaseSimulator::getWorld()->lattice;
    nbreStats=0;
    for (int i = 0; i < 10; i++) {
        tabStatsData[i] = 0;
    }
}

SbReconfBlockCode::~SbReconfBlockCode() {
    // OUTPUT << "SbReconfBlockCode destructor" << endl;
    // delete [] unlockPathTab;

    if (targetGrid) {
        delete [] targetGrid;
        targetGrid = NULL;
    }

    if (capabilities){
        delete capabilities;
        capabilities = NULL;
    }
}

void SbReconfBlockCode::startup() {
    stringstream info;
    block = (SmartBlocks::SmartBlocksBlock*)(hostBlock);
    wrl = SmartBlocks::getWorld();
    OUTPUT << "Starting " << block->blockId << endl;

    lattice = BaseSimulator::getWorld()->lattice;

    nbreOfWaitedAnswers=0;
    block2Answer = NULL;
    _next = NULL;
    _previous = NULL;
    _numPrev = -1;
    _isBorder=false;
    _isTrain=false;
    _isSingle=false;
    _isHead=false;
    _isEnd=false;
    _motionDir.set(0,0);
    //_currentStage=1;
    _currentMove=0;
    possibleRules=NULL;
    unlockPathTab=NULL;
    unlockPathTabSize=0;
    unlockMotionStep=0;

    // initialise la liste des étapes
    for (int i=0; i<4; i++) {
        tabSteps[i] = false;
    }
    //If i am master block
    if(block->blockId == 1)
    { posGrid.x = block->position[0];
        posGrid.y = block->position[1];
        presence *tab = getTargetGridPtr(gridSize);
#ifdef verbose
        scheduler->trace(info.str(),block->blockId);
#endif
        targetGrid = new presence[gridSize[0]*gridSize[1]];
        memcpy(targetGrid,tab,gridSize[0]*gridSize[1]*sizeof(presence));
        wellPlaced = targetGrid[posGrid.y*gridSize[0]+posGrid.x]==fullCell;
        tabSteps[0] = true;

        // compte le nombre de cellules pleines
        int n=gridSize[0]*gridSize[1];
        _nbreGoalCells=0;
        presence *ptr=tab;
        while (n--) {
            _nbreGoalCells+=(*ptr==fullCell);
            ptr++;
        }

        sendMapToNeighbors(NULL);
    } else {
        targetGrid=NULL;
#ifdef verbose
        scheduler->trace(info.str(),block->blockId);
#endif
    }
}

/** Prepare unlock table for next unlock action **/
void SbReconfBlockCode::prepareUnlock(const vector<short>&path,int step) {
    vector<short>::const_iterator cs=path.begin();
    unlockPathTabSize = path.size();
    if (unlockPathTabSize) {
        delete [] unlockPathTab;
        unlockPathTab = new short[unlockPathTabSize];
        int i=0;
        while (cs!=path.end()) {
            unlockPathTab[i++] = *cs;
            cs++;
        }
    }
    unlockMotionStep = step;
}

void SbReconfBlockCode::printRules() {
#ifdef verbose
    /* affiche les règles*/
    stringstream info;
    info.str("");
    if (!possibleRules || possibleRules->empty()) {
        info << "No rule!";
    } else {
        if (_isHead) info << "H";
        if (_isEnd) info << "E";
        info << possibleRules->size();
        std::reverse_iterator<vector<Validation*>::iterator> rev_until (possibleRules->begin());
        std::reverse_iterator<vector<Validation*>::iterator> rev_from (possibleRules->end());
        while (rev_from!=rev_until) {
            info  << "/" <<(*rev_from)->isZeroDistance << ","<< (*rev_from)->gain << (*rev_from)->capa->name;
            rev_from++;
        }
    }
    scheduler->trace(info.str(),hostBlock->blockId,GOLD);
#endif
}

void SbReconfBlockCode::applyRules() {
    // supprime l'ancienne liste de règles
    if (possibleRules) {
        while (!possibleRules->empty()) {
            delete (possibleRules->back());
            possibleRules->pop_back();
        }
        delete possibleRules;
        possibleRules=NULL;
    }
// apply rules
    posGrid.x = block->position[0];
    posGrid.y = block->position[1];
    getPresenceMatrix(posGrid,_pm);
    PresenceMatrix localTargetGrid;
    getLocalTargetGrid(posGrid,localTargetGrid);
    // OUTPUT << localTargetGrid;
    PointCel neighborsDirection[5];
    SbReconfBlockCode*bc;
    P2PNetworkInterface *ni;
    /*for (int i=0; i<4; i++) {
      ni = block->getInterface(SLattice::Direction(i));
      if (ni->connectedInterface!=NULL) {
      bc = (SbReconfBlockCode*)(ni->connectedInterface->hostBlock->blockCode);
      neighborsDirection[i]=bc->_motionDir;
      } else {
      neighborsDirection[i].set(0,0);
      }
      }*/
//    OUTPUT << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1) << endl;
    neighborsDirection[4] = _motionDir;
    for (int i=0; i<4; i++) {
        ni = block->getInterface(SLattice::Direction(i));
        if (_previous && _previous==ni) {
            // OUTPUT << "previous=" << i << endl;
            bc = (SbReconfBlockCode*)(ni->connectedInterface->hostBlock->blockCode);
            neighborsDirection[i]=bc->_motionDir;
        } else {
            if (ni->connectedInterface!=NULL && ((SbReconfBlockCode*)(ni->connectedInterface->hostBlock->blockCode))->wellPlaced) {
                neighborsDirection[i].set(0,0);
            } else {
                neighborsDirection[i].unSet();
            }
        }
    }
    possibleRules = getCapabilities()->validateDirection(_pm,localTargetGrid,neighborsDirection);
    if (possibleRules && !possibleRules->empty()) {
        _motionDir = possibleRules->back()->capa->tabMotions[0]->vect;
//        OUTPUT << "_motionDir =" << _motionDir << endl;
    }


#ifdef verbose
    printRules();
    if (!possibleRules || possibleRules->empty()) {
        OUTPUT << "PM=" << endl;
        OUTPUT << _pm << endl;
    }
#endif // verbose
    setRulesColor();
}

void SbReconfBlockCode::setRulesColor() {
    if (_isBorder) {
        if (_isTrain) {
            block->setColor(_isHead?RED:_isEnd?GOLD:ORANGE);
        } else {
            block->setColor(_isHead?RED:_isEnd?GREY:PINK);
        }

    } else {
        block->setColor(_isHead?RED:wellPlaced?YELLOW:GREEN);
    }
}

void SbReconfBlockCode::startMotion(Time t,const PointCel &mv,int step,const vector<short>&path) {
    prepareUnlock(path,step);
    Vector3D finalPosition;
    finalPosition.set(block->position.pt[0]+mv.x,block->position.pt[1]+mv.y,0);
    scheduler->schedule(new TranslationStartEvent(t,block,finalPosition));
#ifdef verbose
    stringstream info;
    info.str("");
    info << "TranslationStartEvent(" << t << ") vect=" << mv << "  unlock=" << path.size() << " step=" << step;
    scheduler->trace(info.str(),block->blockId,LIGHTGREY);
#endif
}

void SbReconfBlockCode::processLocalEvent(EventPtr pev) {
    stringstream info;
    MessagePtr message;

    switch (pev->eventType) {
    case EVENT_TRANSLATION_END: {
#ifdef verbose
        info.str("");
        info << "rec.: EVENT_TRANSLATION_END, tabUnlockPathSize=" << unlockPathTabSize << " order=" << unlockMotionStep;
        scheduler->trace(info.str(),hostBlock->blockId);
#endif
        posGrid.x = block->position[0];
        posGrid.y = block->position[1];
        getPresenceMatrix(posGrid,_pm);
        wellPlaced = targetGrid[posGrid.y*gridSize[0]+posGrid.x]==fullCell;

        addStat(2,1);
        if (_isEnd) { // c'est une fin de train (et de ligne)
            addStat(0,1);
            // GlutContext::mustSaveImage=true;
        }
        if (unlockPathTabSize>0) { // c'est une fin de ligne (sauf train)
            addStat(1,1);
            printStats();
            //GlutContext::mustSaveImage=true;
        }
        //printStats();
        _isTrain=false;

        // prepare for next motion
        if (unlockPathTabSize>0) {
            // envoie le message de déblocage
            P2PNetworkInterface *p2p = block->getInterface(SLattice::Direction(unlockPathTab[0]));
            UnlockMessage *message = new UnlockMessage(unlockPathTab+1,unlockPathTabSize-1,unlockMotionStep);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
#ifdef verbose
            info.str("");
            info << "send UnlockMessage("<< unlockMotionStep << ") to " << p2p->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
        } else {
/***************************************************************
 si c'est le dernier du train et qu'il a fini son déplacement
 on reconnecte le train !
 Si il existe une règle de fin il reste la fin du train suivant
***************************************************************/
            setRulesColor();
            if (_isEnd) {

                sendAsk4EndToNeighbors(NULL);
            }
        }
    }
        break;

    case EVENT_NI_RECEIVE:
        message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
        P2PNetworkInterface * recvInterface = message->destinationInterface;
        switch(message->id) {
        case MAP_MSG_ID : {
            MapMessage_ptr recvMessage = std::static_pointer_cast<MapMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << hostBlock->blockId << " rec. MapMessage : " << recvMessage->posx << "," << recvMessage->posy << " from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            if (targetGrid) {
                sendAckMap(recvInterface);
            } else {
                tabSteps[0] = true;
                posGrid.x = recvMessage->posx;
                posGrid.y = recvMessage->posy;
                gridSize[0] = recvMessage->gridw;
                gridSize[1] = recvMessage->gridh;
                _nbreGoalCells = recvMessage->nbreGoalCells;
                targetGrid = new presence[gridSize[0]*gridSize[1]];
                memcpy(targetGrid,recvMessage->targetGrid,gridSize[0]*gridSize[1]*sizeof(presence));

                wellPlaced = targetGrid[posGrid.y*gridSize[0]+posGrid.x]==fullCell;
                //block->setDisplayedValue(-1);
                setRulesColor();
                block2Answer=recvInterface;
                sendMapToNeighbors(block2Answer);
#ifdef verbose
                info.str("");
                info << "TargetState(" << posGrid.x << "," << posGrid.y << ")  " << wellPlaced;
                scheduler->trace(info.str(),hostBlock->blockId);
#endif
            }
            if (nbreOfWaitedAnswers==0) {
                if (block2Answer!=NULL) {
                    sendAckMap(block2Answer);
                    block2Answer=NULL;
#ifdef verbose
                    info.str("");
                    info << " READY";
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif
                } else {
#ifdef verbose
                    info.str("");
                    info << "Master READY";
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif
                    setRulesColor();

                    //block->setDisplayedValue(-1);

                }
                tabSteps[1] = true;
                createBorder();
            }
        }
            break;

        case ACKMAP_MSG_ID : {
            AckMapMessage_ptr recvMessage = std::static_pointer_cast<AckMapMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. AckMapMessage(" << nbreOfWaitedAnswers << ") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            nbreOfWaitedAnswers--;
            if (nbreOfWaitedAnswers==0) {
                if (block2Answer!=NULL) {
                    sendAckMap(block2Answer);
                    block2Answer=NULL;
#ifdef verbose
                    info.str("");
                    info << " READY";
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif
                } else {
#ifdef verbose
                    info.str("");
                    info << "MASTER READY";
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                    setRulesColor();

                    addStat(0,0);
                    addStat(1,0);
                    addStat(2,0);
                    printStats();

                    //block->setDisplayedValue(-1);
                }
                tabSteps[1] = true;
                createBorder();
            }
        }
            break;

        case HEAD_MSG_ID : {
            SearchHeadMessage_ptr recvMessage = std::static_pointer_cast<SearchHeadMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. SearchHeadMessage() from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            if (!tabSteps[1]) {
                tabMemorisedMessages[2] = message;
                tabSteps[2]=true;
            } else {
                step2(message);
            }

        }
            break;

        case HBCK_MSG_ID : {
            SearchBackHeadMessage_ptr recvMessage = std::static_pointer_cast<SearchBackHeadMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. SearchBackHeadMsg("<<(recvMessage->exceptionBlock==NULL?-1:recvMessage->exceptionBlock->hostBlock->blockId)<<") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            if (recvMessage->exceptionBlock!=NULL) {
                SLattice::Direction dir= SLattice::North;
                int i=4;
                while (i-- && block->getInterface(dir)->connectedInterface!=recvMessage->exceptionBlock) {
                    dir=SLattice::Direction((int(dir)+1)%4);
                }
                OUTPUT << "except dir=" << dir << endl;
                dir=SLattice::Direction((int(dir)+1)%4);
                i=3;
                while (i-- && block->getInterface(dir)==NULL) {
                    dir=SLattice::Direction((int(dir)+1)%4);
                }
                if (i) {
                    sendSearchBackHeadMessage(block->getInterface(dir));
                }
            } else {
                _previous = recvMessage->sourceInterface->connectedInterface;
                _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
                applyRules();
                if (possibleRules && !possibleRules->empty()) {
                    // on recherche une règle de tete
                    //possibleRules->pop_back();
                    while (!possibleRules->empty() && !possibleRules->back()->capa->isHead) possibleRules->pop_back();
#ifdef verbose
                    OUTPUT << "Cherche head" << endl;
                    printRules();
#endif // verbose
                    // il existe une regle de tete
                    if (!possibleRules->empty()) {
                        printRules();
                        /********************************************/
                        /*** cas particulier des éléments uniques ***/
                        /*** qui sont head et end en meme temps   ***/
                        if (possibleRules->back()->capa->isEnd) {
                            Capability *capa = possibleRules->back()->capa;
#ifdef verbose
                            info.str("");
                            info << "special motion :" << capa->name ;
                            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                            vector<Motion*>::const_iterator cm = capa->tabMotions.begin();
                            while (cm!=capa->tabMotions.end()) {
                                singleMotion(*cm,capa);
                                cm++;
                            }
                        } else {
                            // si le block était le block de départ de la recherche (_isEnd)
                            if (_isEnd) {
#ifdef verbose
                                OUTPUT << "le block de départ de la recherche " << endl;
#endif // verbose
                                _previous = getBorderPreviousNeightbor(NULL);
                                _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
                                _next = getBorderNextNeightbor();
                                //block->setDisplayedValue(0);
                                _isTrain = true;
                                _isHead = true;
                                _isEnd = false;
                                _motionDir = possibleRules->back()->capa->tabMotions[0]->vect;

#ifdef verbose
                                info.str("");
                                info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
                                     << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
                                     << " _isHead = " << _isHead << " _isEnd = " << _isEnd;
                                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                                SearchEndTrainMessage *message = new SearchEndTrainMessage(1);
                                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
                                info.str("");
                                info << "send SearchEndTrainMessage to " << _next->connectedInterface->hostBlock->blockId;
                                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                                setRulesColor();
                            } else {
                                // on a trouvé la tete on cherche la queue du train
                                _motionDir = possibleRules->back()->capa->tabMotions[0]->vect;
                                PointCel pos = *possibleRules->back()->capa->linkNextPos;
                                _next = block->getP2PNetworkInterfaceByRelPos(Cell3DPosition(pos.x, pos.y, 0));
                                SearchEndTrainMessage *message = new SearchEndTrainMessage(1);
                                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
                                info.str("");
                                info << "send SearchEndTrainMessage to " << _next->connectedInterface->hostBlock->blockId;
                                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                                setRulesColor();
                                //block->setDisplayedValue(0);
                                _isTrain = true;
                                _isHead = true;
                                _isEnd = false;
                                _previous = NULL;
                            }
                        }
                    } else {
                        // si pas de rule de tete : on demande un retour en arriere
                        if (_isEnd || _next==NULL) {
#ifdef verbose
                            OUTPUT << "le block de départ de la recherche 2" << endl;
#endif // verbose
                            setRulesColor();
                            /**
                               On peut envoyer un message searchHeadBack particulier qui demande d'ometre le block courant car il est un isthme !
                               on le renvoie au sender
                               SearchBackHeadMessage *message = new SearchBackHeadMessage(blockId);

                            **/
                            sendSearchBackHeadMessage(_previous,recvMessage->destinationInterface);
                        } else {
//                        OUTPUT << "case 1" << endl;
                            _previous = recvMessage->sourceInterface->connectedInterface;
                            _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
                            applyRules();
                            _next = getBorderNextNeightbor(_previous);
#ifdef verbose
                            info.str("");
                            info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
                                 << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
                                 << " _isHead = " << _isHead << " _isEnd = " << _isEnd;
                            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                            sendSearchBackHeadMessage(_next);
                        }
                    }
                } else {
//                        OUTPUT << "case 2" << endl;
                    _previous = recvMessage->sourceInterface->connectedInterface;
                    _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
                    applyRules();
                    _next = getBorderNextNeightbor(_previous);
#ifdef verbose
                    info.str("");
                    info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
                         << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
                         << " _isHead = " << _isHead << " _isEnd = " << _isEnd;
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                    sendSearchBackHeadMessage(_next);
                }
            }
        }
            break;

        case END_MSG_ID : {
            SearchEndTrainMessage_ptr recvMessage = std::static_pointer_cast<SearchEndTrainMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. SearchEndTrainMessage("<< recvMessage->num <<") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);

            OUTPUT << "isSingle : " << _isSingle << endl;
#endif // verbose
            if (!tabSteps[1]) {
                tabMemorisedMessages[3] = message;
                tabSteps[3]=true;
            } else {
                _previous = recvMessage->sourceInterface->connectedInterface;
                _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
                step3(message);
                tabSteps[3]=false;
            }

        }
            break;

        case TRAIN_READY_MSG_ID : {
            TrainReadyMessage_ptr recvMessage = std::static_pointer_cast<TrainReadyMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. TrainReadyMessage(" << recvMessage->queueFound << ") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif

            if (recvMessage->queueFound) {
#ifdef verbose
                info.str("");
                info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
                     << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
                     << " _isHead = " << _isHead << " _isEnd = " << _isEnd ;
                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                printRules();
//                        Capability *capa = possibleRules->back()->capa;

                _isTrain=true;
// si on est à la tete du train on peut créer des lignes
                if (_isHead) {
                    createLine(scheduler->now(),true);
                } else { // sinon on propage
                    TrainReadyMessage *message = new TrainReadyMessage(true);
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                    info.str("");
                    info << "send TrainReadyMessage(true) to " << _previous->connectedInterface->hostBlock->blockId;
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                    _isEnd=false;
                }
            } else {
                applyRules();
                Capability *capa = possibleRules->back()->capa;
                if (capa && !capa->isHead) {
                    // il faut chercher une règle de queue dans le bloc courant qui ne forme pas d'isthme
                    while (!possibleRules->empty() && !possibleRules->back()->capa->isEnd) {
                        possibleRules->pop_back();
                    }
                    bool test=!possibleRules->empty();
                    if (test && !_isSingle) {
                        capa = possibleRules->back()->capa;
                        test &= !testIsthmusTail(capa->linkPrevPos->x,capa->linkPrevPos->y);
                    }
                    if (test) {
                        TrainReadyMessage *message = new TrainReadyMessage(true);
                        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                        info.str("");
                        info << "send TrainReadyMessage(true) to " << _previous->connectedInterface->hostBlock->blockId;
                        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                        _isTrain = true;
                        _isEnd = true;
                    } else {
                        // sinon on propage faux vers le début
                        TrainReadyMessage *message = new TrainReadyMessage(false);
                        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                        info.str("");
                        info << "send TrainReadyMessage(false) to " << _previous->connectedInterface->hostBlock->blockId;
                        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                        _isTrain = false;
                        _isEnd = false;
                    }
                } else {
                    /*DisableTrainMessage *message = new DisableTrainMessage();
                      scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
                      info.str("");
                      info << "send DisableTrainMessage() to " << _next->connectedInterface->hostBlock->blockId;
                      scheduler->trace(info.str(),hostBlock->blockId);
                      _isTrain = false;
                      _isEnd=false;*/
                }
            }
        }
            break;

        case CREATE_LINE_MSG_ID : {
            CreateLineMessage_ptr recvMessage = std::static_pointer_cast<CreateLineMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << " rec. CreateLineMessage(" << recvMessage->etime << ") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            createLine(recvMessage->etime,false);
        }
            break;

        case SET_RDV_MSG_ID : {
            SetRDVMessage_ptr recvMessage = std::static_pointer_cast<SetRDVMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << hostBlock->blockId << " rec. SetRDVMessage(" << recvMessage->rdvTime << "," << recvMessage->motionVector << ") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            if (!_isHead && !_isHeadOfLine) {
                SetRDVMessage *message = new SetRDVMessage(recvMessage->rdvTime ,recvMessage->motionVector);
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                info.str("");
                info << "send SetRDVMessage to " << _previous->connectedInterface->hostBlock->blockId;
                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            }
            startMotion(recvMessage->rdvTime,recvMessage->motionVector,0,possibleRules->back()->capa->tabUnlockPath);
        }
            break;

        case UNLOCK_MSG_ID : {
            UnlockMessage_ptr recvMessage = std::static_pointer_cast<UnlockMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << hostBlock->blockId << " rec. UnlockMessage(" << recvMessage->sz << "," << recvMessage->step << ") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            if (recvMessage->sz==0) {
                //_previous = NULL;
                _isHeadOfLine = true;
                // case ou le bloc de fin est lui meme sur un angle
                Capability *capa = possibleRules->back()->capa;
                if (capa->isAngle || capa->isEnd) {
                    vector<Motion*>::const_iterator cm = capa->tabMotions.begin();
                    Motion *currentMotion=NULL;
                    while (cm!=capa->tabMotions.end()) {
                        if ((*cm)->time==recvMessage->step) {
                            singleMotion(*cm,capa);
                            currentMotion = *cm;
                        }
                        cm++;
                    }
                    // si on a pas trouve de déplacement valide : fin de déplacement
                    if (!currentMotion) {
                        setRulesColor();
                        // apply rules
                        applyRules();
                        // on cherche la premiere règle qui est un dernier
                        while (!possibleRules->empty() && !possibleRules->back()->capa->isEnd) possibleRules->pop_back();

                        printRules();

                        _previous = getBorderPreviousNeightbor(NULL);
                        _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
                        _next = NULL;
                        _isTrain = false;
                        // envoie le message de reconstruction du train
                        ReconnectTrainMessage *message = new ReconnectTrainMessage(possibleRules && possibleRules->size()>0);
                        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                        info.str("");
                        info << "send ReconnectTrainMessage to " << _previous->connectedInterface->hostBlock->blockId;
                        scheduler->trace(info.str(),hostBlock->blockId,YELLOW);
#endif // verbose
                    }
                } else { // si non on propage
                    createLine(scheduler->now(),true);
                }
            } else {
                // envoie le message de déblocage
                P2PNetworkInterface *p2p = block->getInterface(SLattice::Direction(recvMessage->tab[0]));
                UnlockMessage *message = new UnlockMessage(recvMessage->tab+1,recvMessage->sz-1,recvMessage->step);
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
#ifdef verbose
                info.str("");
                info << "send UnlockMessage to " << p2p->connectedInterface->hostBlock->blockId;
                scheduler->trace(info.str(),hostBlock->blockId);
#endif
            }
        }
            break;

        case SINGLEMV_MSG_ID : {
            SingleMoveMessage_ptr recvMessage = std::static_pointer_cast<SingleMoveMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << hostBlock->blockId << " rec. SingleMoveMessage(" << recvMessage->sz << "," << recvMessage->step << ") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            if (recvMessage->sz==0) {
                startMotion(recvMessage->startTime,recvMessage->motionVector,recvMessage->step,recvMessage->unlockPath);
            } else {
                // envoie le message de déblocage
                P2PNetworkInterface *p2p = block->getInterface(SLattice::Direction(recvMessage->tab[0]));
                SingleMoveMessage *message = new SingleMoveMessage(recvMessage->tab+1,recvMessage->sz-1,recvMessage->startTime,recvMessage->motionVector,recvMessage->unlockPath,recvMessage->step);
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
#ifdef verbose
                info.str("");
                info << "send SingleMoveMessage("<< recvMessage->sz-1 <<") to " << p2p->connectedInterface->hostBlock->blockId;
                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            }
        }
            break;

        case RECONNECT_MSG_ID : {
            ReconnectTrainMessage_ptr recvMessage = std::static_pointer_cast<ReconnectTrainMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. ReconnectTrainMsg("<< recvMessage->hasRule <<") from " << sourceId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            if (recvMessage->hasRule) {
                _next = recvMessage->sourceInterface->connectedInterface;
                _previous = getBorderPreviousNeightbor(_next);
                if (_previous==_next) {
                    _previous=NULL;
                }
            } else {
                _previous = recvMessage->sourceInterface->connectedInterface;
                _next = getBorderNextNeightbor();
            }
            _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
            reconnect(recvMessage->hasRule);
        }
            break;

        case ASK4END_MSG_ID : {
            Ask4EndMessage_ptr recvMessage = std::static_pointer_cast<Ask4EndMessage>(message);
            //block->setDisplayedValue(-1);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. Ask4EndMessage(" << recvMessage->currentMove << ") from " << sourceId;
//					scheduler->trace(info.str(),hostBlock->blockId);
#endif
            if (_currentMove==recvMessage->currentMove) {
                sendAns4EndMessage(recvInterface,0);
            } else {
                init();
                block2Answer=recvInterface;
                sendAsk4EndToNeighbors(block2Answer);
                if (nbreOfWaitedAnswers==0) {
                    if (block2Answer!=NULL) {
                        sendAns4EndMessage(block2Answer,_nbreWellPlacedBlocks);
                        block2Answer=NULL;
                    } else {
                        setRulesColor();
                    }
                }
            }
        }
            break;

        case ANS4END_MSG_ID : {
            Ans4EndMessage_ptr recvMessage = std::static_pointer_cast<Ans4EndMessage>(message);
#ifdef verbose
            unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
            info.str("");
            info << "rec. Ans4EndMessage(" << recvMessage->nbreWellPlaced << "," << nbreOfWaitedAnswers << ") from " << sourceId;
//					scheduler->trace(info.str(),hostBlock->blockId);
#endif
            nbreOfWaitedAnswers--;
            _nbreWellPlacedBlocks+=recvMessage->nbreWellPlaced;
            if (nbreOfWaitedAnswers==0) {
                if (block2Answer!=NULL) {
                    sendAns4EndMessage(block2Answer,_nbreWellPlacedBlocks);
                    block2Answer=NULL;
                } else {
                    setRulesColor();
#ifdef verbose
                    OUTPUT << _pm << endl;
                    OUTPUT << block->blockId << "._numPrev =" << _numPrev << endl;
#endif

                    if (_isSingle)  {
#ifdef verbose
                        OUTPUT << "isSingle" << endl;
#endif
                        _next=NULL;
                        _previous = getBorderSinglePrevious();
                        _numPrev=-1;
                    } else {
                        if (_numPrev!=-1 && (_previous = getBorderNeighborById(_numPrev))!=NULL) {
                            _next = getBorderNextNeightborNoWellPlaced(_previous);
                        } else {
                            _next = getBorderNextNeightborNoWellPlaced(NULL);
                            _previous = getBorderPreviousNeightborNoWellPlaced(_next);
                        }
                        _numPrev = (_previous?_previous->connectedInterface->hostBlock->blockId:-1);
                    }

#ifdef verbose
                    info.str("");
                    info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
                         << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
                         << " _isHead = " << _isHead << " _isEnd = " << _isEnd;
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                    applyRules();

                    if (_nbreWellPlacedBlocks!=_nbreGoalCells) {

                        if (possibleRules && possibleRules->back()) {
#ifdef verbose
                            info.str("");
                            info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
                                 << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
                                 << " _isHead = " << _isHead << " _isEnd = " << _isEnd;
                            scheduler->trace(info.str(),hostBlock->blockId);
#endif
                        }
                        /********************************************/
                        /*** cas particulier des éléments uniques ***/
                        /*** qui sont head et end en meme temps   ***/
                        if (possibleRules->back()->capa->isEnd && possibleRules->back()->capa->isHead) {
                            Capability *capa = possibleRules->back()->capa;
#ifdef verbose
                            info.str("");
                            info << "special motion :" << capa->name ;
                            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                            vector<Motion*>::const_iterator cm = capa->tabMotions.begin();
                            while (cm!=capa->tabMotions.end()) {
                                singleMotion(*cm,capa);
                                cm++;
                            }
                        } else
                            // on garde en priorité une règle de queue
                            // il faut que la règle courante soit une regle de fin
                            if (possibleRules) {
                                while (!possibleRules->empty() && !possibleRules->back()->capa->isEnd) {
                                    possibleRules->pop_back();
                                }
                                printRules();
                                if (!possibleRules->empty()) {
                                    /** on peut poursuivre le déplacement du train à partir du meme bloc de fin **/
                                    _isEnd=true;
                                    _next=NULL;
                                    ReconnectTrainMessage *message = new ReconnectTrainMessage(possibleRules && possibleRules->size()>0);
                                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                                    info.str("");
                                    info << "send ReconnectTrainMessage to " << _previous->connectedInterface->hostBlock->blockId;
                                    scheduler->trace(info.str(),hostBlock->blockId,YELLOW);
#endif // verbose
                                } else {
                                    /** on ne peut pas repartir de la queue courante, on recherche un train complet **/
                                    applyRules();

                                    // send searchHeadMessage
                                    SearchHeadMessage *message = new SearchHeadMessage();
                                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                                    info.str("");
                                    info << "send SearchHeadMessage to " << _previous->connectedInterface->hostBlock->blockId;
                                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                                }
                            } else {
                                /** on ne peut pas repartir de la queue courante, on recherche un train complet **/
                                applyRules();

                                // send searchHeadMessage
                                SearchHeadMessage *message = new SearchHeadMessage();
                                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                                info.str("");
                                info << "send SearchHeadMessage to " << _previous->connectedInterface->hostBlock->blockId;
                                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                            }
                    } else {
                        printStats();
                    }
                }
            }
        }
            break;


        default :
            cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
            break;
        }
        break;
    }
    setRulesColor();
}

void SbReconfBlockCode::init() {
    /*tabSteps[1] = false;
      tabSteps[2] = false;
      tabSteps[3] = false;*/
    posGrid.x = block->position[0];
    posGrid.y = block->position[1];
    wellPlaced = targetGrid[posGrid.y*gridSize[0]+posGrid.x]==fullCell;
    setRulesColor();
    //block->setDisplayedValue(-1);
    _isTrain = false;
    _isBorder = false;
    /*_isHead = false;
      _isEnd = false;*/
    _motionDir.set(0,0);

    setRulesColor();
}

BlockCode* SbReconfBlockCode::buildNewBlockCode(BuildingBlock *host) {
    return(new SbReconfBlockCode((SmartBlocksBlock*)host));
}

void SbReconfBlockCode::step2(MessagePtr message) {
    SearchHeadMessage_ptr recvMessage = std::static_pointer_cast<SearchHeadMessage>(message);
    stringstream info;

    _isTrain = false;
    _next = recvMessage->sourceInterface->connectedInterface;
    _previous=NULL;
    _isEnd=false;
    applyRules();
    if (possibleRules && !possibleRules->empty()) {
        Capability *capa = possibleRules->back()->capa;
        _isTrain = false;
        if (capa->isHead) {
            // send searchEndTrainMessage
#ifdef verbose
            info.str("");
            info << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1);
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            _previous = NULL;
            SearchEndTrainMessage *message = new SearchEndTrainMessage(1);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
            info.str("");
            info << "send SearchEndTrainMessage to " << _next->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif
            setRulesColor();
            //block->setDisplayedValue(0);
            _isHead = true;
        } else /*if (capa->linkPrevPos)*/ {
            _previous = getBorderPreviousNeightbor(_next);
            _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
#ifdef verbose
            info.str("");
            info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1);
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            _isHead = false;
            //block->setDisplayedValue(-1);

            // send searchHeadMessage
            SearchHeadMessage *message = new SearchHeadMessage();
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
            info.str("");
            info << "send SearchHeadMessage to " << _previous->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        }
    } else /*if (_isBorder && _next) */{
        // si pas de rule : on demande un retour en arriere
#ifdef verbose
        info.str("");
        info << "send SearchBackHeadMessage to " << _next->connectedInterface->hostBlock->blockId;
        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        _isHead = false;
        _isTrain = false;
        _isBorder = false;
        //block->setDisplayedValue(-1);

        setRulesColor();
        SearchBackHeadMessage *message = new SearchBackHeadMessage();
        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message,_next));
    }
}

void SbReconfBlockCode::step3(MessagePtr message) {
    SearchEndTrainMessage_ptr recvMessage = std::static_pointer_cast<SearchEndTrainMessage>(message);
#ifdef verbose
    stringstream info;
#endif // verbose

    applyRules();
/** attention,
    on test d'abord si c'est un isthme avant de faire tout déplacement
**/
    if (possibleRules && !possibleRules->empty()) {
        Capability *capa = possibleRules->back()->capa;
        bool test = testIsthmus(capa->linkPrevPos->x,capa->linkPrevPos->y);
        if (capa->isEnd && !capa->isHead && !_isSingle) {
            test = test || testIsthmusTail(capa->linkPrevPos->x,capa->linkPrevPos->y);
        }

        if (test) {
#ifdef verbose
            printRules();
            info.str("");
            info << "isthmus " << capa->name << " en " << posGrid << ": " << capa->isAngle << ";" << _previous->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            // il faut trouver un bloc de fin de train avant le bloc courant.
            TrainReadyMessage *message = new TrainReadyMessage(false);

            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
            info.str("");
            info << "send TrainReadyMessage(false) to " << _previous->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            _isTrain = false;
            _isEnd = false;
            _isBorder = false;
            return;
        }
    }

    if (possibleRules) {
/** Si il existe une règle de corps, on propage **/
// il faut que la règle courante soit ni tete ni queue
        while (!possibleRules->empty() && (possibleRules->back()->capa->isHead || possibleRules->back()->capa->isEnd)) {
            possibleRules->pop_back();
        }
        printRules();
//        if (possibleRules->empty()) OUTPUT << _pm << endl;
    }

    if (!_isSingle && possibleRules && !possibleRules->empty()) {
        Capability *capa = possibleRules->back()->capa;
        block->setDisplayedValue(block->blockId);
        // send searchEndTrainMessage
        PointCel np = *capa->linkNextPos;
        _next = block->getP2PNetworkInterfaceByRelPos(Cell3DPosition(np.x, np.y, 0)); //
        _motionDir = capa->tabMotions[0]->vect;
#ifdef verbose
        info.str("");
        info << "next = " << np << "," << (_next->connectedInterface?_next->connectedInterface->hostBlock->blockId:-1);
        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        SearchEndTrainMessage *message = new SearchEndTrainMessage(recvMessage->num+1);
        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
        info.str("");
        info << "send SearchEndTrainMessage to " << _next->connectedInterface->hostBlock->blockId;
        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        _isTrain = true;
        _isBorder = true;
        _isEnd = false;
        //}
    } else {
/** si echec de propagation, on répond si le bloc peut être une queue **/
        applyRules();
        if (possibleRules) {
            while (!possibleRules->empty() && !possibleRules->back()->capa->isEnd) {
                possibleRules->pop_back();
            }
            printRules();
        }
        if (possibleRules && !possibleRules->empty()) {
            // send trainReadyMessage
            TrainReadyMessage *message = new TrainReadyMessage(true);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
            info.str("");
            info << "send TrainReadyMessage(true) to " << _previous->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            _isTrain = true;
            _isEnd = true;
            _isBorder = true;
            _isSingle = false; /////////// TODO
        } else {
// il faut trouver un bloc de fin de train avant le bloc courant.
            TrainReadyMessage *message = new TrainReadyMessage(false);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
            info.str("");
            info << "send TrainReadyMessage(false) to " << _previous->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            _isTrain = false;
            _isEnd = false;
            _isBorder = false;
        }
    }

}

void SbReconfBlockCode::reconnect(bool hasRule) {
#ifdef verbose
    stringstream info;
    info.str("");
    info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
         << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
         << " _isHead = " << _isHead << " _isEnd = " << _isEnd;
    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
    // apply rules
    applyRules();

    if (!hasRule) {
        //if (_previous==_next) {
        // on recherche une règle de tete et queue (déplacement spéciaux)
        while (!possibleRules->empty() && (!possibleRules->back()->capa->isHead || !possibleRules->back()->capa->isEnd)) possibleRules->pop_back();
        //}

        if (!possibleRules->empty()) {
            printRules();
            if (possibleRules->back()->capa->isHead) {
                // cas particulier des singletons en mouvement
                if (possibleRules->back()->capa->isEnd) {
                    Capability *capa = possibleRules->back()->capa;
#ifdef verbose
                    info.str("");
                    info << "special motion :" << capa->name ;
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                    vector<Motion*>::const_iterator cm = capa->tabMotions.begin();
                    while (cm!=capa->tabMotions.end()) {
                        singleMotion(*cm,capa);
                        cm++;
                    }
                    return;
                } else {
                    // cas général on cherche la queue
                    _isHead = true;
                    //block->setDisplayedValue(0);

                }
            }
        }

    }

/********************************************************************************
 * on remonte le bord jusqu'à la tête
 ********************************************************************************/
    if (!_previous || _isHead) {
        setRulesColor();
        // on recherche une rêgle de tete
        bool headFound=false;
        if (possibleRules && !possibleRules->empty()) {
            // on recherche une règle de tete
            while (!possibleRules->empty() && !possibleRules->back()->capa->isHead) possibleRules->pop_back();

            if (!possibleRules->empty()) {
                printRules();
                _isHead = true;
                _previous = NULL;
                _isTrain = true;
//block->setDisplayedValue(0);

                _motionDir = possibleRules->back()->capa->tabMotions[0]->vect;
                // send searchEndTrainMessage
                SearchEndTrainMessage *message = new SearchEndTrainMessage(1);
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
                info.str("");
                info << "send SearchEndTrainMessage to " << _next->connectedInterface->hostBlock->blockId;
                scheduler->trace(info.str(),hostBlock->blockId);
#endif
                setRulesColor();
                headFound=true;
                //block->setDisplayedValue(1);
            }
        }
        if (!headFound) {
            if (_previous) {
                _isHead=false;
                SearchHeadMessage *message = new SearchHeadMessage();
                scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                info.str("");
                info << "send SearchHeadMessage to " << _previous->connectedInterface->hostBlock->blockId;
                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            } else {
                sendSearchBackHeadMessage(_next);
            }
        }
    } else {
        // envoie le message de reconstruction du train
        //_next = recvMessage->sourceInterface->connectedInterface;
        _previous = getBorderPreviousNeightbor(_next);
        _numPrev = ((_previous && !_isSingle) ?_previous->connectedInterface->hostBlock->blockId:-1);
#ifdef verbose
        info.str("");
        info << "_previous =" << (_previous?_previous->connectedInterface->hostBlock->blockId:-1)
             << "  _next =" << (_next?_next->connectedInterface->hostBlock->blockId:-1)
             << " _isHead = " << _isHead << " _isEnd = " << _isEnd;
        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        _isTrain = false;
        if (possibleRules && !possibleRules->empty()) {
            ReconnectTrainMessage *message = new ReconnectTrainMessage(true);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
            info.str("");
            info << "send ReconnectTrainMsg(true) to " << _previous->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId,YELLOW);
#endif // verbose
        } else {
            ReconnectTrainMessage *message = new ReconnectTrainMessage(false);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
            info.str("");
            info << "send ReconnectTrainMsg(false) to " << _next->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId,YELLOW);
#endif // verbose
        }
        setRulesColor();
    }

}

void SbReconfBlockCode::createLine(Time t,bool hol) {
    static PointCel tabDirections[4] = { PointCel(0,1),PointCel(1,0),PointCel(0,-1),PointCel(-1,0)};
#ifdef verbose
    stringstream info;
#endif // verbose
    Capability *capa = possibleRules->back()->capa;
    /*info.str("");
      info << "capa " << capa->name << " " << capa->isAngle;
      scheduler->trace(info.str(),hostBlock->blockId);*/
    _isHeadOfLine=hol;

    if (capa->isAngle || !_next || capa->isEnd) {
        PointCel motionVector;
        if (!_isHead && _previous) {
            motionVector = tabDirections[block->getDirection(_previous)];
        } else {
            motionVector = capa->tabMotions[0]->vect;
        }
        Time t0 = scheduler->now(),
            t1 = t0+(t0-t)*5.0;
        if (_previous) {
            SetRDVMessage *message = new SetRDVMessage(t1,motionVector);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
            info.str("");
            info << "send SetRDVMessage("<<motionVector<<") to " << _previous->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        }
        startMotion(t1,motionVector,0,capa->tabUnlockPath);
    } else {
        CreateLineMessage *message = new CreateLineMessage(t);
        scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
        info.str("");
        info << "send CreateLineMessage to " << _next->connectedInterface->hostBlock->blockId;
        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
    }
}

void SbReconfBlockCode::sendMapToNeighbors(P2PNetworkInterface *p2pExcept) {
    static const int dirx[4]={0,1,0,-1}, diry[4]={1,0,-1,0};
    P2PNetworkInterface *p2p;
#ifdef verbose
    stringstream info;
#endif // verbose

    nbreOfWaitedAnswers=0;
    for(int i = SLattice::North; i <= SLattice::West; i++) {
        p2p = smartBlock->getInterface( SLattice::Direction(i));
        if(p2p->connectedInterface && p2p!=p2pExcept) {
            MapMessage *message = new MapMessage(posGrid.x+dirx[i],posGrid.y+diry[i],gridSize[0],gridSize[1],_nbreGoalCells,targetGrid);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
            nbreOfWaitedAnswers++;
#ifdef verbose
            info.str("");
            info << "send MapMessage to " << p2p->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        }
    }
}

void SbReconfBlockCode::sendAsk4EndToNeighbors(P2PNetworkInterface *p2pExcept) {
//	static const int dirx[4]={0,1,0,-1}, diry[4]={1,0,-1,0};
    P2PNetworkInterface *p2p;
#ifdef verbose
    stringstream info;
#endif // verbose

    nbreOfWaitedAnswers=0;
    _nbreWellPlacedBlocks=int(wellPlaced);
    _currentMove++;
    for(int i = SLattice::North; i <= SLattice::West; i++) {
        p2p = smartBlock->getInterface( SLattice::Direction(i));
        if(p2p->connectedInterface && p2p!=p2pExcept) {
            Ask4EndMessage *message = new Ask4EndMessage(_currentMove);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
            nbreOfWaitedAnswers++;
#ifdef verbose
            info.str("");
            info << "send Ask4EndMessage to " << p2p->connectedInterface->hostBlock->blockId;
//				scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        }
    }
}

void SbReconfBlockCode::sendAns4EndMessage(P2PNetworkInterface *p2p,int value) {
    Ans4EndMessage *message = new Ans4EndMessage(value);
    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
#ifdef verbose
    stringstream info;
    info.str("");
    info << "send Ans4EndMessage(" << value << "," << nbreOfWaitedAnswers << ") to " << p2p->connectedInterface->hostBlock->blockId;
//	scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
}


P2PNetworkInterface *SbReconfBlockCode::getBorderPreviousNeightborNoWellPlaced(P2PNetworkInterface *next) {
    //static int border[8][2] = {{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1},{-1,0},{-1,1}};
    static int border[8][2] = {{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1},{1,0},{1,1}};
//    OUTPUT << "getBorderPreviousNeightbor()" << endl;
//    OUTPUT << _pm;

    SLattice::Direction dir;
    int i;
    // on recherche une cellule vide
    if (next) {
        // cherche la direction de next
        dir=SLattice::North;
        i=4;
        while (i-- && block->getInterface(dir)!=next) {
            dir=SLattice::Direction((int(dir)+1)%4);
        }
//        OUTPUT << "next :" << (i==-1?-1:dir) << endl;
        dir=SLattice::Direction((int(dir)+3)%4);
        // on cherche une cellule pleine
        i=3;
        while (i-- && (block->getInterface(dir)->connectedInterface==NULL
                       || ((SbReconfBlockCode*)(block->getInterface(dir)->connectedInterface->hostBlock->blockCode))->wellPlaced)) {
            dir=SLattice::Direction((int(dir)+3)%4);
//		OUTPUT << dir << " ";
        }

    } else {
/************ on recherche le cas des isthmes ************/
/** existe-t-il un voisin avec un vide avant et un après */
        for (i=0; i<4; i++) {
            if (_pm.get(border[i*2][0],border[i*2][1])!=emptyCell &&
                _pm.get(border[(i*2+7)%8][0],border[(i*2+7)%8][1])==emptyCell &&
                _pm.get(border[(i*2+1)%8][0],border[(i*2+1)%8][1])==emptyCell) {
                dir=SLattice::Direction(((8-2*i)%8)/2);
                return (block->getInterface(dir)->connectedInterface==NULL?NULL:block->getInterface(dir));
            }
        }

// on cherche une case vide
        i=7;
        while (i>=0 && _pm.get(border[i][0],border[i][1])!=emptyCell) {
            i--;
        }
        if (i==-1) return NULL;
        dir=SLattice::Direction(((8-i)%8)/2);
//OUTPUT << "une case vide = " << i << "(" << border[i][0] << "," << border[i][1]<< ")" << endl;
// puis on cherche la case non vide la plus eloignée en tournant vers la gauche
        if (wellPlaced) {
            i=3;
            while (i-- && (block->getInterface(dir)->connectedInterface==NULL
                           || ((SbReconfBlockCode*)(block->getInterface(dir)->connectedInterface->hostBlock->blockCode))->wellPlaced)) {
                dir=SLattice::Direction((int(dir)+3)%4);
            }
        } else {
            i=3;
            while (i-- && (block->getInterface(dir)->connectedInterface==NULL)) {
                dir=SLattice::Direction((int(dir)+3)%4);
            }
        }
    }
//OUTPUT << "previous :" << (block->getInterface(dir)->connectedInterface==NULL?-1:dir) << endl;
    return (block->getInterface(dir)->connectedInterface==NULL?NULL:block->getInterface(dir));
}

P2PNetworkInterface *SbReconfBlockCode::getBorderPreviousNeightbor(P2PNetworkInterface *next) {
    //static int border[8][2] = {{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1},{-1,0},{-1,1}};
    static int border[8][2] = {{0,1},{-1,1},{-1,0},{-1,-1},{0,-1},{1,-1},{1,0},{1,1}};
//    OUTPUT << "getBorderPreviousNeightbor()" << endl;
//    OUTPUT << _pm;

    SLattice::Direction dir;
    int i;
    // on recherche une cellule vide
    if (next) {
        // cherche la direction de next
        dir=SLattice::North;
        i=4;
        while (i-- && block->getInterface(dir)!=next) {
            dir=SLattice::Direction((int(dir)+1)%4);
        }
//        OUTPUT << "next :" << (i==-1?-1:dir) << endl;
        dir=SLattice::Direction((int(dir)+3)%4);
        // on cherche une cellule pleine
        i=3;
        while (i-- && block->getInterface(dir)->connectedInterface==NULL) {
            dir=SLattice::Direction((int(dir)+3)%4);
//		OUTPUT << dir << " ";
        }

    } else {
/************ on recherche le cas des isthmes ************/
/** existe-t-il un voisin avec un vide avant et un après */
        for (i=0; i<4; i++) {
            if (_pm.get(border[i*2][0],border[i*2][1])!=emptyCell &&
                _pm.get(border[(i*2+7)%8][0],border[(i*2+7)%8][1])==emptyCell &&
                _pm.get(border[(i*2+1)%8][0],border[(i*2+1)%8][1])==emptyCell) {
                dir=SLattice::Direction(((8-2*i)%8)/2);
                return (block->getInterface(dir)->connectedInterface==NULL?NULL:block->getInterface(dir));
            }
        }

// on cherche une case vide
        i=7;
        while (i>=0 && _pm.get(border[i][0],border[i][1])!=emptyCell) {
            i--;
        }
        if (i==-1) return NULL;
        dir=SLattice::Direction(((8-i)%8)/2);
//OUTPUT << "une case vide = " << i << "(" << border[i][0] << "," << border[i][1]<< ")" << endl;
// puis on cherche la case non vide la plus eloignée en tournant vers la gauche
        if (wellPlaced) {
            i=3;
            while (i-- && block->getInterface(dir)->connectedInterface==NULL) {
                dir=SLattice::Direction((int(dir)+3)%4);
            }
        } else {
            i=3;
            while (i-- && (block->getInterface(dir)->connectedInterface==NULL)) {
                dir=SLattice::Direction((int(dir)+3)%4);
            }
        }
    }
//OUTPUT << "previous :" << (block->getInterface(dir)->connectedInterface==NULL?-1:dir) << endl;
    return (block->getInterface(dir)->connectedInterface==NULL?NULL:block->getInterface(dir));
}

P2PNetworkInterface *SbReconfBlockCode::getBorderNextNeightbor(P2PNetworkInterface *prev) {
    static int border[8][2] = {{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1},{-1,0},{-1,1}};
//    OUTPUT << "getBorderNextNeightbor()" << endl;
//    OUTPUT << _pm;

    int i=0;
    SLattice::Direction dir;
    if (prev) {
        // cherche la direction de prev
        dir=SLattice::North;
        i=4;
        while (i-- && block->getInterface(dir)!=prev) {
            dir=SLattice::Direction((int(dir)+1)%4);
        }
        dir=SLattice::Direction((dir+1)%4);
    } else {
        // on recherche une cellule vide
        while (i<8 && _pm.get(border[i][0],border[i][1])!=emptyCell) {
            i++;
        }
        dir=SLattice::Direction((i/2+1)%4);
    }

    i=3;
    while (i-- && block->getInterface(dir)->connectedInterface==NULL) {
        dir=SLattice::Direction((int(dir)+1)%4);
//		OUTPUT << dir << " ";
    }
//	OUTPUT << dir << endl;
    return (block->getInterface(dir)->connectedInterface==NULL?NULL:block->getInterface(dir));
}

P2PNetworkInterface *SbReconfBlockCode::getBorderNextNeightborNoWellPlaced(P2PNetworkInterface *prev) {
    static int border[8][2] = {{0,1},{1,1},{1,0},{1,-1},{0,-1},{-1,-1},{-1,0},{-1,1}};
//    OUTPUT << "getBorderNextNeightbor()" << endl;
//    OUTPUT << _pm;

    int i=0;
    SLattice::Direction dir;
    if (prev) {
        // cherche la direction de prev
        dir=SLattice::North;
        i=4;
        while (i-- && block->getInterface(dir)!=prev) {
            dir=SLattice::Direction((int(dir)+1)%4);
        }
        dir=SLattice::Direction((dir+1)%4);
    } else {
        // on recherche une cellule vide
        while (i<8 && _pm.get(border[i][0],border[i][1])!=emptyCell) {
            i++;
        }
        dir=SLattice::Direction((i/2+1)%4);
    }
//    OUTPUT << i << "," << dir << endl;

    i=3;
    while (i-- && (block->getInterface(dir)->connectedInterface==NULL
                   || ((SbReconfBlockCode*)(block->getInterface(dir)->connectedInterface->hostBlock->blockCode))->wellPlaced)) {
//            || _pm.get(border[dir*2][0],border[dir*2][1])==borderNotTrainCell)) {
        dir=SLattice::Direction((int(dir)+1)%4);
//		OUTPUT << dir << "(" << _pm.get(border[dir*2][0],border[dir*2][1]) << ") ";
    }
//	OUTPUT << dir << endl;
    return (block->getInterface(dir)->connectedInterface==NULL?NULL:block->getInterface(dir));
}

P2PNetworkInterface *SbReconfBlockCode::getBorderSinglePrevious() {
    SLattice::Direction dir;// ,dirStop;

//OUTPUT << "motionDir:" <<  _motionDir << endl;
    if (_motionDir.x==0) {
        if (_motionDir.y==1) {
            dir = SLattice::West;
            // dirStop = SLattice::North;
        } else {
            dir = SLattice::East;
            // dirStop = SLattice::South;
        }
    } else {
        if (_motionDir.x==1) {
            dir = SLattice::North;
            // dirStop = SLattice::East;
        } else {
            dir = SLattice::South;
            // dirStop = SLattice::West;
        }
    }
//    P2PNetworkInterface *stop = block->getInterface(dirStop);
    //return (stop && stop->connectedInterface && !((SmartBlocks::SmartBlocksBlock*)(stop->connectedInterface->hostBlock))->wellPlaced)?stop:block->getInterface(dir);

    // a partir de cette position on cherche la derniere cellule pleine
    int i=3;
    while (i-- && block->getInterface(SLattice::Direction((int(dir)+1)%4))->connectedInterface!=NULL) {
        dir=SLattice::Direction((int(dir)+1)%4);
//		OUTPUT << dir << " ";
    }

    return block->getInterface(dir);
}


P2PNetworkInterface *SbReconfBlockCode::getBorderNeighborById(int id) {
    P2PNetworkInterface *p2p;
    int i=4;
    bool found=false;
    while (i-- && !found) {
        p2p=block->getInterface(SLattice::SLattice::Direction(i));
        if (p2p->connectedInterface) {
//            OUTPUT << i << " : " << p2p->connectedInterface->hostBlock->blockId << endl;
            found=p2p->connectedInterface->hostBlock->blockId==(bID)id;
        }
    }
    return found?p2p:NULL;
}


/*
  void SbReconfBlockCode::sendNoActivity(SLattice::Direction dir, int id) {
  stringstream info;

// on recherche le premier voisin dans le sens horaire
dir=SLattice::Direction((int(dir)+1)%4);
int i=3;
while (i-- && block->getInterface(dir)->connectedInterface==NULL) {
dir=SLattice::Direction((int(dir)+1)%4);
}
P2PNetworkInterface *p2p = block->getInterface(dir);

NoActivityMessage *message = new NoActivityMessage(id);
scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
info.str("");
info << "send NoActivityMessage("<< id <<") to " << p2p->connectedInterface->hostBlock->blockId;
scheduler->trace(info.str(),hostBlock->blockId);
}
*/

void SbReconfBlockCode::sendAckMap(P2PNetworkInterface *p2p) {
    AckMapMessage *message = new AckMapMessage ();
    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
#ifdef verbose
    stringstream info;
    info.str("");
    info << "send AckMapMessage(" << nbreOfWaitedAnswers << ") to " << p2p->connectedInterface->hostBlock->blockId;
    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
}

void SbReconfBlockCode::sendSearchBackHeadMessage(P2PNetworkInterface *dest,P2PNetworkInterface *except) {
#ifdef verbose
    stringstream info;
    info.str("");
    info << "send SearchBackHeadMessage(" << (except==NULL?-1:except->hostBlock->blockId) << ") to " << dest->connectedInterface->hostBlock->blockId;
    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
    //setRulesColor();
    SearchBackHeadMessage *message = new SearchBackHeadMessage(except);
    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message,dest));

    _isTrain = false;
    _isHead = false;
    _isBorder = false;
    //block->setDisplayedValue(-1);

}

bool SbReconfBlockCode::testIsthmus(int dx,int dy) {
    Cell3DPosition gridSize = lattice->gridSize;

/*
  il faut aussi interdir <-XB0 ou B est un bord
*/
    SmartBlocks::SmartBlocksBlock *support;
    SmartBlocks::SmartBlocksBlock *supportDiag;
    SmartBlocks::SmartBlocksBlock *voisin;
    SmartBlocks::SmartBlocksBlock *voisin2=NULL;
    SmartBlocks::SmartBlocksBlock *voisin3=NULL;
    // recherche la présence d'isthme de niveau 1
    if (dy==1) {
        support = (posGrid.x>0) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y,0)) : NULL;
        supportDiag = (posGrid.x>0 && posGrid.y>0) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y-1,0)) : NULL;
        voisin = (posGrid.y>0 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y-1,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y-1,0)) : NULL;
        voisin2 = (posGrid.x>=1 && posGrid.y>1 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y-1,0))
                   && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-2,posGrid.y-2,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y-1,0)) : NULL;
        voisin3 = (posGrid.x<gridSize[0]-1 && posGrid.y>1 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y-1,0))
                   && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+2,posGrid.y-2,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y-1,0)) : NULL;
    } else if (dy==-1) {
        support = (posGrid.x<gridSize[0]-1) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y,0)) : NULL;
        supportDiag = (posGrid.x<gridSize[0]-1 && posGrid.y<gridSize[1]-1) ?
                                 (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y+1,0)) : NULL;
        voisin = (posGrid.y<gridSize[1]-1 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y+1,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y+1,0)) : NULL;
        voisin2 = (posGrid.x>=1 && posGrid.y<gridSize[1]-2 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y+1,0))
                   && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-2,posGrid.y+2,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y+1,0)) : NULL;
        voisin3 = (posGrid.x<gridSize[0]-1 && posGrid.y<gridSize[1]-2
                   && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y+1,0)) && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+2,posGrid.y+2,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y+1,0)) : NULL;
    } else if (dx==1) {
        support = (posGrid.y<gridSize[1]-1) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y+1,0)) : NULL;
        supportDiag = (posGrid.y<gridSize[1]-1 && posGrid.x>=1) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y+1,0)) : NULL;
        voisin = (posGrid.x>=1 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y,0)) : NULL;
        voisin2 = (posGrid.x>=1 && posGrid.y<gridSize[1]-2 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y+1,0))
                   && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-2,posGrid.y+2,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y+1,0)) : NULL;
        voisin3 = (posGrid.x<1 && posGrid.y>=1 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y-1,0))
                   && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-2,posGrid.y-2,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y-1,0)) : NULL;
    } else {
        // dx==-1
        support = (posGrid.y>0) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y-1,0)) : NULL;
        supportDiag = (posGrid.y>0 && posGrid.x<gridSize[0]-1) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y-1,0)) : NULL;
        voisin = (posGrid.x<gridSize[0]-1 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y,0)) : NULL;
        voisin2 = (posGrid.x<gridSize[0]-2 && posGrid.y<gridSize[1]-2
                   && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y+1,0)) && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+2,posGrid.y+2,0)))?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y+1,0)) : NULL;
        voisin3 = (posGrid.x<gridSize[0]-2 && posGrid.y>=1 && (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y-1,0))
                   && !(SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+2,posGrid.y-2,0))) ?
            (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y-1,0)) : NULL;
    }
#ifdef verbose
    if (support && ((SbReconfBlockCode*)support->blockCode)->_isTrain) OUTPUT << block->blockId << posGrid << " ISTHME SUPPORT("
                                             << support->blockId <<")=TRAIN dx,dy=" << dx << ","
                                             << dy << endl;
    if (supportDiag && ((SbReconfBlockCode*)supportDiag->blockCode)->_isTrain) OUTPUT << "ISTHME SUPPORT_DIAG("<< supportDiag->blockId
                                                     <<")=TRAIN dx,dy=" << dx << "," << dy << endl;
    if (voisin && ((SbReconfBlockCode*)voisin->blockCode)->_isTrain) OUTPUT << "ISTHME VOISIN("<< voisin->blockId <<")=TRAIN dx,dy=" << dx
                                           << "," << dy << endl;
    if (voisin2 && ((SbReconfBlockCode*)voisin2->blockCode)->_isTrain) OUTPUT << "ISTHME VOISIN2("<< voisin2->blockId <<")=TRAIN dx,dy="
                                             << dx << "," << dy << endl;
    if (voisin3 && ((SbReconfBlockCode*)voisin3->blockCode)->_isTrain) OUTPUT << "ISTHME VOISIN3("<< voisin3->blockId <<")=TRAIN dx,dy="
                                             << dx << "," << dy << endl;
#endif
    return (support && ((SbReconfBlockCode*)support->blockCode)->_isTrain) ||
        (supportDiag && ((SbReconfBlockCode*)supportDiag->blockCode)->_isTrain) ||
        (voisin && ((SbReconfBlockCode*)voisin->blockCode)->_isTrain) ||
        (voisin2 && ((SbReconfBlockCode*)voisin2->blockCode)->_isTrain) ||
        (voisin3 && ((SbReconfBlockCode*)voisin3->blockCode)->_isTrain);
}

bool SbReconfBlockCode::testIsthmusTail(int dx,int dy) {
    Cell3DPosition gridSize = lattice->gridSize;
/*
  il faut aussi interdir <-XB0
*/
    SmartBlocks::SmartBlocksBlock *support_1,*support_2;
    bool isthmus=false;
    // recherche la présence d'isthme de niveau 1
    if (dy==1) {
        support_1 = (posGrid.x>=1) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-1,posGrid.y,0)) : NULL;
        support_2 = (posGrid.x>=2) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x-2,posGrid.y,0)) : NULL;
    } else if (dy==-1) {
        support_1 = (posGrid.x<gridSize[0]-1) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+1,posGrid.y,0)) : NULL;
        support_2 = (posGrid.x<gridSize[0]-2) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x+2,posGrid.y,0)) : NULL;
    } else if (dx==1) {
        support_1 = (posGrid.y<gridSize[1]-1) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y+1,0)) : NULL;
        support_2 = (posGrid.y<gridSize[1]-2) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y+2,0)) : NULL;
    } else {
        // dx==-1
        support_1 = (posGrid.y>=1) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y-1,0)) : NULL;
        support_2 = (posGrid.y>=2) ? (SmartBlocks::SmartBlocksBlock *)lattice->getBlock(Cell3DPosition(posGrid.x,posGrid.y-2,0)) : NULL;
    }
    isthmus = support_1!=NULL && support_2==NULL;
#ifdef verbose
    if (isthmus) OUTPUT << block->blockId << posGrid << " ISTHME QUEUE dx,dy=" << dx << "," << dy << endl;
#endif
    return (isthmus);
}


void SbReconfBlockCode::createBorder() {
    getPresenceMatrix(posGrid,_pm);
    _isBorder = _pm.isBorder();

    //block->setDisplayedValue(-1);

#ifdef verbose
    stringstream info;
    info.str("");
    info << "_isBorder = " << _isBorder;
    scheduler->trace(info.str(),hostBlock->blockId,GREEN);
#endif // verbose
    if (_isBorder) {
        setRulesColor();
        _previous = getBorderPreviousNeightbor(NULL);
        _numPrev = ((_previous && !_isSingle)  ? _previous->connectedInterface->hostBlock->blockId : -1);
        applyRules();
        if (possibleRules && !possibleRules->empty()) {
            Capability *capa = possibleRules->back()->capa;
            if (!capa->isHead && capa->linkPrevPos) {
                PointCel pos = *capa->linkPrevPos;
                _previous = block->getP2PNetworkInterfaceByRelPos(Cell3DPosition(pos.x, pos.y, 0));
                _numPrev = ((_previous && !_isSingle)  ? _previous->connectedInterface->hostBlock->blockId : -1);
#ifdef verbose
                info.str("");
                info << "previous = " << *capa->linkPrevPos << "," << ((_previous->connectedInterface) ? _previous->connectedInterface->hostBlock->blockId : -1);
                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            } else _previous=NULL;
            if (!capa->isEnd && capa->linkNextPos) {
                PointCel np = *capa->linkNextPos;
                _next = block->getP2PNetworkInterfaceByRelPos(Cell3DPosition(np.x, np.y, 0));
#ifdef verbose
                info.str("");
                info << "next = " << np << "," << (_next->connectedInterface ? _next->connectedInterface->hostBlock->blockId : -1);
                scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            } else _next=NULL;

            // recherche une règle pour lequel il est distance 0
            if (possibleRules->back()->isZeroDistance) {
                if (capa->isHead) {
                    // send searchEndTrainMessage
#ifdef verbose
                    info.str("");
                    info << "  _next =" << (_next ? _next->connectedInterface->hostBlock->blockId : -1);
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                    _previous = NULL;
                    SearchEndTrainMessage *message = new SearchEndTrainMessage(1);
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _next));
#ifdef verbose
                    info.str("");
                    info << "send SearchEndTrainMessage to " << _next->connectedInterface->hostBlock->blockId;
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif
                    setRulesColor();
                    //block->setDisplayedValue(0);
                    _isHead = true;
                } else {
                    SearchHeadMessage *message = new SearchHeadMessage();
                    scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, _previous));
#ifdef verbose
                    info.str("");
                    info << "send SearchHeadMessage to " << _previous->connectedInterface->hostBlock->blockId;
                    scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
                }
            }
            //block->setDisplayedValue(possibleRules->back()->gain);

            // Gère les messages en attente
            if (tabMemorisedMessages[2]) {
                step2(tabMemorisedMessages[2]);
                tabSteps[2]=false;
            }
            if (tabMemorisedMessages[3]) {
                step3(tabMemorisedMessages[3]);
                tabSteps[3]=false;
            }
        } else {
            _previous = getBorderPreviousNeightbor(NULL);
            _numPrev = ((_previous && !_isSingle)  ? _previous->connectedInterface->hostBlock->blockId : -1);
            _next = getBorderNextNeightbor();
#ifdef verbose
            info.str("");
            info << "no rule, previous = " << ((_previous->connectedInterface) ? _previous->connectedInterface->hostBlock->blockId : -1) <<
                " next = " << ((_next->connectedInterface) ? _next->connectedInterface->hostBlock->blockId : -1);
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        }
    }
}

/*
  void SbReconfBlockCode::sendInitToNeighbors(P2PNetworkInterface *p2pExcept,int stage) {
  P2PNetworkInterface *p2p;
  stringstream info;

  nbreOfWaitedAnswers=0;
  for(int i = SLattice::North; i <= SLattice::West; i++) {
  p2p = smartBlock->getInterface( SLattice::Direction(i));
  if(p2p->connectedInterface && p2p!=p2pExcept) {
  ReInitMessage *message = new ReInitMessage(stage);
  scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
  nbreOfWaitedAnswers++;
  info.str("");
  info << "send ReInitMessage to " << p2p->connectedInterface->hostBlock->blockId;
  scheduler->trace(info.str(),hostBlock->blockId);
  }
  }
  }

  void SbReconfBlockCode::sendAckInit(P2PNetworkInterface *p2p) {
  AckInitMessage *message = new AckInitMessage();
  scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
  stringstream info;
  info.str("");
  info << "send AckInitMessage(" << nbreOfWaitedAnswers << ") to " << p2p->connectedInterface->hostBlock->blockId;
  scheduler->trace(info.str(),hostBlock->blockId);
  }
*/

MapMessage::MapMessage(int x,int y,int w,int h,int n,presence *tg):Message(){
    id = MAP_MSG_ID;
    posx = x;
    posy = y;
    gridw = w;
    gridh = h;
    nbreGoalCells = n;
    targetGrid = tg;
}

MapMessage::~MapMessage() {
}

AckMapMessage::AckMapMessage():Message(){
    id = ACKMAP_MSG_ID;
}

AckMapMessage::~AckMapMessage() {
}

SearchHeadMessage::SearchHeadMessage():Message(){
    id = HEAD_MSG_ID;
}

SearchHeadMessage::~SearchHeadMessage() {
}

SearchBackHeadMessage::SearchBackHeadMessage(P2PNetworkInterface *except):Message(){
    exceptionBlock = except;
    id = HBCK_MSG_ID;
}

SearchBackHeadMessage::~SearchBackHeadMessage() {
}

SearchEndTrainMessage::SearchEndTrainMessage(int n):Message(){
    id = END_MSG_ID;
    num = n;
}

SearchEndTrainMessage::~SearchEndTrainMessage() {
}

TrainReadyMessage::TrainReadyMessage(bool qf):Message(){
    id = TRAIN_READY_MSG_ID;
    queueFound = qf;
}

TrainReadyMessage::~TrainReadyMessage() {
}

CreateLineMessage::CreateLineMessage(Time t):Message(){
    id = CREATE_LINE_MSG_ID;
    etime = t;
}

CreateLineMessage::~CreateLineMessage() {
}

SetRDVMessage::SetRDVMessage(Time t,const PointCel &v):Message(){
    id = SET_RDV_MSG_ID;
    rdvTime = t;
    motionVector = v;
}

SetRDVMessage::~SetRDVMessage() {
}

UnlockMessage::UnlockMessage(short *t,int n,int s):Message(){
    id = UNLOCK_MSG_ID;
    if (n>0) {
        tab = new short[n];
        memcpy(tab,t,n*sizeof(short));
        sz = n;
    } else {
        tab=NULL;
        sz=0;
    }
    step = s;
}

UnlockMessage::~UnlockMessage() {
    delete [] tab;
}

ReconnectTrainMessage::ReconnectTrainMessage(bool hr):Message(){
    id = RECONNECT_MSG_ID;
    hasRule=hr;
}

ReconnectTrainMessage::~ReconnectTrainMessage() {
}

/*
  DisableTrainMessage::DisableTrainMessage():Message(){
  id = DISABLE_MSG_ID;
  }

  DisableTrainMessage::~DisableTrainMessage() {
  }

  NoActivityMessage::NoActivityMessage(int sid):Message() {
  id = NOACTIVITY_MSG_ID;
  senderID=sid;
  }

  NoActivityMessage::~NoActivityMessage() {
  }

  ReInitMessage::ReInitMessage(int s):Message(){
  id = REINIT_MSG_ID;
  stage=s;
  }

  ReInitMessage::~ReInitMessage() {
  }

  AckInitMessage::AckInitMessage():Message(){
  id = ACKINIT_MSG_ID;
  }

  AckInitMessage::~AckInitMessage() {
  }*/

SingleMoveMessage::SingleMoveMessage(short *t,int n,Time st,const PointCel &mv,const vector<short>&up,int s):Message() {
    id = SINGLEMV_MSG_ID;
    if (n>0) {
        tab = new short[n];
        memcpy(tab,t,n*sizeof(short));
        sz = n;
    } else {
        tab=NULL;
        sz=0;
    }
    startTime = st;
    motionVector = mv;
    step = s;
    // copy up dans unlockPath
    unlockPath = up;
}

SingleMoveMessage::~SingleMoveMessage() {
    delete [] tab;
}

Ask4EndMessage::Ask4EndMessage(int cm):Message(){
    id = ASK4END_MSG_ID;
    currentMove = cm;
}

Ask4EndMessage::~Ask4EndMessage() {
}

Ans4EndMessage::Ans4EndMessage(int n):Message() {
    nbreWellPlaced = n;
    id = ANS4END_MSG_ID;
}

Ans4EndMessage::~Ans4EndMessage() {
}

/****************************************************/
void SbReconfBlockCode::getLocalTargetGrid(const PointCel &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    presence *tg = targetGrid;

    for (int i=0; i<9; i++) { *gpm++ = wallCell; };

    int ix0 = (pos.x<1) ? 1-pos.x : 0,
        ix1 = (pos.x>gridSize[0]-2) ? gridSize[0]-pos.x+1 : 3,
        iy0 = (pos.y<1) ? 1-pos.y : 0,
        iy1 = (pos.y>gridSize[1]-2) ? gridSize[1]-pos.y+1 : 3,
        ix,iy;

    for (iy=iy0; iy<iy1; iy++) {
        gpm = pm.grid+(iy*3+ix0);
        tg = targetGrid+(ix0+pos.x-1+(iy+pos.y-1)*gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            *gpm++ = *tg++;
        }
    }
}

void SbReconfBlockCode::singleMotion(Motion *currentMotion,Capability *capa) {
    Time t = scheduler->now(),
        st = t+20*time_offset;
#ifdef verbose
    stringstream info;
#endif // verbose

//------------------------------------
    if (capa->isEnd && capa->isHead) {
        _isSingle=true;
        _numPrev=-1;
#ifdef verbose
        OUTPUT << block->blockId << " is single" << endl;
#endif // verbose
    }
    if (currentMotion->PathToBlock.empty()) {
#ifdef verbose
        info.str("");
        info << "single motion " << currentMotion->vect;
        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        startMotion(t+2*time_offset,currentMotion->vect,0,capa->tabUnlockPath);
    } else {
#ifdef verbose
        info.str("");
        info << "multiple motion :" << capa->name ;
        scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
        vector<short>::const_iterator cs=currentMotion->PathToBlock.begin();
        int n = currentMotion->PathToBlock.size();
        if (n) {
#ifdef verbose
            info.str("");
#endif // verbose
            short *tabDir = new short[n];
            int i=0;
            while (cs!=currentMotion->PathToBlock.end()) {
                tabDir[i++] = *cs;
#ifdef verbose
                info << *cs << " ";
#endif // verbose
                cs++;
            }
#ifdef verbose
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            // envoie le message de déplacement
            P2PNetworkInterface *p2p = block->getInterface(SLattice::Direction(tabDir[0]));
            SingleMoveMessage *message = new SingleMoveMessage(tabDir+1,n-1,st,currentMotion->vect,currentMotion->UnlockPath,1);
            scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + time_offset, message, p2p));
#ifdef verbose
            info.str("");
            info << "send SingleMoveMessage("<<n-1<<") to " << p2p->connectedInterface->hostBlock->blockId;
            scheduler->trace(info.str(),hostBlock->blockId);
#endif // verbose
            delete [] tabDir;
        }
    }
//------------------------------------
}

void SbReconfBlockCode::parseUserElements(TiXmlDocument *config) {
    TiXmlNode *xmlWorldNode = config->FirstChild("world");

    TiXmlNode *nodeGrid = xmlWorldNode->FirstChild("targetGrid");
    vector<Cell3DPosition> targetCells; // Locations of all target full cells

    if (nodeGrid) {
        TiXmlNode *block = nodeGrid->FirstChild("block");
        Cell3DPosition position;
        const char *attr;
        TiXmlElement* element;
        while (block) {
            element = block->ToElement();
            attr = element->Attribute("position");
            if (attr) {
                string str(attr);
                int pos = str.find_first_of(',');
                int pos2 = str.find_last_of(',');
                int ix = atof(str.substr(0,pos).c_str()),
                    iy = atoi(str.substr(pos+1,pos2-pos-1).c_str()),
                    iz = atoi(str.substr(pos2+1,str.length()-pos2-1).c_str());

                position.pt[0] = ix;
                position.pt[1] = iy;
                position.pt[2] = iz;

                targetCells.push_back(Cell3DPosition(position[0], position[1], position[2]));
            }

            block = block->NextSibling("block");
        }

        block = nodeGrid->FirstChild("targetLine");
        int line = 0, plane = 0;
        while (block) {
            TiXmlElement* element = block->ToElement();
            const char *attr = element->Attribute("line");
            if (attr) {
                line = atoi(attr);
            }
            attr = element->Attribute("plane");
            if (attr) {
                plane = atoi(attr);
            }
            attr = element->Attribute("values");
            if (attr) {
                string str(attr);
                int n = str.length();
                for(int i = 0; i < n; i++) {
                    if(str[i] == '1') {
                        targetCells.push_back(Cell3DPosition(i, line, plane));
                    }
                }
            }

            block = block->NextSibling("targetLine");
        }
    } else {
        ERRPUT << "warning: No target grid in configuration file" << endl;
    }

    // Add target cells to world
    initTargetGrid();
    for (Cell3DPosition p : targetCells) {
        setTargetGrid(fullCell, p[0], p[1]);
    }

    // then parse and load capabilities...
    TiXmlNode *nodeCapa = xmlWorldNode->FirstChild("capabilities");
    if (nodeCapa) {
        setCapabilities(new SmartBlocksCapabilities(nodeCapa));
    }
}


void SbReconfBlockCode::initTargetGrid() {
    if (targetGrid) delete [] targetGrid;
    int sz = lattice->gridSize[0]*lattice->gridSize[1];
    targetGrid = new presence[sz];
    memset(targetGrid,emptyCell,sz*sizeof(presence));
}

int SbReconfBlockCode::nbreWellPlacedBlock() {
    std::map<bID, BuildingBlock*> buildingBlocksMap = SmartBlocks::getWorld()->getMap();
    std::map<bID, BuildingBlock*>::iterator it;
    int n=0;
    SmartBlocks::SmartBlocksBlock *sb;
    for( it = buildingBlocksMap.begin() ; it != buildingBlocksMap.end() ; ++it) {
        sb = (SmartBlocks::SmartBlocksBlock *)(it->second);
        if (((SbReconfBlockCode*)sb->blockCode)->wellPlaced) n++;
    }
    return n;
}


void SbReconfBlockCode::getPresenceMatrix0(const PointCel &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    SmartBlocks::SmartBlocksBlock **grb;

    for (int i=0; i<9; i++) { *gpm++ = wallCell; };

    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>lattice->gridSize[0]-2)?lattice->gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>lattice->gridSize[1]-2)?lattice->gridSize[1]-pos.y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        gpm = pm.grid+(iy*3+ix0);
        grb = (SmartBlocks::SmartBlocksBlock **)lattice->grid+(ix0+pos.x-1+(iy+pos.y-1)*lattice->gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            *gpm++ = (*grb)?fullCell:emptyCell;
            grb++;
        }
    }
}

bool SbReconfBlockCode::isBorder(int x,int y) {
    SmartBlocks::SmartBlocksBlock **grb=(SmartBlocks::SmartBlocksBlock **)lattice->grid+x+y*lattice->gridSize[0];
    //if ((*grb)->_isBorder) return true;
    int ix0 = (x<1)?1-x:0,
        ix1 = (x>lattice->gridSize[0]-2)?lattice->gridSize[0]-x+1:3,
        iy0 = (y<1)?1-y:0,
        iy1 = (y>lattice->gridSize[1]-2)?lattice->gridSize[1]-y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        grb = (SmartBlocks::SmartBlocksBlock **)lattice->grid+(ix0+x-1+(iy+y-1)*lattice->gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            //if (*grb==NULL || (*grb)->wellPlaced) return true;
            if (*grb==NULL) return true;
            grb++;
        }
    }
    return false;
}

bool SbReconfBlockCode::isSingle(int x,int y) {
    SmartBlocks::SmartBlocksBlock **grb=(SmartBlocks::SmartBlocksBlock **)lattice->grid+x+y*lattice->gridSize[0];
    return ((SbReconfBlockCode*)(*grb)->blockCode)->_isSingle;
}

void SbReconfBlockCode::getPresenceMatrix(const PointCel &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    SmartBlocks::SmartBlocksBlock **grb;

    for (int i=0; i<9; i++) { *gpm++ = wallCell; };
    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>lattice->gridSize[0]-2)?lattice->gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>lattice->gridSize[1]-2)?lattice->gridSize[1]-pos.y+1:3,
        ix,iy;
    for (iy=iy0; iy<iy1; iy++) {
        gpm = pm.grid+(iy*3+ix0);
        grb = (SmartBlocks::SmartBlocksBlock **)lattice->grid+(ix0+pos.x-1+(iy+pos.y-1)*lattice->gridSize[0]);
        for (ix=ix0; ix<ix1; ix++) {
            *gpm++ = (*grb)?
                ((isBorder(ix+pos.x-1,iy+pos.y-1))?
                 (isSingle(ix+pos.x-1,iy+pos.y-1)?
                  singleCell
                  :borderCell)
                 :fullCell)
                :emptyCell;
            grb++;
        }
    }
}


void SbReconfBlockCode::addStat(int n,int v) {
    tabStatsData[n]+=v;
    if (nbreStats<=n) nbreStats=n+1;
}

void SbReconfBlockCode::printStats() {
    OUTPUT << "stats: \t" << nbreWellPlacedBlock();
    for (int i=0;i<nbreStats; i++) {
        OUTPUT << "\t"<< tabStatsData[i] ;
    }
    OUTPUT << "\t" << getScheduler()->getNbreMessages() << endl;
}
