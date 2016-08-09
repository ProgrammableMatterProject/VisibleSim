/*
 * Robot01BlockCode.cpp
 *
 *  Created on: 12 avril 2013
 *      Author: ben
 */

#include <iostream>
#include <sstream>

#include "robot01BlockCode.h"
#include "scheduler.h"
#include "translationEvents.h"
#include "capabilities.h"

using namespace std;
using namespace RobotBlocks;

const int COM_DELAY=1000;

presence *initGrid(short gridSize[3],presence *init) {
	int size = gridSize[0]*gridSize[1]*gridSize[2];
	presence *targetGrid2 = new presence[size];
	memcpy(targetGrid2,init,size);
	return targetGrid2;
}

Robot01BlockCode::Robot01BlockCode(RobotBlocksBlock *host):RobotBlocksBlockCode(host) {
	// cout << "Robot01BlockCode constructor" << endl;
	scheduler = getScheduler();
	robotBlock = (RobotBlocksBlock*)hostBlock;

// initialize object deleted in destructor
	targetGrid=NULL;
	possibleMotions=NULL;
	lattice = BaseSimulator::getWorld()->lattice;		
}

Robot01BlockCode::~Robot01BlockCode() {
	// cout << "Robot01BlockCode destructor" << endl;
	if (targetGrid) {
		delete [] targetGrid;
		targetGrid = NULL;
	}
	
	if (capabilities){
		delete capabilities;
	    capabilities = NULL;
	}
	
	delete possibleMotions;
}

void Robot01BlockCode::startup() {
	stringstream info;
	
	info << "start #" << robotBlock->blockId;
	nbreOfWaitedAnswers=0;
	block2Answer = NULL;
	trainNext = NULL;
	trainPrevious = NULL;
	possibleMotions=NULL;
	currentTrainGain=0;
	//If i am master block
	if(robotBlock->isMaster)
	{
	    presence *tg = getTargetGridPtr(gridSize);
		info << " (Master Block at " << robotBlock->position[0] << "," << robotBlock->position[1] << "," << robotBlock->position[2] << ")";
		scheduler->trace(info.str(),robotBlock->blockId,YELLOW);
		targetGrid = initGrid(gridSize,tg);
		nbreOfWaitedAnswers=0;

		sendMapToNeighbors(NULL);
	} else {
		targetGrid=NULL;
		scheduler->trace(info.str(),robotBlock->blockId,BLUE);
	}
}

void Robot01BlockCode::processLocalEvent(EventPtr pev) {
	stringstream info;
	MessagePtr message;

	switch (pev->eventType) {
    case EVENT_TRANSLATION_END:
		robotBlock->setColor(LIGHTBLUE);
		info.str("");
		info << robotBlock->blockId << " rec.: EVENT_TRANSLATION_END";
		scheduler->trace(info.str(),hostBlock->blockId);
		// prepare for next motion
		nbreOfWaitedAnswers=0;
		block2Answer=NULL;
		if (blockToUnlock!=0) {
			sendUnlockMessage(blockToUnlock);
		} else { // dernier élément du train
			if (trainNext==NULL) {
				info.str("");
				info << "rerun " ;//<< trainNextId << "," << trainPreviousId;
				scheduler->trace(info.str(),hostBlock->blockId);
				robotBlock->setColor(DARKORANGE);
				trainPrevious=NULL;
				PointRel3D pt;
				calcPossibleMotions(pt);
				sendReLinkTrainMessage();
			} else {
				info.str("");
				info << "ready " ;//<< trainNextId << "," << trainPreviousId;
				scheduler->trace(info.str(),hostBlock->blockId);
				robotBlock->setPrevNext(trainPrevious,trainNext);
				robotBlock->setColor(BLUE);
			}
		}
		break;

	case EVENT_NI_RECEIVE:
		message = (std::static_pointer_cast<NetworkInterfaceReceiveEvent>(pev))->message;
		P2PNetworkInterface * recvInterface = message->destinationInterface;
		switch(message->id) {
		case MAP_MSG_ID : {
			MapMessage_ptr recvMessage = std::static_pointer_cast<MapMessage>(message);

			unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;

			// cout << "INTERFACE" << robotBlock->getDirection(message->destinationInterface) << endl;
					
			info.str("");
			info << "rec. MapMessage : MAP_MSG from " << sourceId;
			scheduler->trace(info.str(),hostBlock->blockId);

			if (targetGrid) {
				sendAckMap(recvInterface);
			} else {
				// first message
				memcpy(gridSize,recvMessage->gridSize,3*sizeof(short));
				targetGrid = initGrid(gridSize,recvMessage->targetGrid);

				block2Answer=recvInterface;
				nbreOfWaitedAnswers=0;
				sendMapToNeighbors(block2Answer);

				if (nbreOfWaitedAnswers==0) {
					sendAckMap(block2Answer);
					block2Answer=NULL;
					info.str("");
					info << " the end";
					scheduler->trace(info.str(),hostBlock->blockId,GOLD);
					currentTrainGain=0;

					PointRel3D pt;
					calcPossibleMotions(pt);
				}
			}
		}
			break;

		case ACKMAP_MSG_ID : {
			AckMapMessage_ptr recvMessage = std::static_pointer_cast<AckMapMessage>(message);
			unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
			info.str("");
			info << "rec. AckMapMessage(" << nbreOfWaitedAnswers << ") from " << sourceId;
			scheduler->trace(info.str(),hostBlock->blockId,LIGHTBLUE);

			nbreOfWaitedAnswers--;
			if (nbreOfWaitedAnswers==0) {
				if (block2Answer!=NULL) {
					sendAckMap(block2Answer);
					block2Answer=NULL;
					info.str("");
					info << "waits for train message";
				} else {
// you are master block because NULL father
					info.str("");
					info << " next step";
				}
				scheduler->trace(info.str(),hostBlock->blockId,GOLD);
				currentTrainGain=0;

				PointRel3D pos;
				pos.x = robotBlock->position[0];
				pos.y = robotBlock->position[1];
				pos.z = robotBlock->position[2];
				goodPlace = targetGrid[(pos.z*gridSize[1]+pos.y)*gridSize[0]+pos.x]==fullCell;
				PointRel3D pt;
				calcPossibleMotions(pt);
			}
		}
			break;

		case TRAIN_MSG_ID : {
			TrainMessage_ptr recvMessage = std::static_pointer_cast<TrainMessage>(message);
			unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
			info.str("");
			info << "rec. TrainMessage (" << recvMessage->newPos << "," << recvMessage->gain <<") from " << sourceId;
			currentTrainGain+=recvMessage->gain;
			currentTrainGoal= recvMessage->newPos;
			info << "\ncurrentGain = " << currentTrainGain;
			scheduler->trace(info.str(),hostBlock->blockId);

			trainPrevious = recvMessage->sourceInterface;
			robotBlock->setPrevNext(trainPrevious,trainNext);

			if (block2Answer==NULL) {
				calcPossibleMotions(recvMessage->newPos);
				sendLinkTrainMessages(recvMessage->destinationInterface);
			} else {
				stringstream info;
				info.str("");
				info << "block2answer!=NULL";
				scheduler->trace(info.str(),hostBlock->blockId,RED);
			}
		}
			break;

        case ACKTRAIN_MSG_ID : {
			AckTrainMessage_ptr recvMessage = std::static_pointer_cast<AckTrainMessage>(message);
			unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
			info.str("");
			info << "rec. AckTrainMessage("<< recvMessage->answer << ") from " << sourceId;
			scheduler->trace(info.str(),hostBlock->blockId);
			// 2 situations à gérer :
			// - le voisin ne peut pas se déplacer
			// - le bloc est en tête
			if (recvMessage->answer && trainPrevious!=NULL) {
				AckTrainMessage *message = new AckTrainMessage(true);
				scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, trainPrevious->connectedInterface));
				info.str("");
				info << "send AckTrainMessage(true) to " << trainPrevious->connectedInterface->connectedInterface->hostBlock->blockId;
				scheduler->trace(info.str(),hostBlock->blockId,GREEN);
			} else {
				// gestion de l'échec
				if (!recvMessage->answer) {
					trainPrevious=NULL;

					if (possibleMotions && !possibleMotions->empty()) {
						// il y a eu échec sur la premiere règle
						// on essaye la suivante :
						Validation *firstCondition = possibleMotions->back();
						possibleMotions->pop_back();
						delete firstCondition;

						info.str("");
						info << possibleMotions->size();
						std::reverse_iterator<vector<Validation*>::iterator> rev_until (possibleMotions->begin());
						std::reverse_iterator<vector<Validation*>::iterator> rev_from (possibleMotions->end());
						while (rev_from!=rev_until) {
							info  << "/" << (*rev_from)->capa->isHead << ":" << (*rev_from)->capa->isEnd << " " << (*rev_from)->capa->name << " gain=" << (*rev_from)->gain;
							rev_from++;
						}
						scheduler->trace(info.str(),hostBlock->blockId,GOLD);

						/***********************************/
						/* IL FAUT TESTER SI C'EST UNE FIN */

						if (!possibleMotions->empty()) {
							sendLinkTrainMessages(trainNext);
						} /*else {
							sendLinkTrainMessages(trainNext,NULL);
							trainNext=NULL;
							robotBlock->setPrevNext(trainPrevious,trainNext);
							}*/
					} else {
//								sendLinkTrainMessages(block2Answer);
					}
				} else {
					info.str("");
					info << "Head of train :" << robotBlock->blockId << ", next=" << trainNext->hostBlock->blockId;
					//info << "\n" << robotBlock->blockId << " mv(" << motionVector.x << "," << motionVector.y << "," << motionVector.z << ")"
					//		 << " nmv(" << nextMotionVector.x << "," << nextMotionVector.y << "," << nextMotionVector.z << ")";
					scheduler->trace(info.str(),hostBlock->blockId,GREEN);
					block2Answer=NULL;
					sendAnswerDelayOrMotionDelayMessage(scheduler->now()-10*COM_DELAY);
				}
			}
        }
			break;

        case MOTIONDELAY_MSG_ID : {
			MotionDelayMessage_ptr recvMessage = std::static_pointer_cast<MotionDelayMessage>(message);
			unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
			info.str("");
			info << robotBlock->blockId << " rec. MotionDelayMsg(" << recvMessage->unlockMode << ") from " << sourceId;
			scheduler->trace(info.str(),hostBlock->blockId,GREEN);
			info.str("");
			info << robotBlock->blockId << " mv(" << motionVector.x << "," << motionVector.y << "," << motionVector.z << ")"
				 << " nmv(" << nextMotionVector.x << "," << nextMotionVector.y << "," << nextMotionVector.z << ")";
			scheduler->trace(info.str(),hostBlock->blockId,GREEN);

			if (recvMessage->unlockMode) {
				trainPrevious=NULL;
				robotBlock->setPrevNext(trainPrevious,trainNext);
			}
			sendAnswerDelayOrMotionDelayMessage(recvMessage->globalTime);
		}
			break;
        case ANSWERDELAY_MSG_ID : {
			AnswerDelayMessage_ptr recvMessage = std::static_pointer_cast<AnswerDelayMessage>(message);
			unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
			info.str("");
			info << robotBlock->blockId << " rec. AnswerDelayMsg(" << recvMessage->globalRDVTime << ") from " << sourceId;
			scheduler->trace(info.str(),hostBlock->blockId);
			Vector3D finalPosition;
			finalPosition.set(robotBlock->position.pt[0]+motionVector.x,
							  robotBlock->position.pt[1]+motionVector.y,robotBlock->position.pt[2]+motionVector.z);
			blockToUnlock=0;
			scheduler->schedule(new TranslationStartEvent(recvMessage->globalRDVTime,robotBlock,finalPosition));
			stringstream info;
			info.str("");
			info << robotBlock->blockId << " TranslationStartEvent(" << recvMessage->globalRDVTime << ")";
			scheduler->trace(info.str(),hostBlock->blockId,LIGHTGREY);
			if (trainPrevious) {
				AnswerDelayMessage *adm_message = new AnswerDelayMessage(recvMessage->globalRDVTime,false);
				scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, adm_message, trainPrevious->connectedInterface));
				stringstream info;
				info.str("");
				info << robotBlock->blockId << " send AnswerDelayMsg("<< adm_message->globalRDVTime << ") to " << trainPrevious->hostBlock->blockId;
				scheduler->trace(info.str(),hostBlock->blockId,GREEN);
			}
		}
			break;

        case UNLOCK_MSG_ID : {
			UnlockMessage_ptr recvMessage = std::static_pointer_cast<UnlockMessage>(message);
			unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
			info.str("");
			info << robotBlock->blockId << " rec. UnlockMessage(" << recvMessage->target << ") from " << sourceId;
			scheduler->trace(info.str(),hostBlock->blockId);

			// search if target is directly connected to the block
			int i=0;
			P2PNetworkInterface *p2p = robotBlock->getInterface(SCLattice::Direction(i));
			bool found=(p2p->connectedInterface && p2p->connectedInterface->hostBlock->blockId==recvMessage->target);
			//cout <<(p2p->connectedInterface?p2p->connectedInterface->hostBlock->blockId:-1) << endl;
			while (i<6 && !found) {
				i++;
				if (i<6) {
					p2p = robotBlock->getInterface(SCLattice::Direction(i));
					found=(p2p->connectedInterface && p2p->connectedInterface->hostBlock->blockId==recvMessage->target);
					//cout <<(p2p->connectedInterface?p2p->connectedInterface->hostBlock->blockId:-1) << endl;
				}
			}
			if (found) {
				Time time = scheduler->now();
				MotionDelayMessage *message = new MotionDelayMessage(time,true);
				scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(time + COM_DELAY, message, p2p));
				stringstream info;
				info.str("");
				info << robotBlock->blockId << " send MotionDelayMsg(unlock) to " << p2p->connectedInterface->hostBlock->blockId;
				scheduler->trace(info.str(),hostBlock->blockId,GREEN);
			}
        }
			break;
/**************************
				case RELINKTRAIN_MSG_ID : {
					ReLinkTrainMessage_ptr recvMessage = std::static_pointer_cast<ReLinkTrainMessage>(message);
					unsigned int sourceId = recvMessage->sourceInterface->hostBlock->blockId;
          info.str("");
					info << " rec. ReLinkTrainMessage() from " << sourceId;
					scheduler->trace(info.str(),hostBlock->blockId,GREEN);
					trainNext=message->destinationInterface;
					trainPrevious=NULL;
					currentTrainGain=0;
					PointRel3D pt;
					calcPossibleMotions(pt);
					sendReLinkTrainMessage();
				}
        break;*/

        default :
			cerr << "Block " << hostBlock->blockId << " received an unrecognized message from " << message->sourceInterface->hostBlock->blockId << endl;
			break;
		}
		break;
	}
	robotBlock->setColor((trainPrevious?(trainNext?MAGENTA:PINK):(trainNext?RED:goodPlace?GREEN:YELLOW)));
}

BlockCode* Robot01BlockCode::buildNewBlockCode(BuildingBlock *host) {
	return(new Robot01BlockCode((RobotBlocksBlock*)host));
}

void Robot01BlockCode::sendMapToNeighbors(P2PNetworkInterface *p2pExcept) {
	P2PNetworkInterface *p2p;
	stringstream info;

	for(int i = 0; i < 6; i++) {
		p2p = robotBlock->getInterface(SCLattice::Direction(i));
		if( p2p->connectedInterface && p2p!=p2pExcept) {
			MapMessage *message = new MapMessage(gridSize,targetGrid);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
			nbreOfWaitedAnswers++;
			info.str("");
			info << "send MapMessage to " << p2p->connectedInterface->hostBlock->blockId;
			scheduler->trace(info.str(),hostBlock->blockId);
		}
	}
}

void Robot01BlockCode::sendAckMap(P2PNetworkInterface *p2p) {
	AckMapMessage *message = new AckMapMessage ();
	scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
	stringstream info;
	info.str("");
	info << "send AcqMapMessage(" << nbreOfWaitedAnswers << ") to " << p2p->connectedInterface->hostBlock->blockId;
	scheduler->trace(info.str(),hostBlock->blockId);
}

void Robot01BlockCode::calcPossibleMotions(const PointRel3D &posTrainGoal) {
	stringstream info;
	PresenceMatrix pm,localTargetGrid;

	if (possibleMotions) {
		vector <Validation*>::const_iterator ci = possibleMotions->begin();
		while (ci!=possibleMotions->end()) {
			delete (*ci);
			ci++;
		}
		possibleMotions->clear();
		delete possibleMotions;
		possibleMotions = NULL;
	}

// définit si le block est bien placé
	PointRel3D pos;
	pos.x = robotBlock->position[0];
	pos.y = robotBlock->position[1];
	pos.z = robotBlock->position[2];
	goodPlace = targetGrid[(pos.z*gridSize[1]+pos.y)*gridSize[0]+pos.x]==fullCell;
	getPresenceMatrix(pos,pm);
	if (posTrainGoal.isSet()) {
		pm.add(lockedCell,posTrainGoal,pos);
	}
// détermination de la localTargetGrid
	getLocalTargetGrid(pos,localTargetGrid);
	possibleMotions = getCapabilities()->validateMulti(pm,localTargetGrid);
	if (possibleMotions) {
		info.str("");
		info << possibleMotions->size();
		std::reverse_iterator<vector<Validation*>::iterator> rev_until (possibleMotions->begin());
		std::reverse_iterator<vector<Validation*>::iterator> rev_from (possibleMotions->end());
		while (rev_from!=rev_until) {
			info  << "/" << (*rev_from)->capa->isHead << ":" << (*rev_from)->capa->isEnd << (*rev_from)->capa->name;
			rev_from++;
		}
		scheduler->trace(info.str(),hostBlock->blockId,GOLD);
		// critère pour lancer l'étape suivante :
		// - le bloc est une tete de train
		// - il est blocké par un block en place ?

		if (possibleMotions->back()->capa->isHead) {
			info.str("");
			info << "Head " << possibleMotions->back()->capa->name << ":" << possibleMotions->back()->gain;
			scheduler->trace(info.str(),hostBlock->blockId,GOLD);
			sendLinkTrainMessages(NULL);
		}
	} else {
		info.str("No motion!");
		scheduler->trace(info.str(),hostBlock->blockId,GOLD);
	}
}

void Robot01BlockCode::getLocalTargetGrid(const PointRel3D &pos,PresenceMatrix &pm) {
    presence *gpm=pm.grid;
    presence *tg = targetGrid;

    for (int i=0; i<27; i++) { *gpm++ = wallCell; };

    int ix0 = (pos.x<1)?1-pos.x:0,
        ix1 = (pos.x>gridSize[0]-2)?gridSize[0]-pos.x+1:3,
        iy0 = (pos.y<1)?1-pos.y:0,
        iy1 = (pos.y>gridSize[1]-2)?gridSize[1]-pos.y+1:3,
        iz0 = (pos.z<1)?1-pos.z:0,
        iz1 = (pos.z>gridSize[2]-2)?gridSize[2]-pos.z+1:3,
        ix,iy,iz;

    for (iz=iz0; iz<iz1; iz++) {
        for (iy=iy0; iy<iy1; iy++) {
            gpm = pm.grid+((iz*3+iy)*3+ix0);
            tg = targetGrid+(ix0+pos.x-1+(iy+pos.y-1+(iz+pos.z-1)*gridSize[1])*gridSize[0]);
            for (ix=ix0; ix<ix1; ix++) {
                *gpm++ = *tg++;
            }
        }
    }
}

void Robot01BlockCode::sendLinkTrainMessages(P2PNetworkInterface *sender) {
	if (possibleMotions && !possibleMotions->empty()) {
		Capability *firstCondition = possibleMotions->back()->capa;
		motionVector = firstCondition->getMotionVector(0,1);
		nextMotionVector = firstCondition->getMotionVector(1);
		// si ce n'est pas une queue de train, on propage au suivant
		stringstream info;
		info.str("");
		info << "FirstCondifition :"<< firstCondition->isHead << ":" << firstCondition->isEnd << firstCondition->name;
		scheduler->trace(info.str(),hostBlock->blockId,DARKORANGE);

		PointRel3D pos;
		if (firstCondition->isEnd) {
			pos = *firstCondition->linkPrevPos;
			P2PNetworkInterface *p2p = robotBlock->getP2PNetworkInterfaceByRelPos(
				Cell3DPosition(pos.x,pos.y,pos.z));
			AckTrainMessage *message = new AckTrainMessage(currentTrainGain>0);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message,p2p));
			stringstream info;
			info.str("");
			info << "send AckTrainMessage("<< int(currentTrainGain>0) << ") to " << p2p->connectedInterface->hostBlock->blockId;
			scheduler->trace(info.str(),hostBlock->blockId,GREEN);
			trainNext = NULL;
			trainPrevious = currentTrainGain>0?p2p->connectedInterface:NULL;
			robotBlock->setPrevNext(trainPrevious,trainNext);
		} else {
			pos = *firstCondition->linkNextPos;
			P2PNetworkInterface *p2p = robotBlock->getP2PNetworkInterfaceByRelPos(
				Cell3DPosition(pos.x,pos.y,pos.z));
			// si c'est la tête du train on envoie la nouvelle position
			// sinon on transmet celle recue
			TrainMessage *message;
			stringstream info;
			if (sender==NULL) {
				PointRel3D pos;
				pos.x = robotBlock->position[0]+motionVector.x;
				pos.y = robotBlock->position[1]+motionVector.y;
				pos.z = robotBlock->position[2]+motionVector.z;
				info.str("");
				info << "send TrainMessage("<<pos<<",1) to " << p2p->connectedInterface->hostBlock->blockId;
				message = new TrainMessage(pos,1);
			} else {
				message = new TrainMessage(currentTrainGoal,currentTrainGain+2*possibleMotions->back()->gain);
				info.str("");
				info << "send TrainMessage("<<currentTrainGoal<<","<<currentTrainGain+2*possibleMotions->back()->gain<<") to " << p2p->connectedInterface->hostBlock->blockId;
			}
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
			scheduler->trace(info.str(),hostBlock->blockId,GREEN);

			trainNext = p2p->connectedInterface;
			robotBlock->setPrevNext(trainPrevious,trainNext);
		}
	} else if (sender) {
		AckTrainMessage *message = new AckTrainMessage(false);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message,sender));
		stringstream info;
		info.str("");
		info << "send AckTrainMessage(false) to " << sender->connectedInterface->hostBlock->blockId;
		scheduler->trace(info.str(),hostBlock->blockId,GREEN);
		trainNext = NULL;
		trainPrevious = NULL;
		robotBlock->setPrevNext(trainPrevious,trainNext);
	}
}

void Robot01BlockCode::sendAnswerDelayOrMotionDelayMessage(Time gt) {
	if (motionVector!=nextMotionVector) {
		Time time = scheduler->now(),
			rdvTime = time+(time-gt)*1.5;
		if (trainPrevious) {
			AnswerDelayMessage *adm_message = new AnswerDelayMessage(rdvTime,trainNext!=NULL);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(time + COM_DELAY, adm_message, trainPrevious->connectedInterface));
			stringstream info;
			info.str("");
			info << robotBlock->blockId << " send AnswerDelayMessage(" << adm_message->globalRDVTime << ") to " << trainPrevious->hostBlock->blockId;
			scheduler->trace(info.str(),hostBlock->blockId,GREEN);
		}
		Vector3D finalPosition(robotBlock->position.pt[0]+motionVector.x,
							   robotBlock->position.pt[1]+motionVector.y,
							   robotBlock->position.pt[2]+motionVector.z);
		scheduler->schedule(new TranslationStartEvent(rdvTime,robotBlock,finalPosition));
		stringstream info;
		info.str("");
		info << robotBlock->blockId << " TranslationStartEvent(" << rdvTime << ")";
		scheduler->trace(info.str(),hostBlock->blockId,LIGHTGREY);
		if (trainNext) {
			blockToUnlock=trainNext->hostBlock->blockId;
			trainNext=NULL;
			robotBlock->setPrevNext(trainPrevious,trainNext);
		} else {
			blockToUnlock=0;
		}
	} else {
		if (motionVector.isZero()) {
			cout << "erreur, block #"<< robotBlock->blockId << "mv=nmv=0 ?" << endl;
			return;
		}
		MotionDelayMessage *mdm_message = new MotionDelayMessage(gt);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, mdm_message, trainNext->connectedInterface));
		stringstream info;
		info.str("");
		info << " send MotionDelayMsg(" << motionVector.x << "," << motionVector.y << "," << motionVector.z << ") to " << trainNext->hostBlock->blockId;
		scheduler->trace(info.str(),hostBlock->blockId,GREEN);
		blockToUnlock=0;
	}
}

void Robot01BlockCode::sendUnlockMessage(int id) {
	P2PNetworkInterface *p2p;
	stringstream info;

	for(int i = 0; i < 6; i++) {
        p2p = robotBlock->getInterface(SCLattice::Direction(i));
		if( p2p->connectedInterface) {
			UnlockMessage *message = new UnlockMessage(id);
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
			info.str("");
			info << "send UnlockMessage(" << id << ") to " << p2p->connectedInterface->hostBlock->blockId;
			scheduler->trace(info.str(),hostBlock->blockId);
		}
	}
}

void Robot01BlockCode::sendReLinkTrainMessage() {
	stringstream info;

	if (possibleMotions && !possibleMotions->empty()) {
		Capability *firstCondition = possibleMotions->back()->capa;
		if (firstCondition->isHead) {
			sendLinkTrainMessages(NULL);
		} else if (firstCondition->linkPrevPos) {
						PointRel3D pos = *firstCondition->linkPrevPos;
			P2PNetworkInterface *p2p = robotBlock->getP2PNetworkInterfaceByRelPos(
				Cell3DPosition(pos.x,pos.y,pos.z));
			ReLinkTrainMessage *message = new ReLinkTrainMessage();
			scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, p2p));
			info.str("");
			info << "send ReLinkTrainMessage() to " << p2p->connectedInterface->hostBlock->blockId;
			scheduler->trace(info.str(),hostBlock->blockId);

			trainPrevious = p2p->connectedInterface;
		}
	} else {
		AckTrainMessage *message = new AckTrainMessage(false);
		scheduler->schedule(new NetworkInterfaceEnqueueOutgoingEvent(scheduler->now() + COM_DELAY, message, trainNext));
		stringstream info;
		info.str("");
		info << "send AckTrainMessage(false) to " << trainNext->connectedInterface->hostBlock->blockId;
		scheduler->trace(info.str(),hostBlock->blockId,GREEN);
		trainNext=NULL;
		trainPrevious=NULL;
		robotBlock->setPrevNext(trainPrevious,trainNext);
	}
}

MapMessage::MapMessage(short *gs,presence *tg):Message(){
	id = MAP_MSG_ID;
	memcpy(gridSize,gs,3*sizeof(short));
	targetGrid = initGrid(gridSize,tg);
}

MapMessage::~MapMessage() {
	delete [] targetGrid;
}

AckMapMessage::AckMapMessage():Message(){
	id = ACKMAP_MSG_ID;
}

AckMapMessage::~AckMapMessage() {
}

TrainMessage::TrainMessage(const PointRel3D &p,int g):Message(){
	id = TRAIN_MSG_ID;
	newPos = p;
	gain=g;
}

TrainMessage::~TrainMessage() {
}

AckTrainMessage::AckTrainMessage(bool v):Message(){
	id = ACKTRAIN_MSG_ID;
	answer=v;
}

AckTrainMessage::~AckTrainMessage() {
}


MotionDelayMessage::MotionDelayMessage(Time time,bool unlock):Message(){
	id = MOTIONDELAY_MSG_ID;
	globalTime = time;
	unlockMode = unlock;
}

MotionDelayMessage::~MotionDelayMessage() {
}

AnswerDelayMessage::AnswerDelayMessage(Time time,bool b2ul):Message(){
	id = ANSWERDELAY_MSG_ID;
	globalRDVTime = time;
	block2unlock=b2ul;
}

AnswerDelayMessage::~AnswerDelayMessage() {
}

UnlockMessage::UnlockMessage(int tid) {
    id = UNLOCK_MSG_ID;
    target=tid;
}

UnlockMessage::~UnlockMessage() {
}

ReLinkTrainMessage::ReLinkTrainMessage() {
    id = RELINKTRAIN_MSG_ID;
}

ReLinkTrainMessage::~ReLinkTrainMessage() {
}

void Robot01BlockCode::parseUserElements(TiXmlDocument *config) {
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
	    setTargetGrid(fullCell, p[0], p[1], p[2]);
	}

	// then parse and load capabilities...
	TiXmlNode *nodeCapa = xmlWorldNode->FirstChild("capabilities");
	if (nodeCapa) {
		setCapabilities(new Capabilities(nodeCapa));
	}
}

void Robot01BlockCode::initTargetGrid() {
	if (targetGrid) delete [] targetGrid;
	int sz = lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2];
	targetGrid = new presence[lattice->gridSize[0]*lattice->gridSize[1]*lattice->gridSize[2]];
	memset(targetGrid,0,sz*sizeof(presence));
}

void Robot01BlockCode::getPresenceMatrix(const PointRel3D &pos, PresenceMatrix &pm) {
	presence *gpm = pm.grid;
    BuildingBlock **grb;

	//memset(pm.grid,wall,27*sizeof(presence));

	for (int i = 0; i < 27; i++) { *gpm++ = wallCell; };

	int ix0 = (pos.x < 1) ? 1  -  pos.x : 0,
		ix1 = (pos.x > lattice->gridSize[0] - 2) ? lattice->gridSize[0] - pos.x + 1 : 3,
		iy0 = (pos.y < 1) ? 1 - pos.y : 0,
		iy1 = (pos.y > lattice->gridSize[1] - 2) ? lattice->gridSize[1] - pos.y + 1 : 3,
		iz0 = (pos.z < 1) ? 1 - pos.z : 0,
		iz1 = (pos.z > lattice->gridSize[2] - 2) ? lattice->gridSize[2] - pos.z + 1 : 3,
		ix,iy,iz;
	for (iz = iz0; iz < iz1; iz++) {
		for (iy = iy0; iy < iy1; iy++) {
			gpm = pm.grid + ((iz * 3 + iy) * 3 + ix0);
			grb = (BuildingBlock **)lattice->grid +
				(ix0 + pos.x - 1 + (iy + pos.y - 1 +
									(iz + pos.z - 1) * lattice->gridSize[1]) * lattice->gridSize[0]);
			for (ix = ix0; ix < ix1; ix++) {
				*gpm++ = (*grb++) ? fullCell : emptyCell;
			}
		}
	}
}
