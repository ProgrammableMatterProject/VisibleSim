/*
 * meldProcessVM.h
 *
 *  Created on: 28 avril 2015
 *      Author: Andre
 */

#ifndef MELDPROCESSVM_H_
#define MELDPROCESSVM_H_

#include <iostream>
#include <queue>
#include <inttypes.h>
#include <boost/asio.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include <boost/interprocess/sync/interprocess_semaphore.hpp>
#include "meldProcessVMCommands.h"
#include "buildingBlock.h"

using namespace std;
using boost::asio::ip::tcp;

namespace MeldProcess {

//class BuildingBlock;

class MeldProcessVM {

protected:
	/* associated Block */
	BaseSimulator::BuildingBlock* hostBlock;
	static boost::interprocess::interprocess_mutex mutex_ios;
   
	inline tcp::socket& getSocket() { 
		assert(socket != NULL);
		return *(socket.get()); };
	/* kill the associated VM program (and wait for the effective end) */
	void terminate();
	
	//static int port;
	static boost::asio::io_service *ios;
	static tcp::acceptor *acceptor;
	static string vmPath;
	static string programPath;
	static bool debugging;
	
	uint64_t currentLocalDate; // fastest mode
	bool hasWork; // fastest mode
	bool polling; // fastest mode

public:

	/* socket connected to the associated VM program */
	boost::shared_ptr<tcp::socket> socket;
	
	MeldProcessVM(BaseSimulator::BuildingBlock* bb);
	~MeldProcessVM();

	/* associated VM program pid */
	pid_t pid;
	/* buffer used to receive tcp message */
	commandType inBuffer[VM_COMMAND_MAX_LENGHT];
		
	commandType nbSentCommands; // mode fastest 1
	
	/* True only if the id was sent */
	bool idSent;
	/* True only if the simulation is in fastest mode and it has been
	 * set on the VM. */
	bool deterministicSet;
	
	void asyncAcceptHandler(boost::system::error_code& error, bool* connected);
   
   /* send and receive message from the associated VM program */
	int sendCommand(VMCommand &command);
	void asyncReadCommand();	
	void asyncReadCommandHandler(const boost::system::error_code& error, std::size_t bytes_transferred);
	void handleInBuffer();
	void handle_write(const boost::system::error_code& error);
	/* kill the VM process */
	void killProcess();
	/* close the socket associated to the VM program */
	void closeSocket();
   
	void handleDeterministicMode(VMCommand &command);
   	void handleCommand(VMCommand &command);
   	
	inline static bool isInDebuggingMode() { return debugging; };
	static void setConfiguration(string v, string p, bool d);
	static void createServer(int p);
	static void deleteServer();
	static void checkForReceivedCommands();
	static void waitForOneCommand();
	
	
};

	inline void createVMServer(int p) { MeldProcessVM::createServer(p); };
	inline void deleteVMServer() { MeldProcessVM::deleteServer(); };
	inline void setVMConfiguration(string v, string p, bool d) { MeldProcessVM::setConfiguration(v,p,d); };
	inline void checkForReceivedVMCommands() { MeldProcessVM::checkForReceivedCommands(); };
	inline void waitForOneVMCommand() { MeldProcessVM::waitForOneCommand(); };
	
	/*
	setVMConfiguration(vmPath, programPath, debugging);
	createVMServer(port);
	if(debugging) {
		createDebugger();
	}
		deleteDebugger();
	deleteVMServer();
	* 
	* 
	* 
	/*
	 * 
	 		
	TiXmlNode *node = xmlDoc->FirstChild("vm");
	if (node) {		
		TiXmlElement* vmElement = node->ToElement();
		const char *attr = vmElement->Attribute("serverport");
		if (attr) {
			port = atoi(attr);
		}
		attr = vmElement->Attribute("vmPath");
		if (attr) {
			vmPath = string(attr);
		}	
		attr = vmElement->Attribute("programPath");
		if (attr) {
         if (programPath == "") {
            programPath = string(attr);
         } else {
            cerr << "Warning: meld program provided in the command line and in the xml file" << endl;
            cerr << "Warning: meld program provided in the xml file is ignored" << endl;
         }
		}
		attr = vmElement->Attribute("debugging");
		if (attr) {
			if ((strcmp(attr, "True") == 0) ||(strcmp(attr, "true") == 0) ) {
					if (!testMode) {
                  debugging = true;
               }
			}
		}
	}
   
   if (programPath == "")
      help();
      
	Sends directly (not scheduled) a message to all the active VMs of the world.
	Returns to how many nodes the message has been sent.

	//int broadcastDebugCommand(DebbuggerVMCommand &c);
	//int sendCommand(int id, VMCommand &c);
	//bool dateHasBeenReachedByAll(uint64_t date);
	//bool equilibrium();
	//void killAllVMs();
	//void closeAllSockets();
	
	bool BlinkyBlocksWorld::dateHasBeenReachedByAll(uint64_t date) {
		static uint64_t minReallyReached = 0;
		uint64_t min, min2;
		int alive = 0, hasNoWork = 0;
		if (date < minReallyReached) {
			return true;
		}
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		for(it = buildingBlocksMap.begin();
				it != buildingBlocksMap.end(); it++) {
			BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
			BlinkyBlocksBlockCode *bc = (BlinkyBlocksBlockCode*) bb->blockCode;
			if (bb->getState() < BlinkyBlocksBlock::ALIVE) {
				continue;
			}
			alive++;
			if (!bc->hasWork || bc->polling) {
				hasNoWork++;
				if (alive == 1) {
					min2 = bc->currentLocalDate;
				} else if (bc->currentLocalDate < min2) {
					min2 = bc->currentLocalDate;
				}
			} else {
				if ((alive - 1) == hasNoWork) {
					min = bc->currentLocalDate;
				} else if (bc->currentLocalDate < min) {
					min = bc->currentLocalDate;
				}
				if (min < min2) {
					min2 = min;
				}
			}
		}
		if (alive==hasNoWork) {
			return true;
		}
		minReallyReached = min2;
		return (date < min);
	}

	
	void BlinkyBlocksWorld::killAllVMs() {
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		for(it = buildingBlocksMap.begin();
				it != buildingBlocksMap.end(); it++) {
			BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
			bb->killVM();
		}
	}*/
	/*
   void BlinkyBlocksWorld::closeAllSockets() {
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		for(it = buildingBlocksMap.begin();
				it != buildingBlocksMap.end(); it++) {
			BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
         if(bb->vm != NULL) {
            bb->vm->socket->close();
            bb->vm->socket.reset();
         }
		}
	}
	
		int BlinkyBlocksWorld::broadcastDebugCommand(DebbuggerVMCommand &c) {
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		int aliveBlocks = 0;
		for(it = buildingBlocksMap.begin();
				it != buildingBlocksMap.end(); it++) {
			BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
			BlinkyBlocksBlockCode* bbc = (BlinkyBlocksBlockCode*) bb->blockCode;
			// Send id & set deterministic mode if necessary
			bbc->init();
			aliveBlocks += bb->sendCommand(c);
		}
		return aliveBlocks;
	}

	int BlinkyBlocksWorld::sendCommand(int id, VMCommand &c) {
		BlinkyBlocksBlock *bb = (BlinkyBlocksBlock*)getBlockById(id);
      BlinkyBlocksBlockCode* bbc = (BlinkyBlocksBlockCode*) bb->blockCode;
		bbc->init();
      return bb->sendCommand(c);
	} 
	

	bool BlinkyBlocksWorld::equilibrium() {
		map<int, BaseSimulator::BuildingBlock*>::iterator it;
		for(it = buildingBlocksMap.begin();
				it != buildingBlocksMap.end(); it++) {
			BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
			BlinkyBlocksBlockCode *bc = (BlinkyBlocksBlockCode*) bb->blockCode;
			if (bb->getState() < BlinkyBlocksBlock::ALIVE) {
				continue;
			}
			if (bc->hasWork) {
				return false;
			}
		}
		return true;
	}*/
}
#endif /* MELDPROCESS_VM_H */
