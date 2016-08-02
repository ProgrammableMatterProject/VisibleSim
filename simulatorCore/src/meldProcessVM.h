
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
#include <boost/interprocess/sync/interprocess_mutex.hpp>
#include "meldProcessVMCommands.h"
#include "buildingBlock.h"

using namespace std;
using boost::asio::ip::tcp;

namespace MeldProcess {

//class BuildingBlock;

class MeldProcessVM;

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
	
	static map<int,MeldProcessVM*> vmMap;

public:

	/* socket connected to the associated VM program */
    std::shared_ptr<tcp::socket> socket;
	
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
   	
   	void setCurrentLocalDate(uint64_t t) {currentLocalDate = t;}
   	void setPolling(bool b) {polling = b; }
   	
	inline static bool isInDebuggingMode() { return debugging; };
	static void setConfiguration(string v, string p, bool d);
	static void createServer(int p);
	static void deleteServer();
	static void checkForReceivedCommands();
	static void waitForOneCommand();
	
	static bool dateHasBeenReachedByAll(uint64_t date);
	static bool equilibrium();
	static int broadcastDebugCommand(DebbuggerVMCommand &c);
	static int sendCommand(int id, VMCommand &c);
	static void closeAllSockets();
	static MeldProcessVM* getMeldProcessVMById(int id);
	
};

	inline void createVMServer(int p) { MeldProcessVM::createServer(p); };
	inline void deleteVMServer() { MeldProcessVM::deleteServer(); };
	inline void setVMConfiguration(string v, string p, bool d) { MeldProcessVM::setConfiguration(v,p,d); };
	inline void checkForReceivedVMCommands() { MeldProcessVM::checkForReceivedCommands(); };
	inline void waitForOneVMCommand() { MeldProcessVM::waitForOneCommand(); };
	
	/*
	    
	Sends directly (not scheduled) a message to all the active VMs of the world.
	Returns to how many nodes the message has been sent.

	//int broadcastDebugCommand(DebbuggerVMCommand &c);
	//int sendCommand(int id, VMCommand &c);
	//bool dateHasBeenReachedByAll(uint64_t date);
	//bool equilibrium();
	//void killAllVMs();
	//void closeAllSockets();

	*/
}
#endif /* MELDPROCESS_VM_H */
