#include "meldProcessVM.h"
#include "meldProcessScheduler.h"
#include "meldProcessDebugger.h"
#include "meldProcessEvents.h"

//#include "blinkyBlocksBlock.h"
//#include "blinkyBlocksBlockCode.h"
//#include "blinkyBlocksEvents.h"

#include <boost/asio/io_service.hpp>
#include <sys/wait.h>
#include <stdio.h>
#include <functional>
#include "trace.h"
#include <stdexcept>
#include <string.h>
#include "events.h"
#include "openglViewer.h"

#include <boost/bind.hpp>
#include <memory>
#include <boost/enable_shared_from_this.hpp>

using namespace boost;
using asio::ip::tcp;

namespace MeldProcess {

boost::asio::io_service *MeldProcessVM::ios = NULL;
boost::interprocess::interprocess_mutex MeldProcessVM::mutex_ios;
tcp::acceptor *MeldProcessVM::acceptor = NULL;
string MeldProcessVM::vmPath;
string MeldProcessVM::programPath;
bool MeldProcessVM::debugging = false;
map<int, MeldProcessVM*> MeldProcessVM::vmMap;

MeldProcessVM::MeldProcessVM(BuildingBlock* bb){
	int ret;
	boost::system::error_code error;
	stringstream vmLogFile;

	assert(ios != NULL && acceptor != NULL);
	hostBlock = bb;
	currentLocalDate = 0; // mode fastest
	hasWork = true; // mode fastest
	polling = false; // mode fastest
		
	vmLogFile << "VM" << hostBlock->blockId << ".log";
   
	OUTPUT << "VM "<< hostBlock->blockId << " constructor" << endl;
	// Start the VM
	pid = 0;
	memset(inBuffer,0,VM_COMMAND_MAX_LENGHT*sizeof(commandType));
	mutex_ios.lock();
	ios->notify_fork(boost::asio::io_service::fork_prepare);
	pid = fork();
	if(pid < 0) {ERRPUT << "Error when starting the VM" << endl;}
	if(pid == 0) {
		ios->notify_fork(boost::asio::io_service::fork_child);
		mutex_ios.unlock();
		acceptor->close();
		closeAllSockets();
#ifdef LOGFILE
		log_file.close();
#endif
		int fd = open(vmLogFile.str().c_str(), O_RDWR | O_CREAT | O_TRUNC, S_IRUSR | S_IWUSR);
		dup2(fd, 1);
		dup2(fd, 2);
		close(fd);
		if (debugging) {
			//./meld -f  /home/ubuntu/Bureau/CMU/meld/examples/ends.m -c sl -D SIM
			char* cmd[] = {(char*)vmPath.c_str(), (char*)"-a", (char*)"bbsim", (char*)"-f", (char*)programPath.c_str(), (char*)"-c", (char*) "sl", (char*) "-D", (char*) "SIM", NULL };
			ret = execv(vmPath.c_str(), const_cast<char**>(cmd));
		} else {
			//./meld -f  /home/ubuntu/Bureau/CMU/meld/examples/ends.m -c sl
			char* cmd[] = {(char*)vmPath.c_str(), (char*)"-a", (char*)"bbsim", (char*)"-f", (char*)programPath.c_str(), (char*)"-c", (char*) "sl", NULL };
			ret = execv(vmPath.c_str(), const_cast<char**>(cmd));
		}
		if(ret == -1) {
			cerr << "Error: VM executable, " << vmPath.c_str()  << ", does not exist or is not executable" << endl;
			exit(EXIT_FAILURE);
		}
	}	
	ios->notify_fork(boost::asio::io_service::fork_parent);
	mutex_ios.unlock();
	socket = std::shared_ptr<tcp::socket>(new tcp::socket(*ios));
	if (hostBlock->blockId == 1) {
		bool connected = false;
      
		acceptor->async_accept(*(socket.get()), boost::bind(&MeldProcessVM::asyncAcceptHandler, this, error , &connected));
		while(!connected &&  (pid != waitpid(pid, NULL, WNOHANG))) {
         	if (!MeldProcessVM::isInDebuggingMode()) { // In debugging mode the scheduler thread is looking for connections
				checkForReceivedVMCommands(); // it is actually check for connection
				usleep(10000);
            }
		}
		if(!connected) {
			ifstream file (vmLogFile.str().c_str());
			string line;
			cerr << "VisibleSim error: unable to connect to the VM" << endl;
			cerr << vmLogFile.str() << ":" << endl;
			if (file.is_open()) {
				while (!file.eof()) {
					getline(file,line);
					cerr << line;
				}
				cerr << endl;
				file.close();
			}
			acceptor->close();
			exit(EXIT_FAILURE);
		}
	} else {
		acceptor->accept(*(socket.get()));
	}
	idSent = false;
	deterministicSet = false;
	nbSentCommands = 0;
	asyncReadCommand();
	vmMap.insert(std::pair<int,MeldProcessVM*>(bb->blockId,this));
}

void MeldProcessVM::asyncAcceptHandler(boost::system::error_code& error, bool* connected) {
	if (error) {
		*connected = false;
	} else {
		*connected = true;
	}
}

MeldProcessVM::~MeldProcessVM() {
	closeSocket();
	killProcess();
}

void MeldProcessVM::terminate() {
	waitpid(pid, NULL, 0);
}

void MeldProcessVM::killProcess() {
	kill(pid, SIGTERM);
	waitpid(pid, NULL, 0);
}

void MeldProcessVM::closeSocket() {
	if (socket != NULL) {
		socket->cancel();
		socket->close();
		socket.reset();
	}
}

void MeldProcessVM::asyncReadCommandHandler(const boost::system::error_code& error, std::size_t bytes_transferred) {
	if(error) {
		ERRPUT << "An error occurred while receiving a tcp command from VM " << hostBlock->blockId << " (socket closed ?) " <<endl;
		return;
	}
    try {
		memset(inBuffer+1, 0, inBuffer[0]);
		boost::asio::read(getSocket(),boost::asio::buffer((void*)(inBuffer + 1), inBuffer[0]) );
	} catch (std::exception& e) {
		ERRPUT << "Connection to the VM "<< hostBlock->blockId << " lost" << endl;
	}
	handleInBuffer();
	while (socket->available()) {
		try {
			boost::asio::read(getSocket(), boost::asio::buffer(inBuffer, sizeof(commandType))); 
			boost::asio::read(getSocket(),boost::asio::buffer((void*)(inBuffer + 1), inBuffer[0]));
		}  catch (std::exception& e) {
			ERRPUT << "Connection to the VM "<< hostBlock->blockId << " lost" << endl;
		}
		handleInBuffer();
	}
    this->asyncReadCommand();
}

void MeldProcessVM::handleInBuffer() {
	VMCommand command(inBuffer);
	handleCommand(command);
}

void MeldProcessVM::asyncReadCommand() {
	if (socket == NULL) {
		ERRPUT << "Simulator is not connected to the VM "<< hostBlock->blockId << endl;
		return;
	}
	try {
		inBuffer[0] = 0;
		boost::asio::async_read(getSocket(),
								boost::asio::buffer(inBuffer, sizeof(commandType)),
								boost::bind(&MeldProcessVM::asyncReadCommandHandler, this,
											boost::asio::placeholders::error,
											boost::asio::placeholders::bytes_transferred));
	} catch (std::exception& e) {
		ERRPUT << "Connection to the VM "<< hostBlock->blockId << " lost" << endl;
	}
}

int MeldProcessVM::sendCommand(VMCommand &command){
	if (socket == NULL) {
		ERRPUT << "Simulator is not connected to the VM "<< hostBlock->blockId << endl;
		return 0;
	}
	if (command.getType() != VM_COMMAND_DEBUG) {
		nbSentCommands++;
		handleDeterministicMode(command);
	}
	try {
		boost::asio::write(getSocket(), boost::asio::buffer(command.getData(), command.getSize()));
	} catch (std::exception& e) {
		ERRPUT << "Connection to the VM "<< hostBlock->blockId << " lost" << endl;
		return 0;
	}
	return 1;
}


void MeldProcessVM::handle_write(const boost::system::error_code& error)
{
    if (!error)
    {
		cout << "ok" << endl;
    }
}

void MeldProcessVM::checkForReceivedCommands() {
	if (ios != NULL) {
		mutex_ios.lock();
		try {
			ios->poll();
			ios->reset();
		} catch (boost::exception& e) {
		}
		mutex_ios.unlock();
	}
}

void MeldProcessVM::waitForOneCommand() {
	if (ios != NULL) {
		mutex_ios.lock();
		try {
			ios->run_one();
			ios->reset();
		} catch (boost::exception& e) {
		}
		mutex_ios.unlock();
	}
	checkForReceivedCommands();
}

void MeldProcessVM::setConfiguration(string v, string p, bool d) {
	vmPath = v;
	programPath = p;
	debugging = d;
}

void MeldProcessVM::createServer(int p) {
	assert(ios == NULL);
	ios = new boost::asio::io_service();
	acceptor =  new tcp::acceptor(*ios, tcp::endpoint(tcp::v4(), p));
}

void MeldProcessVM::deleteServer() {
	ios->stop();
	delete acceptor;
	delete ios;
	ios = NULL; acceptor = NULL;
}

void MeldProcessVM::handleDeterministicMode(VMCommand &command){
	currentLocalDate = max(getScheduler()->now(), currentLocalDate);
	if(!hasWork && (command.getType() != VM_COMMAND_STOP)) {
		hasWork = true;
	}
}

void MeldProcessVM::handleCommand(VMCommand &command) {
	Time dateToSchedule;
		
	currentLocalDate = max(getScheduler()->now(), command.getTimestamp());
	if (getScheduler()->getMode() == SCHEDULER_MODE_FASTEST) {
		//assert(currentLocalDate <= command.getTimestamp()); -- not true because of asynchrone debug commands
		dateToSchedule = currentLocalDate;
	} else {
		dateToSchedule = getScheduler()->now();
	}
	
	switch (command.getType()) {
	case VM_COMMAND_SET_COLOR:	
	{
		// format: <size> <command> <timestamp> <src> <red> <blue> <green> <intensity>
		SetColorVMCommand c(command.getData());
		Color color = c.getColor();
		getScheduler()->scheduleLock(new SetColorEvent(dateToSchedule, hostBlock, color));
	}
	break;
	case VM_COMMAND_SEND_MESSAGE:
	{
		P2PNetworkInterface *interface;
		SendMessageVMCommand c(command.getData());
		interface = hostBlock->getP2PNetworkInterfaceByDestBlockId(c.getDestId());
		if (interface == NULL) {
			stringstream info;
			info.str("");
			info << "Warning: sends a message to " << endl << "the non-connected block " << c.getDestId();
			getScheduler()->trace(info.str(),hostBlock->blockId);
			ERRPUT << "Interface not found" << endl;
			return;
		}
		getScheduler()->scheduleLock(new VMSendMessageEvent(dateToSchedule, hostBlock,
															new ReceiveMessageVMCommand(c), interface));
	}
	break;
	case VM_COMMAND_DEBUG:
	{
		// Copy the message because it will be queued
		DebbuggerVMCommand *c = new DebbuggerVMCommand(command.getData());
		c->copyData();
		handleDebugCommand(c);
	}
	break;
	case VM_COMMAND_WORK_END:
	{
		WorkEndVMCommand c(command.getData());
		if (c.getNbProcessedMsg() == nbSentCommands) {
			hasWork = false;
		}
	}
	break;
	case VM_COMMAND_TIME_INFO:
		;
		break;
	case VM_COMMAND_POLL_START:
		// Polling lasts 1us
		getScheduler()->scheduleLock(new VMEndPollEvent(dateToSchedule+1, hostBlock));
		polling = true;
		break;
	default:
		cout << "DEFAULT" << endl;
		ERRPUT << "*** ERROR *** : unsupported message received from VM (" << command.getType() <<")" << endl;
		break;
	}
}

void MeldProcessVM::closeAllSockets() {
	map<int, MeldProcessVM*>::iterator it;
	for(it = vmMap.begin(); it != vmMap.end(); it++) {
		it->second->socket->close();
		it->second->socket.reset(); 
	}
}


bool MeldProcessVM::dateHasBeenReachedByAll(Time date) {
	static Time minReallyReached = 0;
	Time min, min2;
	int alive = 0, hasNoWork = 0;
		
	if (date < minReallyReached) {
		return true;
	}
		
	map<int, MeldProcessVM*>::iterator it;
	for(it = vmMap.begin(); it != vmMap.end(); it++) {
		MeldProcessVM *vm = it->second;
		BuildingBlock *bb = vm->hostBlock;
		if (bb->getState() < BuildingBlock::ALIVE) {
			continue;
		}
		alive++;
		if (!vm->hasWork || vm->polling) {
			hasNoWork++;
			if (alive == 1) {
				min2 = vm->currentLocalDate;
			} else if (vm->currentLocalDate < min2) {
				min2 = vm->currentLocalDate;
			}
		} else {
			if ((alive - 1) == hasNoWork) {
				min = vm->currentLocalDate;
			} else if (vm->currentLocalDate < min) {
				min = vm->currentLocalDate;
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
	
bool MeldProcessVM::equilibrium() {
	map<int, MeldProcessVM*>::iterator it;
	for(it = vmMap.begin(); it != vmMap.end(); it++) {
		MeldProcessVM *vm = it->second;
		BuildingBlock *bb = vm->hostBlock;
		if (bb->getState() < BuildingBlock::ALIVE) {
			continue;
		}
		if (vm->hasWork) {
			return false;
		}
	}
	return true;
}
	
/*void BlinkyBlocksWorld::killAllVMs() {
  map<int, MeldProcessVM*>::iterator it;
  for(it = vmMap.begin(); it != vmMap.end(); it++) {
  BlinkyBlocksBlock* bb = (BlinkyBlocksBlock*) it->second;
  bb->killVM();
  }
  }*/
		
int MeldProcessVM::broadcastDebugCommand(DebbuggerVMCommand &c) {
	map<int, MeldProcessVM*>::iterator it;
	int aliveBlocks = 0;
		
	for(it = vmMap.begin(); it != vmMap.end(); it++) {
		MeldProcessVM *vm = it->second;
		BuildingBlock *bb = vm->hostBlock;
		BlockCode* bbc = bb->blockCode;
		// Send id & set deterministic mode if necessary
		bbc->init();
		aliveBlocks += vm->sendCommand(c);	
	}
	return aliveBlocks;
}

int MeldProcessVM::sendCommand(int id, VMCommand &c) {
	BuildingBlock *bb = BaseSimulator::getWorld()->getBlockById(id);
	if (bb == NULL) {return 0;}
	MeldProcessVM *vm = getMeldProcessVMById(id);
	if (vm == NULL) {cout << "vm == NULL!" << endl; return 0;}
	BlockCode* bbc = bb->blockCode;
	bbc->init();
	cout << "sending command" << endl;
	return vm->sendCommand(c);
}
	
MeldProcessVM* MeldProcessVM::getMeldProcessVMById(int id) {
	map<int, MeldProcessVM*>::iterator it;
	it = vmMap.find(id);
	if (it == vmMap.end()) {
		return(NULL);
	} else {
		return(it->second);
	}
}
	
}
