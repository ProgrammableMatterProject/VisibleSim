#include "blinkyBlocksVM.h"
#include "blinkyBlocksBlock.h"
#include "blinkyBlocksBlockCode.h"
#include <boost/asio/io_service.hpp>
#include <sys/wait.h>
#include <stdio.h>
#include <boost/bind.hpp>
#include "trace.h"
#include <stdexcept>
#include <string.h>
#include "events.h"
#include "blinkyBlocksEvents.h"
#include "openglViewer.h"

#include <boost/bind.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/enable_shared_from_this.hpp>

using namespace boost;
using asio::ip::tcp;

namespace BlinkyBlocks {

boost::asio::io_service *BlinkyBlocksVM::ios = NULL;
boost::interprocess::interprocess_mutex BlinkyBlocksVM::mutex_ios;
tcp::acceptor *BlinkyBlocksVM::acceptor = NULL;
string BlinkyBlocksVM::vmPath;
string BlinkyBlocksVM::programPath;
bool BlinkyBlocksVM::debugging = false;

BlinkyBlocksVM::BlinkyBlocksVM(BlinkyBlocksBlock* bb){
   int ret;
   boost::system::error_code error;
   stringstream vmLogFile;

	assert(ios != NULL && acceptor != NULL);
	hostBlock = bb;
   vmLogFile << "VM" << hostBlock->blockId << ".log";
   
	OUTPUT << "VM "<< hostBlock->blockId << " constructor" << endl;
	// Start the VM
	pid = 0;
   mutex_ios.lock();
	ios->notify_fork(boost::asio::io_service::fork_prepare);
	pid = fork();
	if(pid < 0) {ERRPUT << "Error when starting the VM" << endl;}
    if(pid == 0) {
		ios->notify_fork(boost::asio::io_service::fork_child);
      mutex_ios.unlock();
      acceptor->close();
      getWorld()->closeAllSockets();
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
	socket = boost::shared_ptr<tcp::socket>(new tcp::socket(*ios));
   if (hostBlock->blockId == 1) {
      bool connected = false;
      
      acceptor->async_accept(*(socket.get()), boost::bind(&BlinkyBlocksVM::asyncAcceptHandler, this, error , &connected));
      while(!connected &&  (pid != waitpid(pid, NULL, WNOHANG))) {
         	if (!BlinkyBlocksVM::isInDebuggingMode()) { // In debugging mode the scheduler thread is looking for connections
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
}

void BlinkyBlocksVM::asyncAcceptHandler(boost::system::error_code& error, bool* connected) {
 if (error) {
   *connected = false;
 } else {
   *connected = true;
 }
}

BlinkyBlocksVM::~BlinkyBlocksVM() {
	closeSocket();
	killProcess();
}

void BlinkyBlocksVM::terminate() {
	waitpid(pid, NULL, 0);
}

void BlinkyBlocksVM::killProcess() {
	kill(pid, SIGTERM);
	waitpid(pid, NULL, 0);
}

void BlinkyBlocksVM::closeSocket() {
	if (socket != NULL) {
		socket->cancel();
		socket->close();
		socket.reset();
	}
}

void BlinkyBlocksVM::asyncReadCommandHandler(const boost::system::error_code& error, std::size_t bytes_transferred) {
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

void BlinkyBlocksVM::handleInBuffer() {
	BlinkyBlocksBlockCode *bbc = (BlinkyBlocksBlockCode*)hostBlock->blockCode;
	VMCommand command(inBuffer);
	bbc->handleCommand(command);
}

void BlinkyBlocksVM::asyncReadCommand() {
	if (socket == NULL) {
		ERRPUT << "Simulator is not connected to the VM "<< hostBlock->blockId << endl;
		return;
	}
	try {
	inBuffer[0] = 0;
	boost::asio::async_read(getSocket(),
		boost::asio::buffer(inBuffer, sizeof(commandType)),
		boost::bind(&BlinkyBlocksVM::asyncReadCommandHandler, this, boost::asio::placeholders::error,
		boost::asio::placeholders::bytes_transferred));
	} catch (std::exception& e) {
		ERRPUT << "Connection to the VM "<< hostBlock->blockId << " lost" << endl;
	}
}

int BlinkyBlocksVM::sendCommand(VMCommand &command){
	if (socket == NULL) {
		ERRPUT << "Simulator is not connected to the VM "<< hostBlock->blockId << endl;
		return 0;
	}
	if (command.getType() != VM_COMMAND_DEBUG) {
		nbSentCommands++;
		((BlinkyBlocksBlockCode*)hostBlock->blockCode)->handleDeterministicMode(command);
	}
	try {
		boost::asio::write(getSocket(), boost::asio::buffer(command.getData(), command.getSize()));
      //boost::asio::async_write(getSocket(), boost::asio::buffer(command.getData(), command.getSize()), boost::bind(&BlinkyBlocksVM::handle_write, this,
      //      boost::asio::placeholders::error));
   } catch (std::exception& e) {
		ERRPUT << "Connection to the VM "<< hostBlock->blockId << " lost" << endl;
		return 0;
	}
	return 1;
}


  void BlinkyBlocksVM::handle_write(const boost::system::error_code& error)
  {
    if (!error)
    {
     cout << "ok" << endl;
    }
  }

void BlinkyBlocksVM::checkForReceivedCommands() {
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

void BlinkyBlocksVM::waitForOneCommand() {
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

void BlinkyBlocksVM::setConfiguration(string v, string p, bool d) {
	vmPath = v;
	programPath = p;
	debugging = d;
}

void BlinkyBlocksVM::createServer(int p) {
	assert(ios == NULL);
	ios = new boost::asio::io_service();
	acceptor =  new tcp::acceptor(*ios, tcp::endpoint(tcp::v4(), p));
}

void BlinkyBlocksVM::deleteServer() {
	ios->stop();
	delete acceptor;
	delete ios;
	ios = NULL; acceptor = NULL;
}

}
