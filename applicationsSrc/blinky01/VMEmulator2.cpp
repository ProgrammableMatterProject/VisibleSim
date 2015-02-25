/*
 * VMEmulator.cpp
 *
 *  Created on: 12 mars 2013
 *      Author: dom
 */

#include <iostream>
#include <boost/asio.hpp>
#include <list>
#include <boost/thread.hpp>
#include <stdlib.h>
#include <time.h>
#include <stdio.h>

using namespace std;
using boost::asio::ip::tcp;


//#define SET_COLOR_EXAMPLE
//#define COLOR_SPREADING_EXAMPLE
//#define COLOR_ON_TAP_EXAMPLE
//#define NEIGHBOR_LIST_EXAMPLE
//#define MESSAGE_ANALYSER
#define DEBUGGER

#define VM_MESSAGE_SET_ID						1
#define VM_MESSAGE_STOP							4
#define VM_MESSAGE_ADD_NEIGHBOR					5
#define VM_MESSAGE_REMOVE_NEIGHBOR				6
#define VM_MESSAGE_TAP							7
#define VM_MESSAGE_SET_COLOR					8
#define VM_MESSAGE_SEND_MESSAGE					12
#define VM_MESSAGE_RECEIVE_MESSAGE				13
#define VM_MESSAGE_ACCEL						14
#define VM_MESSAGE_SHAKE						15
#define VM_MESSAGE_DEBUG						16

#define VM_MESSAGE_START_COMPUTATION 			20
#define VM_MESSAGE_END_COMPUTATION 				21

typedef struct VMMessage_tt {
    uint64_t size;
    uint64_t type;
	uint64_t timestamp;
	uint64_t sourcenode;
    uint64_t param1;
    uint64_t param2;
	uint64_t param3;
	uint64_t param4;
	uint64_t param5;
} VMMessage_t;

//enum NeighborDirection {Front=0, Back, Left, Right, Top, Bottom };
enum NeighborDirection { Bottom = 0, Back, Right, Left, Front, Top };
typedef pair<uint64_t, uint64_t> Neighbor;

string getStringMessage(uint64_t t) {
	switch(t) {
		case VM_MESSAGE_SET_ID:
			return string("VM_MESSAGE_SET_ID");
			break;
		case VM_MESSAGE_STOP:
			return string("VM_MESSAGE_STOP");
			break;
		case VM_MESSAGE_ADD_NEIGHBOR:
			return string("VM_MESSAGE_ADD_NEIGHBOR");
			break;
		case VM_MESSAGE_REMOVE_NEIGHBOR:
			return string("VM_MESSAGE_REMOVE_NEIGHBOR");
			break;
		case VM_MESSAGE_TAP:
			return string("VM_MESSAGE_TAP");
			break;
		case VM_MESSAGE_SET_COLOR:
			return string("VM_MESSAGE_SET_COLOR");
			break;
		case VM_MESSAGE_SEND_MESSAGE:
			return string("VM_MESSAGE_SEND_MESSAGE");
			break;
		case VM_MESSAGE_RECEIVE_MESSAGE:
			return string("VM_MESSAGE_RECEIVE_MESSAGE");
			break;
		case VM_MESSAGE_ACCEL:
			return string("VM_MESSAGE_ACCEL");
			break;
		case VM_MESSAGE_SHAKE:
			return string("VM_MESSAGE_SHAKE");
			break;
		case VM_MESSAGE_DEBUG:
			return string("VM_MESSAGE_DEBUG");
			break;
		case VM_MESSAGE_START_COMPUTATION:
			return string("VM_MESSAGE_START_COMPUTATION");
			break;
		case VM_MESSAGE_END_COMPUTATION:
			return string ("VM_MESSAGE_END_COMPUTATION");
		default:
			cerr << "Unknown Message" << endl;
			return string("Unknown");
			break;
		}
}

string getStringDirection(uint64_t d) {
	switch(d) {
		case Front:
			return string("Front");
			break;
		case Back:
			return string("Back");
			break;
		case Left:
			return string("Left");
			break;
		case Right:
			return string("Right");
			break;
		case Top:
			return string("Top");
			break;
		case Bottom:
			return string("Bottom");
			break;
		default:
			cerr << "Unknown direction" << endl;
			return string("Unknown");
			break;
		}
}

/*
class SimulatedVM {
protected:
//	static int nextId;
public:
	static int nextId;
	int id;
	SimulatedVM() {
		id = nextId;
		nextId++;
		cout << "created VM " << id << endl;
	}
	~SimulatedVM() {

	}
};

int SimulatedVM::nextId = 0;
list<SimulatedVM*> simulatedVMList;
list<boost::thread*> threadsList; */

int readMessageFromVM(tcp::socket &socket, VMMessage_t *buffer, int id) {
	try {
		boost::asio::read(socket,boost::asio::buffer((void*)&buffer->size, sizeof(uint64_t)));
		boost::asio::read(socket,boost::asio::buffer((void*)&buffer->type, buffer->size));
		if (id != -1) {
			cout << "VM " << id << " receive a message (" << getStringMessage(buffer->type) <<")" << endl;
		}
		return 1;
	} catch (std::exception& e) {
		cerr << "Connection to the Simulator lost" << endl;
		return 0;
	}
	return 1;
}

static int currentTime = 0;

void sendMessageThread(tcp::socket *s, int id) {
	VMMessage_t out;
	tcp::socket& socket = *s;
	out.size = 8*sizeof(uint64_t);
	out.type = VM_MESSAGE_SEND_MESSAGE;
	out.timestamp = currentTime;
	out.sourcenode = id;
	out.param1 = Right; // face: right
	out.param3 = 255; // green
	out.param4 = 0;
	out.param5 = 0;
	for (int j = 0; j < 2; j++) {
		for (int i = 1; i < 6; i++) {
			if (i == id) {
				continue;
			}
			out.param2 = i;
			try {
				boost::asio::write(socket, boost::asio::buffer((void*)&out,9*sizeof(uint64_t)));
				cout << "VM " << id << " sent message (color) on face right" <<  endl;
			} catch (std::exception& e) {
				cerr << "Connection to the Simulator lost" << endl;
			}
		}
		usleep(20000);
	}
}

void vm_thread_function(void *data) {	
	boost::asio::io_service ios;
	boost::asio::ip::tcp::resolver resolver(ios);
	boost::asio::ip::tcp::resolver::query query("127.0.0.1", "5000");
	boost::asio::ip::tcp::resolver::iterator iter = resolver.resolve(query);
	boost::asio::ip::tcp::resolver::iterator end;
	boost::asio::ip::tcp::endpoint endpoint;	
	boost::asio::ip::tcp::endpoint VMEndpoint;
	if (iter != end) {
		endpoint = *iter;
		std::cout << endpoint << std::endl;
	} else {
		exit(EXIT_FAILURE);
	}
	tcp::socket socket(ios);
	try {
		socket.connect(*iter);
		cout << "Connected to the Simulator" << endl;
	} catch (std::exception& e) {
		cerr << "Connection to the Simulator failed" << endl;
	}

	VMMessage_t in, out;
	int id, duration, endDate;
	cout << "VMEmulator start" << endl;
	if (readMessageFromVM(socket, &in, -1) == 1) {
		if (in.type == VM_MESSAGE_SET_ID) {
			id = in.sourcenode;
			cout << "VM received id: " << id << endl;
		} else {
			cout << "problem id not first message" << endl;
		}
	}
	currentTime++;
	//new boost::thread(boost::bind(sendMessageThread,&socket,id));
	while (readMessageFromVM(socket, &in, -1) == 1) {
		cout << getStringMessage(in.type) << " received " << endl;
		switch(in.type) {
			case VM_MESSAGE_START_COMPUTATION:
				duration = in.param1;
				endDate = currentTime + duration;
				out.size = 8*sizeof(uint64_t);
				out.type = VM_MESSAGE_SEND_MESSAGE;
				out.sourcenode = id;
				out.param1 = Right; // face: right
				out.param3 = 255; // green
				out.param4 = 0;
				out.param5 = 0;
				for (int j = 0; j < 2; j++) {
					for (int i = 1; i < 6; i++) {
						if (i == id) {
							continue;
						}
						out.param2 = i;
						out.timestamp = currentTime++;
						try {
							boost::asio::write(socket, boost::asio::buffer((void*)&out,9*sizeof(uint64_t)));
							cout << "VM " << id << " sent message (color) on face right" <<  endl;
						} catch (std::exception& e) {
							cerr << "Connection to the Simulator lost" << endl;
						}
					}
				}
				// SET COLOR
				out.size = 7*sizeof(uint64_t);
				out.type = VM_MESSAGE_SET_COLOR;
				out.param1 = 0;
				out.param2 = 0;
				out.param3 = 255;
				out.param4 = 0;
				try {
					boost::asio::write(socket, boost::asio::buffer((void*)&out,8*sizeof(uint64_t)));
					cout << "VM " << id << " sent SET_COLOR(after receiving a message)" <<  endl;
				} catch (std::exception& e) {
					cerr << "Connection to the Simulator lost" << endl;
				}
				// End Computation
				currentTime = std::max(currentTime, endDate+1);
				// next test can be to start a thread with a timer
				out.size = 4*sizeof(uint64_t);
				out.type = VM_MESSAGE_END_COMPUTATION;				
				out.timestamp =  currentTime;
				out.param1 =  duration;
				//usleep(20000); // 2ms
				try {
					boost::asio::write(socket, boost::asio::buffer((void*)&out,5*sizeof(uint64_t)));
					cout << "VM " << id << " sent END_COMPUTATION" <<  endl;
				} catch (std::exception& e) {
					cerr << "Connection to the Simulator lost" << endl;
				}
				break;
			default:
				break;
		}
				
	}
	//getchar();
	socket.close();
	cout << "VMEmulator "<< id << " end" << endl;

}

/*
void threadFunction(void *data) {
	SimulatedVM *simulatedVM = (SimulatedVM*)data;
	cout << "thread started to handle VM " << simulatedVM->id << endl;
	vm_thread_function(NULL);
	cout << "end of thread handling VM " << simulatedVM->id << endl;
}
*/

int main(int argc, char **argv) {
	vm_thread_function(NULL);
	/*SimulatedVM *sVM = new SimulatedVM();
	simulatedVMList.push_back(sVM);
	threadFunction(sVM);	
	cout << "VMEmulator start" << endl;

	SimulatedVM *sVM;
	for (int i = 0; i < 2; i++) {
		sVM = new SimulatedVM();
		simulatedVMList.push_back(sVM);
		threadsList.push_back(new boost::thread(threadFunction, sVM));
	}
	list<boost::thread*>::iterator it;
	for (it=threadsList.begin(); it != threadsList.end(); it++) {
		(*it)->join();
	}
	cout << "VMEmulator end" << endl; */
}

