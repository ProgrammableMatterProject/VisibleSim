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

#define VM_MESSAGE_SET_ID			1
#define VM_MESSAGE_STOP				4
#define VM_MESSAGE_ADD_NEIGHBOR		5
#define VM_MESSAGE_REMOVE_NEIGHBOR	6
#define VM_MESSAGE_TAP				7
#define VM_MESSAGE_SET_COLOR		8
#define VM_MESSAGE_SEND_MESSAGE		12
#define VM_MESSAGE_RECEIVE_MESSAGE	13
#define VM_MESSAGE_ACCEL			14
#define VM_MESSAGE_SHAKE			15
#define VM_MESSAGE_DEBUG			16			

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

	VMMessage_t in;
	int id;	
	cout << "VMEmulator start" << endl;
	if (readMessageFromVM(socket, &in, -1) == 1) {
		if (in.type == VM_MESSAGE_SET_ID) {
			id = in.param1; // CHANGE IN THE FORMAT...
			cout << "VM received id: " << id << endl;
		} else {
			cout << "problem id not first message" << endl;
		}
	}
#ifdef SET_COLOR_EXAMPLE
	out.size = 7*sizeof(uint64_t);
	out.type = VM_MESSAGE_SET_COLOR;
	out.param1 = 0;
	out.param2 = 0;
	out.param3 = 255; // blue
	out.param4 = 0;
	try {
		boost::asio::write(socket, boost::asio::buffer((void*)&out,8*sizeof(uint64_t)));
		cout << "VM " << id << " sent SET_COLOR(blue)" <<  endl;
	} catch (std::exception& e) {
		cerr << "Connection to the Simulator lost" << endl;
	}
	//getchar();
#endif
#ifdef COLOR_SPREADING_EXAMPLE
	// block 1 sends the color to the block on the right
	if (id == 1) {
		out.size = 8*sizeof(uint64_t);
		out.type = VM_MESSAGE_SEND_MESSAGE;
		out.param1 = Right; // face: right
		out.param2 = 0;
		out.param3 = 255; // green
		out.param4 = 0;
		out.param5 = 0;
		try {
			boost::asio::write(socket, boost::asio::buffer((void*)&out,9*sizeof(uint64_t)));
			cout << "VM " << id << " sent message (color) on face right" <<  endl;
		} catch (std::exception& e) {
			cerr << "Connection to the Simulator lost" << endl;
		}
		// SET COLOR
		out.size = 7*sizeof(uint64_t);
		out.type = VM_MESSAGE_SET_COLOR;
		out.param1 = 0;
		out.param2 = 255;
		out.param3 = 0;
		out.param4 = 0;
		try {
			boost::asio::write(socket, boost::asio::buffer((void*)&out,8*sizeof(uint64_t)));
			cout << "VM " << id << " sent SET_COLOR(after receiving a message)" <<  endl;
		} catch (std::exception& e) {
			cerr << "Connection to the Simulator lost" << endl;
		} sleep(10);
	} else {
		while (true) {
			// RECEIVE MESSAGE & SET COLOR
			if(readMessageFromVM(socket, &in, id) == 0) {cout << "VM " << id << " break " << endl; break;}
			if(in.type == VM_MESSAGE_RECEIVE_MESSAGE) {
				out.size = 7*sizeof(uint64_t);
				out.type = VM_MESSAGE_SET_COLOR;
				out.param1 = in.param2;
				out.param2 = in.param3;
				out.param3 = in.param4;
				out.param4 = in.param5;
				try {
					boost::asio::write(socket, boost::asio::buffer((void*)&out,8*sizeof(uint64_t)));
					cout << "VM " << id << " sent SET_COLOR(after receiving a message)" <<  endl;
				} catch (std::exception& e) {
					cerr << "Connection to the Simulator lost" << endl;
				}
				if (id != 6) {
					// SEND MESSAGE ON RIGHT		
					out.size = 8*sizeof(uint64_t);
					out.type = VM_MESSAGE_SEND_MESSAGE;
					out.param1 = Right; // face: right
					out.param2 = in.param2;
					out.param3 = in.param3; // green
					out.param4 = in.param4;
					out.param5 = in.param5;
					try {
						boost::asio::write(socket, boost::asio::buffer((void*)&out,9*sizeof(uint64_t)));
						cout << "VM " << id << " sent SET_COLOR(after receiving a message)" <<  endl;
					} catch (std::exception& e) {
						cerr << "Connection to the Simulator lost" << endl;
					}
				}
				break;		
			}
		}
	}
#endif
#ifdef COLOR_ON_TAP_EXAMPLE
	while(true) {
		if (!readMessageFromVM(socket, &in, id)) {cout << "out" << endl; break;}
		if (in.type == VM_MESSAGE_TAP) {
			out.size = 7*sizeof(uint64_t);
			out.type = VM_MESSAGE_SET_COLOR;
			out.param1 = rand() % 256;
			out.param2 = rand() % 256;
			out.param3 = rand() % 256;
			out.param4 = 0;
			try {
				boost::asio::write(socket, boost::asio::buffer((void*)&out,8*sizeof(uint64_t)));
				cout << "VM " << id << " sent SET_COLOR(random)" <<  endl;
			} catch (std::exception& e) {
				cerr << "Connection to the Simulator lost" << endl;
				break;
			}
		}
	}
#endif
#ifdef NEIGHBOR_LIST_EXAMPLE
	//std::list<Neighbor> n;
	map<uint64_t, uint64_t> n;
	while(true) {
		if (!readMessageFromVM(socket, &in, id)) {break;}
		if (in.type == VM_MESSAGE_ADD_NEIGHBOR) {
			cout << "VM "<< id << " has a new neighbor: " << in.param1 << " on " << getStringDirection(in.param2) << " face" << endl;
			n.insert(Neighbor(in.param2, in.param1));
		} else if (in.type == VM_MESSAGE_REMOVE_NEIGHBOR) {
			cout << "VM " << id << " has no more neighbor on face " << getStringDirection(in.param1) << endl;
			map<uint64_t, uint64_t>::iterator it;
			it = n.find(in.param1);
			//remove
			n.erase(it);
		}
		// Display neighbor list
		map<uint64_t, uint64_t>::iterator it;
		stringstream ss;
		ss << "VM "<< id << "'s neighbors: ";
		for (it = n.begin(); it != n.end(); it++) {
			 ss << it->second << "(" << getStringDirection(it->first) << "), ";
		}
		cout << ss.str() << endl;
	}
#endif
#ifdef MESSAGE_ANALYSER
	while(true) {
		if (!readMessageFromVM(socket, &in, id)) break;
	}
#endif
#ifdef DEBUGGER
	while(true) {
		if (!readMessageFromVM(socket, &in, id)) break;
		if (in.type == VM_MESSAGE_DEBUG) {
			uint64_t m[4];
			m[0] = 3*sizeof(uint64_t);
			m[1] = VM_MESSAGE_DEBUG;
			m[2] = 5;
			char* s = (char*) &m[3];
			sprintf(s, "hello\n");
			//m[4] = 0x00484948656C6F;
			try {
				boost::asio::write(socket, boost::asio::buffer((void*)&m,4*sizeof(uint64_t)));
				cout << "VM " << id << " sent PRINT" <<  endl;
			} catch (std::exception& e) {
				cerr << "Connection to the Simulator lost" << endl;
				break;
			}
		}
	}
#endif
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

