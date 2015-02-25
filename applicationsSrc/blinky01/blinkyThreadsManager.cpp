/*
 * blinkyThreadsManager.cpp
 *
 *  Created on: 27 mars 2013
 *      Author: dom
 */


#include <iostream>
#include <list>
#include <boost/thread.hpp>

using namespace std;
using namespace boost;

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

void threadFunction(void *data) {
	SimulatedVM *simulatedVM = (SimulatedVM*)data;
	cout << "thread started to handle VM " << simulatedVM->id << endl;
	cout << "end of thread handling VM " << simulatedVM->id << endl;
}

int SimulatedVM::nextId = 0;
list<SimulatedVM*> simulatedVMList;
list<thread*> threadsList;

int main(int argc, char **argv) {
	cout << "VMEmulator start" << endl;


	SimulatedVM *sVM;
	for (int i=0; i< 2; i++) {
		sVM = new SimulatedVM();
		simulatedVMList.push_back(sVM);
		threadsList.push_back(new thread(threadFunction, sVM));
	}

	list<thread*>::iterator it;
	for (it=threadsList.begin(); it != threadsList.end(); it++) {
		(*it)->join();
	}

	cout << "VMEmulator end" << endl;
}

