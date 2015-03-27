
#include <boost/mpi/communicator.hpp>
#include <boost/mpi/environment.hpp>
#include <iostream>
#include <string>
#include <boost/serialization/string.hpp>
#include <stdlib.h>

namespace mpi = boost::mpi;

#define SENDNEXT 0
#define WAIT 1

using namespace std;

int nextProcess(mpi::communicator world){
    return (world.rank()+1)%world.size();
}

int prevProcess(mpi::communicator world){
    if (world.rank() == 0){
        return world.size()-1;
    } else {
        return world.rank()-1;
    }
}



int main(int argc, char* argv[])
{

    int L = atoi(argv[1]);
    int l = atoi(argv[2]);
    int rdvs = atoi(argv[3]);

    mpi::environment env(argc, argv);
    mpi::communicator world;

    //string msg1,msg2;
    //string str = "hello";

    int sum = 0;
    for (int i = 0; i < L; i++){
        for (int j = 0; j < l; j++){
            sum += j;
        }
        sum = 0;

	world.send(nextProcess(world), SENDNEXT);

	if (rdvs){
        	world.recv(prevProcess(world), SENDNEXT);
	}
    }

    if (!rdvs){
	while(world.iprobe(prevProcess(world),SENDNEXT)){
		world.recv(prevProcess(world),SENDNEXT);
	}
    }
    return 0;
}
