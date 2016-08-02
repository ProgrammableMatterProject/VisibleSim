/*
  The MPI communication API
  =========================
  @author: Xing Zhou
  @email: xingz@andrew.cmu.edu
  @date: 13 July 2013

  @library: boost::mpi v.1.54.0

  All nodes in the database is partitioned to each VM according to specific
  policy.

  Ensemble termination uses Dijkstra and Safra's token ring termination
  detection.
*/

#include <vector>
#include <utility>
#include <boost/mpi/collectives.hpp>
#include <sstream>
#include "utils/types.hpp"
#include "vm/predicate.hpp"
#include "process/machine.hpp"
#include "debug/debug_handler.hpp"
#include "debug/debug_prompt.hpp"
#include "utils/serialization.hpp"
#include "api/api.hpp"
#include "debug/debug_handler.hpp"
#include "compileInfo.hpp"

using namespace std;
namespace mpi = boost::mpi;

declareCompileInfo(targetArchitecture, "mpi");

namespace api {

const char* apiTarget = "mpi";

    // Function Prototypes
    static void freeSendMsgs(void);
    static void processRecvMsgs(sched::base *sched);
    static string _printNode(const db::node::node_id, const db::database::map_nodes&);
    static string _dumpNode(const db::node::node_id, const db::database::map_nodes&);
    static void serialDBOutput(std::ostream &out, const db::database::map_nodes &nodes,
                               string (*format)(
                                   const db::node::node_id,
                                   const db::database::map_nodes&
                                   ));

    static void freeDebugSendMsgs(void);

     // token tags to tag messages in MPI
    enum tokens {
        DEBUG,
        TERM,
        BLACK,
        WHITE,
        DONE,
        TUPLE,
        EXEC,
        INIT,
        PRINT,
        PRINT_DONE
    };

    static int RING_ORIGIN = 0;

    /* Functions to calculate adjacent process ids in a ring structure */
    inline int prevProcess(void) {

        /*if in debugging mode skip over the debug master*/
        if (world->rank() == RING_ORIGIN)
            return world->size() - 1;
        else
            return world->rank() - 1;


    }

    inline int nextProcess(void) {
        /*note- skips over master debugging process in
          debugging mode*/
        if (world->rank() == world->size()-1)
            return RING_ORIGIN;
        else
            return world->rank() + 1;
    }

    // Global MPI variables
    mpi::communicator *world;
    static mpi::environment *env;

    // Vectors to hold created messages in order to free them after the
    // asynchronous MPI calls are complete
    static vector<pair<mpi::request, message_type *> > sendMsgs, recvMsgs,
        debugSendMsgs, debugRecvMsgs;

    // Dijkstra and Safra's token ring termination detection algorithm
    // Default states
    static int counter = 0;
    static int color = WHITE;
    static bool tokenSent = false;
    bool hasToken = false;

    void init(int argc, char **argv, sched::base *sched) {
        /* Initialize the MPI.

           In order to accomodate the bbsimapi implementation, init needs to be
           called multiples times. The value of sched, whether NULL or non-NULL
           works as a switch so that the MPI init is only run once at most.
        */
        if (sched == NULL) {
            env = new mpi::environment(argc, argv);
            world = new mpi::communicator();
            if (debugger::isInMpiDebuggingMode()) {
                /*api master defined in debug_handler.hpp*/
                RING_ORIGIN = api::MASTER;
            }
            if (world->rank() == RING_ORIGIN)
                hasToken = true;
        }
    }

    void end(void) {
        delete env;
        delete world;
    }

    void sendMessage(const db::node* from, const db::node::node_id to,
                      db::simple_tuple* stpl) {
        /* Given a node id and tuple, figure out the process id to send the
           tuple and id to be processed
        */
      ignoreUnusedParamWarning(from);

        int dest = getVMId(to);
        message_type *msg = new message_type[MAXLENGTH];
        size_t msg_length = MAXLENGTH * sizeof(message_type);
        int p = 0;

        utils::pack<message_type>((void *) &to, 1, (utils::byte *) msg,
                                  msg_length, &p);
        stpl->pack((utils::byte *) msg, msg_length - sizeof(message_type), &p);

        mpi::request req = world->isend(dest, TUPLE, msg, MAXLENGTH);
        sendMsgs.push_back(make_pair(req, msg));
        freeSendMsgs();
    }

    void freeSendMsgs(void) {
        /* free messages in the send messages collection if appropriate
           ================================================================

           since the mpi functions are asynchronous, we cannot assume that the
           messages buffer created can be removed as soon as the sents calls are
           executed.

           Instead, the messages are queued up in the sendMsgs vector along with
           their mpi status. Then this function is called periodically in order
           to free the messages where their mpi status indicates as complete.
        */
        for (vector<pair<mpi::request, message_type*> >::iterator it = sendMsgs.begin();
             it != sendMsgs.end(); ) {
            mpi::request req(it->first);
            if (req.test()) {
                delete[] (it->second);
                it = sendMsgs.erase(it);

                // Safra's algorithm: SEND MESSAGE
                counter++;
            } else
                ++it;
        }
    }

    bool pollAndProcess(sched::base *sched) {
        /* Call the mpi asking for messages in the receive queue.  The messages
           are handled appropriately.
           ====================================================================

           Receive the message if there is a message waiting to be received,
           deserialize the message, and then add the message to the
           scheduler's queue as new work.

           Returns true if the system cannot be determined to have terminated.
           Otherwise, returns false if the system has terminated.
        */
        freeSendMsgs();

        bool newMessage = false;

        while (world->iprobe(mpi::any_source, TUPLE)) {
            /* a meld message that needs to be processed by scheduler
               Since the MPI is asynchronous, the messages are queued and
               then processed outside of the loop.
            */
            message_type *msg = new message_type[MAXLENGTH];
            mpi::request req = world->irecv(mpi::any_source, TUPLE,
                                            msg, MAXLENGTH);
            recvMsgs.push_back(make_pair(req, msg));
            newMessage = true;
        }

        processRecvMsgs(sched);
        return newMessage;
    }

    void processRecvMsgs(sched::base *sched) {
        /*
          Process the receive message queue and free the messages appropriatedly
        */
        for (vector<pair<mpi::request, message_type *> > ::iterator
                 it=recvMsgs.begin(); it != recvMsgs.end(); ) {
            mpi::request req = it->first;
            if (req.test()) {
                // Communication is complete, can safely extract tuple
                message_type *msg = it->second;
                size_t msg_length = MAXLENGTH * sizeof(message_type);
                int pos = 0;
                message_type nodeID;

                utils::unpack<message_type>((utils::byte *) msg, msg_length,
                                            &pos, &nodeID, 1);
                db::node::node_id id = nodeID;

                db::simple_tuple *stpl = db::simple_tuple::unpack(
                    (utils::byte *) msg, msg_length - sizeof(message_type),
                    &pos, vm::All->PROGRAM);

                vm::All->MACHINE->route(NULL, sched, id, stpl, 0);

                // Safra's Algorithm for RECEIVE MESSAGE
                counter--;
                color = BLACK;

                // Free the messages
                delete[] msg;
                it = recvMsgs.erase(it);
            } else
                ++it;
        }
    }

    bool ensembleFinished() {
        /*
          Handle tokens sent using Safra's algorithm for termination detection
          ref: http://www.cse.ohio-state.edu/siefast/group/publications/da2000-otdar.pdf
          page: 3
        */
        uint dest = nextProcess();

        if (world->rank() == RING_ORIGIN && !tokenSent) {
            /* Safra's Algorithm: Begin Token Collection  */
            world->isend(dest, WHITE, 0);
            tokenSent = true;
            return false;
        }

        /* Safra's Algorithm: token accumulator */
        int acc, tokenColor;
        bool done = false;

        if (world->iprobe(mpi::any_source, WHITE)) {
            world->irecv(mpi::any_source, WHITE, acc);
            tokenColor = WHITE;
        } else if (world->iprobe(mpi::any_source, BLACK)) {
            world->irecv(mpi::any_source, BLACK, acc);
            tokenColor = BLACK;
        } else if (world->iprobe(mpi::any_source, DONE)) {
            world->irecv(mpi::any_source, DONE);
            done = true;
        } else {
            // No token in progress
            return false;
        }
        if (world->rank() == RING_ORIGIN) {
            if (tokenColor == WHITE && color == WHITE && acc + counter == 0) {
                // System Termination detected, notify other processes
                if (debugger::isInMpiDebuggingMode()) {
                    // end of the ring
                    debugger::sendMsg(debugger::MASTER, debugger::TERMINATE, "");
                    while(!sendMsgs.empty()) {
                        freeSendMsgs();
                    }
                }
                world->isend(dest, DONE);
                return true;
            }
            /* Safra's Algorithm: RETRANSMIT TOKEN */
            world->isend(dest, WHITE, 0);
            color = WHITE;
        } else {
            if (done) {
                world->isend(dest, DONE);
                return true;
            }

            /* Safra's Algorithm: PROPAGATE TOKEN */
            if (color == BLACK)
                tokenColor = BLACK;
            color = WHITE;
            world->isend(dest, tokenColor, acc + counter);
        }
        return false;
    }

    void serializeBeginExec(void) {
        /*
          Use a token ring algorithm to serialize execution from process to
          process

          There should be only 1 EXEC token passed around the ring. Only the
          process holding the token can execute, every other process must wait.
        */

        int source = prevProcess();
        if (!hasToken) {
            // Block process from continuing until token is received
            world->recv(source, EXEC);
            hasToken = true;
        }

    }

    void serializeEndExec(void) {
        int dest = nextProcess();
        if (hasToken) {
            hasToken = false;
            world->send(dest, EXEC);
        }
    }

    int getNodeID(void){
        return world->rank();
    }

    bool onLocalVM(const db::node::node_id id) {
        return getVMId(id) == world->rank();
    }

    int getVMId(const db::node::node_id id) {
        /* POLICY SPECIFIC according to the value of debugger::MASTER */
        if (debugger::isInMpiDebuggingMode()) {
          return (id % (world->size() - 1)) + 1;
        }
        return id % world->size();
    }

    /* === Synced Database Output Functions === */

    void dumpDB(std::ostream &out, const db::database::map_nodes &nodes) {
        serialDBOutput(out, nodes, &_dumpNode);
    }

    void printDB(std::ostream& out, const db::database::map_nodes &nodes) {
        serialDBOutput(out, nodes, &_printNode);
    }

    string _dumpNode(const db::node::node_id id,
                      const db::database::map_nodes &nodes) {
        ostringstream os;
        nodes.at(id)->dump(os);
        return os.str();
    }

    string _printNode(const db::node::node_id id,
                      const db::database::map_nodes &nodes) {
        ostringstream os;
        os << "[VM_ID: " << world->rank() << "]" << *(nodes.at(id));
        return os.str();
    }

    void serialDBOutput(std::ostream &out, const db::database::map_nodes &nodes,
                        string (*format)(const db::node::node_id,
                                         const db::database::map_nodes&)) {
        /* Serialize the database output for increasing internal node id.  How
         * it is output is determined by the format function */

        if (!debugger::isInMpiDebuggingMode()) {
            world->barrier();
        }

        int source = prevProcess();
        int dest = nextProcess();

        if (world->rank() == RING_ORIGIN) {
            for (db::database::map_nodes::const_iterator it(nodes.begin());
                 it != nodes.end(); ++it) {
                db::node::node_id id = it->first;
                if (onLocalVM(id)) {
                    out << format(id, nodes);
                } else {
                    world->send(getVMId(id), PRINT, id);
                    string result;
                    world->recv(getVMId(id), PRINT, result);
                    out << result;
                }
            }
            // Finished printing, singal done
            world->isend(dest, PRINT_DONE);
        } else {
            while(true) {
                mpi::status status = world->probe(mpi::any_source,
                                                  mpi::any_tag);
                if (status.tag() == PRINT_DONE) {
                    world->irecv(source, PRINT_DONE);
                    world->isend(dest, PRINT_DONE);
                    break;
                }
                db::node::node_id id;
                world->recv(RING_ORIGIN, PRINT, id);
                world->send(RING_ORIGIN, PRINT, format(id, nodes));
            }
        }
    }

    /* === Debugger Functions === */

    void debugInit() {
        /* Use MPI gather to make sure that all VMs are initiated before the
         * debugger is run. Debugger MASTER is the debugger process, while MPI
         * RING_ORIGIN is the initial process in a ring structure.
         */
        if (world->size() == 1) {
            throw "Debug must be run with at least 2 MPI processes.";
        }
        debugger::initMpiDebug();
        if (world->rank() == debugger::MASTER) {
            cout << "MPI DEBUGGING MODE -- type 'help' for options" << endl;
            std::vector<tokens> allVMs;
            mpi::gather(*world, INIT, allVMs, debugger::MASTER);
            debugger::run(NULL);
        } else {
            mpi::gather(*world, INIT, debugger::MASTER);
            debugger::pauseIt();
        }
    }

    void debugBroadcastMsg(message_type *msg, size_t msgSize) {

        message_type *temp;

        for (int dest = 1; dest < world->size(); ++dest) {

            temp = (message_type*)new utils::byte[msgSize];

            memcpy(temp,msg,msgSize);

            mpi::request req = world->isend(dest, DEBUG, msg, msgSize);
            debugSendMsgs.push_back(make_pair(req, temp));
        }
        delete[] msg;
        freeDebugSendMsgs();
    }

    void freeDebugSendMsgs(void) {
        for (vector<pair<mpi::request,message_type*> >::iterator it = debugSendMsgs.begin();
             it != debugSendMsgs.end(); ) {

            mpi::request req(it->first);
            if (req.test()) {
                delete[] (it->second);
                it = debugSendMsgs.erase(it);
            } else
                ++it;
        }
    }

    void debugSendMsg(int destination, message_type *msg,
                      size_t msgSize) {

        int dest;

        if (destination == debugger::MASTER){
            dest = debugger::MASTER;
        } else {
	  dest = getVMId(vm::All->DATABASE->translate_real_to_fake_id(
                                         (db::node::node_id) destination));
        }


        /* Send the message through MPI and place the message and status into
           the sendMsgs vector to be freed when the request completes */
        mpi::request req = world->isend(dest, DEBUG, msg, msgSize);
        debugSendMsgs.push_back(make_pair(req, msg));
        freeDebugSendMsgs();
    }

    void debugWaitMsg(void) {
        world->probe(mpi::any_source, DEBUG);
    }

    void debugGetMsgs(void) {
        /* Poll the MPI to find any debug messages and populate the debug
         * message queue */

        while(world->iprobe(mpi::any_source, DEBUG)) {
            message_type *msg = new message_type[MAXLENGTH];
            mpi::request req = world->irecv(mpi::any_source, DEBUG, msg, MAXLENGTH);
            debugRecvMsgs.push_back(make_pair(req, msg));
        }

        // Iterate through the message queue and add the messages with completed
        // requests to the debugger's message queue
        for (vector<pair<mpi::request, message_type*> >::iterator it = debugRecvMsgs.begin();
             it != debugRecvMsgs.end(); ) {
            mpi::request req(it->first);
            if (req.test()) {
                /*left to debugger to free the message*/
                debugger::messageQueue->push(it->second);
                it = debugRecvMsgs.erase(it);
            } else {
                ++it;
            }
        }
    }


#if defined(INCLUDE_DEBUGGER)
  /*NEXT PROCESS --when in Mpi debugging mode, find the next process*/
  int nextProcessForDebugger(void) {
    /*note- skips over master debugging process in
      debugging mode*/

    if (world->rank() == world->size()-1)
      return 1;
    else
      return world->rank() + 1;
  }
#endif


/*
 * Unimplemented functions in MPI
 */
    void set_color(db::node *n, const int r, const int g, const int b) {
      ignoreUnusedParamWarning(n);
      ignoreUnusedParamWarning(r);
      ignoreUnusedParamWarning(g);
      ignoreUnusedParamWarning(b);
    }
    /* Deterministic Simulation */
    void workEnd() {}
    void timeInfo() {}
    
    /* Thanks to isInBBSimMode, waitAndProcess is not called */
    bool isInBBSimMode() { return false; }
    bool waitAndProcess() { return false; }
	
    void regularPollAndProcess(sched::base *sched) {
		static uint i = 1;
		if ( (i%10) == 0) {
			pollAndProcess(sched); 
		}
		i++;
	}

} /* namespace api */
