#ifndef API_H_
#define API_H_

/*
 * Header file for API
 */
#if defined(TARGET_mpi)
# include <boost/mpi.hpp>
#endif
#include <boost/serialization/binary_object.hpp>
#include <boost/asio.hpp>
#include <iostream>

#include "db/database.hpp"
#include "db/tuple.hpp"
#include "db/node.hpp"
#include "sched/base.hpp"
#include "vm/all.hpp"
#include "utils/types.hpp"
#include "queue/safe_general_pqueue.hpp"

namespace api {
  extern const char* apiTarget;

    /* Type representing the message between interprocess communications */
    typedef uint64_t message_type;
    static const size_t MAXLENGTH = 512 / sizeof(message_type);

#if defined(TARGET_mpi)
    extern boost::mpi::communicator *world;
#endif
    extern boost::asio::ip::tcp::socket *tcp_socket;

    /* Given a node destination, compute the process id that the node
     * belongs to, serialize the data for MPI and send the data
     */

    extern void sendMessage(const db::node* from, db::node::node_id to, db::simple_tuple* stpl);


    /* Check for any pending messages waiting to be received and add all of
     * the pending messages to the scheduler queue
     */
    extern bool pollAndProcess(sched::base *sched);
    int getNodeID(void);

    /*
     * Initialize and terminate the API layer
     */
    extern void init(int argc, char **argv, sched::base*);
    extern void end(void);
    extern bool ensembleFinished(void);

    /* Return whether or not the node with id `id` belongs to the current
     * VM
     */
    extern bool onLocalVM(const db::node::node_id id);

    /* Return the VM id that's responsible for the node id
     */
    extern int getVMId(const db::node::node_id id);

    extern void set_color(db::node *n, const int r, const int g, const int b);

    /* Serialize the execution of the system */
    extern void serializeBeginExec(void);
    extern void serializeEndExec(void);

    /* Output the database in a synchronized manner */
    extern void dumpDB(std::ostream &out, const db::database::map_nodes &nodes);
    extern void printDB(std::ostream &out, const db::database::map_nodes &nodes);
    extern bool hasToken;
    /* === Debugger Functions === */

    /* initialize the debugger through the api */
    extern void debugInit(void);

#ifdef INCLUDE_DEBUGGER
  // used by debugger to get the next process id (only mpi?)
  extern int nextProcessForDebugger(void);
#endif

    /*
     * send a massage to a specified node, if broadcast specified, send to all
     * nodes---msg is to be freed
     */
    extern void debugSendMsg(int dest,message_type* msg, size_t messageSize);
    extern void debugBroadcastMsg(message_type *msg, size_t messageSize);

    extern void debugWaitMsg(void);

    /*populate a debugger queue with incomming messages*/

    extern void set_color(db::node *n, const int r, const int g, const int b);
    extern void debugGetMsgs(void);
    
    
#ifdef SIMD
    /* Deterministic Simulation */
    extern void workEnd(void);
    extern void timeInfo(void);
    extern bool waitAndProcess(void);
#endif

	extern bool isInBBSimMode();
	extern void regularPollAndProcess(sched::base *sched);
    
} // namespace api
#endif
