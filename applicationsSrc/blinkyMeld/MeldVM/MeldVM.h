#ifndef MELDVM_H_
#define MELDVM_H_

//#include "bb.h"
#include "../system/led.bbh"
#include "../system/ensemble.bbh"
#include "../system/accelerometer.bbh"
#include "../system/message.bbh"
#include "../hw-api/hwMemory.h"
#include "../system/myassert.h"

#ifdef BBSIM
#include <unistd.h>
#include <sys/types.h>
#else
#endif

#ifdef LOG_DEBUG
#include "../system/log.bbh"
#endif

#include "../system/defs.bbh"

//#include "util.h"

#include "core.h"
#include "api.h"
// include here to make sure we include the threadypes, etc.
#include "model.bbh"

#ifdef BBSIM
#include <sys/timeb.h>
#endif

/******************************************************************************
@Description: MeldVM is the VM's main file, it initializes the VM,
introduces and updates axioms, sends tuples to other blocks, and triggers
the execution of all tuples and rules through the scheduler loop.
*******************************************************************************/


class MeldVM {
	private:
		/* Queue for tuples to send with delay */
		threadvar tuple_pqueue *delayedTuples; 
		/* Contains a queueu for each type, this is essentially the database */
		threadvar tuple_queue *tuples;	      
		/* Where stratified tuples are enqueued for execution  */
		threadvar tuple_pqueue *newStratTuples;
		/* Where non-stratified tuples are enqueued for execution */
		threadvar tuple_queue *newTuples;
		/* Received tuples are enqueued both to a normal queue
		 * and to this one. Tuples store in this one will be used to remove 
		 * remove tuples from the database.*/
		threadvar tuple_queue receivedTuples[NUM_PORTS];
		/* This block's ID */
		threadvar NodeID blockId;
		/* An array of 32 registers (pointers) */
		threadvar Register reg[32];
		/* thr\dvar byte retractionOccured = 0; */
		/* thr\dvar byte inNeighborCountUpdate = 0; */
		#ifdef BBSIM
		pthread_mutex_t printMutex;
		#endif
 	
	public:
		MeldVM();
		~MeldVM();
		byte updateRuleState(byte rid);
		Time myGetTime();
		void print_newTuples();
		void print_newStratTuples();
		inline NodeID get_neighbor_ID(int face);
		void enqueueNewTuple(tuple_t tuple, record_type isNew);
		void enqueue_face(NodeID neighbor, meld_int face, int isNew);
		void enqueue_count(meld_int count, int isNew);
		void enqueue_tap();
		void enqueue_init();
		void init_all_consts();
		void userRegistration();
		#ifdef _LP64
		void endian_swap(Register* x);
		#endif
		void receive_tuple(int isNew);
		void receive_tuple_delete();
		void receive_tuple_add();
		void free_chunk();
		void tuple_send(tuple_t tuple, NodeID rt, meld_int delay, int isNew);
		byte updateRuleState(byte rid);
		void tuple_handle(tuple_t tuple, int isNew, Register *registers);
		void setColorWrapper(byte color);
		void setLEDWrapper(byte r, byte g, byte b, byte intensity);
		NodeID getBlockId();
		#ifdef BBSIM
		int alreadyExecuted(int flag);
		#endif
		void vm_init();
		void vm_alloc();
		void __myassert(char* file, int line, char* exp);
}

#endif /* MELD_H_ */