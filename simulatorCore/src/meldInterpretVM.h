#ifndef MELDINTERPVM_H_
#define MELDINTERPVM_H_

//#include "bb.h"

#include <unistd.h>
#include <sys/types.h>
#include <stdint.h>
#include <sys/timeb.h>
#include <boost/shared_ptr.hpp>
#include <map>

#include "color.h"
#include "meldInterpretVMCore.h"
#include "blinkyBlocksBlock.h"


#include <sys/timeb.h>

/*
#ifdef assert
#undef assert
#endif
# define assert(e)	((e) ? (void)0 : __myassert(__FILE__, __LINE__, #e))
void __myassert(char* file, int line, char* exp);
*/

/******************************************************************************
@Description: MeldInterpVM is the VM's main file, it initializes the VM,
introduces and updates axioms, sends tuples to other blocks, and triggers
the execution of all tuples and rules through the scheduler loop.
*******************************************************************************/

namespace MeldInterpret {

class MeldInterpretVM;

class MeldInterpretVM : public MeldInterpretVMCore{
	private:
		/* Queue for tuples to send with delay */
		tuple_pqueue *delayedTuples;
		/* Contains a queueu for each type, this is essentially the database */
		tuple_queue *tuples;
		/* Where stratified tuples are enqueued for execution  */
		tuple_pqueue *newStratTuples;
		/* Where non-stratified tuples are enqueued for execution */
		tuple_queue *newTuples;
		/* Received tuples are enqueued both to a normal queue
		 * and to this one. Tuples store in this one will be used to remove
		 * remove tuples from the database.*/
		tuple_queue receivedTuples[NUM_PORTS];
		/* This block's ID */
		NodeID blockId;
		/* An array of 32 registers (pointers) */
		Register reg[32];

		NodeID neighbors[6];
		int numNeighbors;
		int waiting;
		uint64_t currentLocalDate;
		static bool debugging;
		static bool configured;
		bool hasWork, polling;
      public:
            static map<int, MeldInterpretVM*> vmMap;
            static const unsigned char* meld_prog;

	public:
            BlinkyBlocks::BlinkyBlocksBlock *host;

		MeldInterpretVM(BlinkyBlocks::BlinkyBlocksBlock *b);
		~MeldInterpretVM();
		void processOneRule();
		bool isWaiting();
		static bool dateHasBeenReachedByAll(uint64_t date);
		static bool equilibrium();
		inline static bool isInDebuggingMode() { return debugging; };
		static void setConfiguration(string path, bool d);
		static void readProgram(string path);

		byte updateRuleState(byte rid);
		Time myGetTime();
		inline NodeID get_neighbor_ID(int face);
		void enqueueNewTuple(tuple_t tuple, record_type isNew);
		void enqueue_face(NodeID neighbor, meld_int face, int isNew);
		void enqueue_count(meld_int count, int isNew);
		void enqueue_tap();
		void enqueue_init();
		void init_all_consts();
		void userRegistration();
		void receive_tuple(int isNew, tuple_t tpl, byte face);
		void free_chunk();
		void tuple_send(tuple_t tuple, NodeID rt, meld_int delay, int isNew);
		void tuple_handle(tuple_t tuple, int isNew, Register *registers);
		NodeID getBlockId();
		void vm_init();
		void vm_alloc();
		byte getNeighborCount();
		Uid down(void);
            Uid up(void);
            Uid north(void);
            Uid south(void);
            Uid east(void);
            Uid west(void);
            void setColor(Color color);
            void setLED(byte r, byte g, byte b, byte intensity);
            inline void setColorWrapper(Color color){
                  setColor(color);
            }
		inline void setLEDWrapper(byte r, byte g, byte b, byte intensity){
                  setLED(r, g, b, intensity);
		}
            NodeID getGUID();
};

}

#endif /* MELDINTERPVM_H_ */
