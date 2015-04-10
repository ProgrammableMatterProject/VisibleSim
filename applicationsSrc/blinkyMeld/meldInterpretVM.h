#ifndef MELDINTERPVM_H_
#define MELDINTERPVM_H_

//#include "bb.h"

#include <unistd.h>
#include <sys/types.h>
#include <stdint.h>

#include "MeldInterpretVMCore.h"
#include "blinkyMeldBlockCode.h"
#include "blinkyBlocksBlock.h"
#include "color.h"

#ifdef BBSIM
#include <sys/timeb.h>
#endif

/******************************************************************************
@Description: api.h contains useful macros for casting or dereferencing
meld values.
*******************************************************************************/

typedef Register meld_value;

#define NODE_FORMAT "%u"

#define MELD_INT(x)   (*(meld_int *)(x))
#define MELD_FLOAT(x) (*(meld_float *)(x))
#define MELD_NODE(x)  (*(Node **)(x))
#define MELD_NODE_ID(x) (*(NodeID *)(x))
#define MELD_SET(x) (*(Set **)(x))
#define MELD_LIST(x)  (*(List **)(x))
#define MELD_PTR(x)	  (*(void **)(x))
#define MELD_BOOL(x)  (*(unsigned char*)(x))

#define MELD_CONVERT_INT(x)   (*(Register *)(meld_int *)&(x))
#define MELD_CONVERT_FLOAT(x) (*(Register *)(meld_float *)&(x))
#define MELD_CONVERT_LIST(x)  ((Register)(x))

#define MELD_CONVERT_REG_TO_PTR(x)   ((void *)(Register)(x))
#define MELD_CONVERT_PTR_TO_REG(x)   ((Register)(void *)(x))


/******************************************************************************
Definitions of all the data structures and types used by the VM.
*******************************************************************************/

/* allocation for tuples */
#define ALLOC_TUPLE(x) malloc(x)
#define FREE_TUPLE(x) free(x)

/* Meld Types */
typedef void* tuple_t;
typedef short tuple_type;
typedef int32_t meld_int;
typedef double meld_float;
typedef unsigned long int Register;
typedef uint8_t byte;
typedef uint16_t NodeID;
typedef uint16_t Uid;

/* Meld Structures */
typedef struct _tuple_entry tuple_entry;
typedef struct _tuple_queue { tuple_entry *head; tuple_entry *tail; unsigned char length;} tuple_queue;
typedef union { int count; tuple_queue *agg_queue; } record_type;
struct _tuple_entry { struct _tuple_entry *next; record_type records; void *tuple;};
typedef struct _tuple_pentry { Time priority; struct _tuple_pentry *next; record_type records; void *tuple; NodeID rt;} tuple_pentry;
typedef struct {struct _tuple_pentry *queue;} tuple_pqueue;
enum portReferences { DOWN, NORTH, EAST, SOUTH, WEST, UP, NUM_PORTS };

#ifdef BBSIM
typedef Block Node;
#else
typedef int Node;
#endif

#define POINTER_SIZE sizeof(char*)
#define TUPLES tuples
#define OLDTUPLES oldTuples
#define EVAL_HOST (&blockId)
#define PERSISTENT_INITIAL 2
#define PERSISTENT persistent
#define PROVED proved
#define PUSH_NEW_TUPLE(tuple) (enqueueNewTuple(tuple, (record_type)1))
#define TERMINATE_CURRENT() /* NOT IMPLEMENTED FOR BBs */

#define VACANT 0



/******************************************************************************
@Description: MeldIntermVM is the VM's main file, it initializes the VM,
introduces and updates axioms, sends tuples to other blocks, and triggers
the execution of all tuples and rules through the scheduler loop.
*******************************************************************************/

class MeldInterpretVM {
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

            BlinkyBlocksBlock *host;
		NodeID neighbors[6];
		int numNeighbors;
		MeldInterpretVMCore *core;
		int waiting;

	public:
		MeldInterpretVM(BlinkyBlocksBlock *b);
		~MeldInterpretVM();
		void processOneRule();
		bool isWaiting();
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
		void receive_tuple_delete();
		void receive_tuple_add();
		void free_chunk();
		void tuple_send(tuple_t tuple, NodeID rt, meld_int delay, int isNew);
		byte updateRuleState(byte rid);
		void tuple_handle(tuple_t tuple, int isNew, Register *registers);
		NodeID getBlockId();
		void vm_init();
		void vm_alloc();
		byte getNeighborCount()
		Uid down(void);
            Uid up(void);
            Uid north(void);
            Uid south(void);
            Uid east(void);
            Uid west(void);
            inline void setColorWrapper(byte color){
                  setColor(color);
            }
		inline void setLEDWrapper(byte r, byte g, byte b, byte intensity){
                  setLED(r, g, b, intensity);
		}
            void setColor(byte color){
                  VM->host->setColor((int) color);
            }
            void setLED(byte r, byte g, byte b, byte intensity){
                  VM->host->setColor(new Color((float)r/255, (float)g/255, (float)b/255, (float)intensity/255));
            }
            NodeID getGUID();
}

#endif /* MELDINTERPVM_H_ */
