#ifndef MELDINTERPVM_H_
#define MELDINTERPVM_H_

//#include "bb.h"

#include <math.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/timeb.h>
#include <memory>
#include <map>

#include "color.h"
#include "blinkyBlocksBlock.h"


#include <sys/timeb.h>

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
typedef uint32_t Time;

/* Meld Structures */

typedef struct _tuple_entry tuple_entry;
typedef struct _tuple_queue { tuple_entry *head = NULL; tuple_entry *tail = NULL; unsigned char length ;} tuple_queue;

class record_type{
public:
      int count;
      tuple_queue *agg_queue = NULL;
      record_type(int i){
            count = i;
      }
      record_type(tuple_queue *q){
            agg_queue = q;
      }
      operator int () const{
            return count;
      }
      operator tuple_queue* () const{
            return agg_queue;
      }
};

struct _tuple_entry { struct _tuple_entry *next; record_type records; void *tuple;};
typedef struct _tuple_pentry { Time priority; struct _tuple_pentry *next; record_type records; void *tuple; NodeID rt;} tuple_pentry;
typedef struct {struct _tuple_pentry *queue;} tuple_pqueue;

enum portReferences { DOWN, NORTH, EAST, SOUTH, WEST, UP, NUM_PORTS };

typedef int Node;


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
@Description: core.h contains all the constants, external function headers, and
macros, needed by the VM to parse the byte code, manage queues, and process
instructions.
@note: Tuple type is the same as predicate!
*******************************************************************************/

/* DEBUG flags */

// print tuple allocations
/* #define TUPLE_ALLOC_DEBUG 1 */
// tuple allocation checks
/* #define TUPLE_ALLOC_CHECKS 1 */


/* ************* INSTRUCTIONS RELATED DEFINES AND MACROS ************* */
#define INSTR_SIZE 1	/* Size of an instruction (without arguments) */
/* All instructions and their values */
#define RETURN_INSTR	0x00
#define NEXT_INSTR	0x01
#define PERS_ITER_INSTR 0x02
#define TESTNIL_INSTR	0x03 /* niy: not implemented yet */
#define OPERS_ITER_INSTR 0x04 /* niy */
#define LINEAR_ITER_INSTR 0x05
#define RLINEAR_ITER_INSTR 0x06 /* niy */
#define NOT_INSTR	0x07
#define SEND_INSTR 0x08
#define FLOAT_INSTR 0x09 /* niy */
#define SELECT_INSTR 0x0A /* niy */
#define RETURN_SELECT_INSTR 0x0B /* niy */
#define OLINEAR_ITER_INSTR 0x0C /* niy */
#define DELETE_INSTR 0x0D /* niy */
#define RESET_LINEAR_INSTR 0x0E /* niy */
#define END_LINEAR_INSTR 0x0F /* niy */
#define RULE_INSTR 0x10
#define RULE_DONE_INSTR 0x11
#define ORLINEAR_ITER_INSTR 0x12 /* niy */
#define NEW_NODE_INSTR 0x13 /* niy - No need to implement it */
#define NEW_AXIOMS_INSTR 0x14 /* niy */
#define SEND_DELAY_INSTR 0x15 /* niy */
#define PUSH_INSTR 0x16 /* niy */
#define POP_INSTR 0x17 /* niy */
#define PUSH_REGS_INSTR 0x18 /* niy */
#define POP_REGS_INSTR 0x19 /* niy */
#define CALLF_INSTR 0x1A /* niy */
#define CALLE_INSTR 0x1B /* niy */
#define SET_PRIORITY_INSTR 0x1C /* niy */
#define MAKE_STRUCTR_INSTR 0x1D /* niy */
#define MVINTFIELD_INSTR 0x1E
#define MVINTREG_INSTR 0x1F
#define CALL_INSTR 0x20 /* niy */
#define MVFIELDFIELD_INSTR 0x21
#define MVFIELDREG_INSTR 0x22
#define MVPTRREG_INSTR 0x23
#define MVNILREG_INSTR 0x24 /* niy */
#define MVFIELDFIELDR_INSTR 0x25
#define MVREGFIELD_INSTR 0x26
#define MVREGFIELDR_INSTR 0x27 /* niy */
#define MVHOSTFIELD_INSTR 0x28
#define MVREGCONST_INSTR 0x29 /* niy */
#define MVCONSTFIELD_INSTR 0x2A /* niy */
#define MVCONSTFIELDR_INSTR 0x2B /* niy */
#define MVADDRFIELD_INSTR 0x2C /* niy */
#define MVFLOATFIELD_INSTR 0x2D
#define MVFLOATREG_INSTR 0x2E
#define MVINTCONST_INSTR 0x2F /* niy */
#define SET_PRIORITYH_INSTR 0x30 /* niy */
#define MVWORLDFIELD_INSTR 0x31 /* niy */
#define MVSTACKPCOUNTER_INSTR 0x32 /* niy */
#define MVPCOUNTERSTACK_INSTR 0x33 /* niy */
#define MVSTACKREG_INSTR 0x34 /* niy */
#define MVREGSTACK_INSTR 0x35 /* niy */
#define MVADDRREG_INSTR 0x36 /* niy */
#define MVHOSTREG_INSTR 0x37
#define ADDRNOTEQUAL_INSTR 0x38
#define ADDREQUAL_INSTR 0x39
#define INTMINUS_INSTR 0x3A
#define INTEQUAL_INSTR 0x3B
#define INTNOTEQUAL_INSTR 0x3C
#define INTPLUS_INSTR 0x3D
#define INTLESSER_INSTR 0x3E
#define INTGREATEREQUAL_INSTR 0x3F
#define ALLOC_INSTR 0x40
#define BOOLOR_INSTR 0x41
#define INTLESSEREQUAL_INSTR 0x42
#define INTGREATER_INSTR 0x43
#define INTMUL_INSTR 0x44
#define INTDIV_INSTR 0x45
#define FLOATPLUS_INSTR 0x46
#define FLOATMINUS_INSTR 0x47
#define FLOATMUL_INSTR 0x48
#define FLOATDIV_INSTR 0x49
#define FLOATEQUAL_INSTR 0x4A
#define FLOATNOTEQUAL_INSTR 0x4B
#define FLOATLESSER_INSTR 0x4C
#define FLOATLESSEREQUAL_INSTR 0x4D
#define FLOATGREATER_INSTR 0x4E
#define FLOATGREATEREQUAL_INSTR 0x4F
#define MVREGREG_INSTR 0x50
#define BOOLEQUAL_INSTR 0x51
#define BOOLNOTEQUAL_INSTR 0x52
#define HEADRR_INSTR 0x53 /* niy */
#define HEADFR_INSTR 0x54 /* niy */
#define HEADFF_INSTR 0x55 /* niy */
#define HEADRF_INSTR 0x56 /* niy */
#define HEADFFR_INSTR 0x57 /* niy */
#define HEADRFR_INSTR 0x58 /* niy */
#define TAILRR_INSTR 0x59 /* niy */
#define TAILFR_INSTR 0x5A /* niy */
#define TAILFF_INSTR 0x5B /* niy */
#define TAILRF_INSTR 0x5C /* niy */
#define MVWORLDREG_INSTR 0x5D /* niy */
#define MVCONSTREG_INSTR 0x5E /* niy */
#define CONSRRR_INSTR 0x5F /* niy */
#define IF_INSTR 0x60
#define CONSRFF_INSTR 0x61 /* niy */
#define CONSFRF_INSTR 0x62 /* niy */
#define CONSFFR_INSTR 0x63 /* niy */
#define CONSRRF_INSTR 0x64 /* niy */
#define CONSRFR_INSTR 0x65 /* niy */
#define CONSFRR_INSTR 0x66 /* niy */
#define CONSFFF_INSTR 0x67 /* niy */
#define CALL0_INSTR 0x68 /* niy */
#define CALL1_INSTR 0x69 /* niy */
#define CALL2_INSTR 0x6A /* niy */
#define CALL3_INSTR 0x6B /* niy */
#define MVINTSTACK_INSTR 0x6C /* niy */
#define PUSHN_INSTR 0x6D /* niy */
#define MAKE_STRUCTF_INSTR 0x6E /* niy */
#define STRUCT_VALRR_INSTR 0x6F /* niy */
#define MVNILFIELD_INSTR 0x70 /* niy */
#define STRUCT_VALFR_INSTR 0x71 /* niy */
#define STRUCT_VALRF_INSTR 0x72 /* niy */
#define STRUCT_VALRFR_INSTR 0x73 /* niy */
#define STRUCT_VALFF_INSTR 0x74 /* niy */
#define STRUCT_VALFFR_INSTR 0x75 /* niy */
#define MVFLOATSTACK_INSTR 0x76 /* niy */
#define ADDLINEAR_INSTR 0x77
#define ADDPERS_INSTR 0x78
#define RUNACTION_INSTR 0x79
#define ENQUEUE_LINEAR_INSTR 0x7A /* niy */
#define UPDATE_INSTR 0x7B
#define MVARGREG_INSTR 0x7C /* niy */
#define INTMOD_INSTR 0x7D
#define CPU_ID_INSTR 0x7E /* niy */
#define NODE_PRIORITY_INSTR 0x7F /* niy */
#define REMOVE_INSTR 0x80
#define IF_ELSE_INSTR 0x81
#define JUMP_INSTR 0x82
#define ADD_PRIORITY_INSTR 0xA0 /* niy */
#define ADD_PRIORITYH_INSTR 0xA1 /* niy */
#define STOP_PROG_INSTR 0xA2 /* niy */
#define RETURN_LINEAR_INSTR 0xD0
#define RETURN_DERIVED_INSTR 0xF0


/* Size of each instruction and arguments */
#define SEND_BASE 3
#define OP_BASE 4
#define BASE_ITER 21
#define ITER_BASE 23
#define PERS_ITER_BASE 21
#define OPERS_ITER_BASE 21
#define LINEAR_ITER_BASE 21
#define RLINEAR_ITER_BASE 21
#define OLINEAR_ITER_BASE 23
#define ORLINEAR_ITER_BASE 23
#define ALLOC_BASE 1 + 2
#define CALL_BASE 5 + 1
#define IF_BASE 1 + 1 + 4
#define TESTNIL_BASE 1 + 1 + 1
#define HEAD_BASE 1 + 3
#define NOT_BASE 1 + 2 * 1
#define RETURN_BASE 1
#define NEXT_BASE 1
#define FLOAT_BASE 1 + 2 * 1
#define SELECT_BASE 1 + 8
#define RETURN_SELECT_BASE 1 + 4
#define DELETE_BASE 1 + 2
#define REMOVE_BASE 1 + 1
#define RETURN_LINEAR_BASE 1
#define RETURN_DERIVED_BASE 1
#define RESET_LINEAR_BASE 1 + 4
#define END_LINEAR_BASE 1
#define RULE_BASE 1 + 4
#define RULE_DONE_BASE 1
#define NEW_NODE_BASE 1 + 1
#define NEW_AXIOMS_BASE 1 + 4
#define SEND_DELAY_BASE 1 + 2 + 4
#define PUSH_BASE 1
#define POP_BASE 1
#define PUSH_REGS_BASE 1
#define POP_REGS_BASE 1
#define CALLF_BASE 1 + 1
#define CALLE_BASE 3 + 1
#define MAKE_STRUCTR_BASE 1 + 1 + 1
#define MVINTFIELD_BASE 1 + 4 + 2
#define MVINTREG_BASE 1 + 4 + 1
#define MVFIELDFIELD_BASE 1 + 2 + 2
#define MVFIELDREG_BASE 1 + 2 + 1
#define MVPTRREG_BASE 1 + 8 + 1
#define MVNILFIELD_BASE 1 + 2
#define MVNILREG_BASE 1 + 1
#define MVREGFIELD_BASE 1 + 1 + 2
#define MVHOSTFIELD_BASE 1 + 2
#define MVREGCONST_BASE 1 + 1 + 4
#define MVCONSTFIELD_BASE 1 + 4 + 2
#define MVADDRFIELD_BASE 1 + 8 + 2
#define MVFLOATFIELD_BASE 1 + 8 + 2
#define MVFLOATREG_BASE 1 + 8 + 1
#define MVINTCONST_BASE 1 + 4 + 4
#define MVWORLDFIELD_BASE 1 + 2
#define MVSTACKPCOUNTER_BASE 1 + 1
#define MVPCOUNTERSTACK_BASE 1 + 1
#define MVSTACKREG_BASE 1 + 1 + 1
#define MVREGSTACK_BASE 1 + 1 + 1
#define MVADDRREG_BASE 1 + 8 + 1
#define MVHOSTREG_BASE 1 + 1
#define MVREGREG_BASE 1 + 2 * 1
#define MVARGREG_BASE 1 + 1 + 1
#define HEADRR_BASE 1 + 2 * 1
#define HEADFR_BASE 1 + 2 + 1
#define HEADFF_BASE 1 + 2 * 2
#define HEADRF_BASE 1 * 1 + 2
#define TAILRR_BASE 3
#define TAILFR_BASE 4
#define TAILFF_BASE 5
#define TAILRF_BASE 3
#define MVWORLDREG_BASE 1 + 1
#define MVCONSTREG_BASE 1 + 4 + 1
#define CONSRRR_BASE 1 + 1 + 3 * 1
#define CONSRFF_BASE 1 + 1 + 2 * 2
#define CONSFRF_BASE 1 + 2 + 1 + 2
#define CONSFFR_BASE 1 + 2 * 2 + 1
#define CONSRRF_BASE 1 + 2 * 1 + 2
#define CONSRFR_BASE 1 + 1 + 2 + 1
#define CONSFRR_BASE 1 + 1 + 2 + 2 * 1
#define CONSFFF_BASE 1 + 3 * 2
#define CALL0_BASE 5
#define CALL1_BASE 5 + 1
#define CALL2_BASE 5 + 2 * 1
#define CALL3_BASE 5 + 3 * 1
#define MVINTSTACK_BASE 1 + 4 + 1
#define PUSHN_BASE 1 + 1
#define MAKE_STRUCTF_BASE 1 + 2
#define STRUCT_VALRR_BASE 1 + 1 + 2 * 1
#define STRUCT_VALFR_BASE 1 + 1 + 2 + 1
#define STRUCT_VALRF_BASE 5
#define STRUCT_VALRFR_BASE 5
#define STRUCT_VALFF_BASE 1 + 1 + 2 * 2
#define STRUCT_VALFFR_BASE 6
#define MVFLOATSTACK_BASE 1 + 8 + 1
#define ADDLINEAR_BASE 1 + 1
#define ADDPERS_BASE 2
#define RUNACTION_BASE 2
#define ENQUEUE_LINEAR_BASE 2
#define UPDATE_BASE 1 + 1
#define SET_PRIORITY_BASE 1 + 2 * 1
#define SET_PRIORITYH_BASE 1 + 1
#define ADD_PRIORITY_BASE 1 + 2 * 1
#define ADD_PRIORITYH_BASE 1 + 1
#define STOP_PROG_BASE 1
#define CPU_ID_BASE 1 + 2 * 1
#define NODE_PRIORITY_BASE 1 + 2 * 1
#define IF_ELSE_BASE 1 + 1 + 2 * 4
#define JUMP_BASE 1 + 4


/* PTHY: Some of these may be unused, if so, they belonged to the old VM. */
#define IF_JUMP(x)    (*(uint32_t*)((const unsigned char*)(x)))

#define ITER_TYPE(x)  ((*(const unsigned char*)((x)+9))&0x7f)
#define ITER_INNER_JUMP(x)  (*(uint32_t*)((const unsigned char*)((x)+12)))
#define ITER_OUTER_JUMP(x)  (*(uint32_t*)((const unsigned char*)((x)+16)))
#define ITER_NUM_ARGS(x) (*(const unsigned char*)((x)+20))
#define ITER_MATCH_FIELD(x)   (*(const unsigned char*)(x))
#define ITER_MATCH_VAL(x)   ((*(const unsigned char*)((x)+1)))

/* Reg num of reg containing tuple to send */
#define SEND_MSG(x)   (*(const unsigned char*)(x))
/* Reg num of reg containing faceNum to send to */
#define SEND_RT(x)    (*(const unsigned char*)((x)+1))
#define SEND_ARG1(x)  ((*((const unsigned char*)(x)+2)) & 0x3f)
#define SEND_DELAY(x) (*(const unsigned char *)((x)+2))

/* Returns the reg num of the reg containing tuple to remove */
#define REMOVE_REG(x) ((*(const unsigned char*)(x))&0x1f)

#define OP_ARG1(x)    (((*(const unsigned char*)(x)) & 0x3f))
#define OP_ARG2(x)    (((*(const unsigned char*)((x)+1)) & 0xfc) >> 2)
#define OP_OP(x)      ((*(const unsigned char*)((x)+2)) & 0x1f)
#define OP_DST(x)     ((((*(const unsigned char*)((x)+1)) & 0x03) << 3) | \
                      (((*(const unsigned char*)((x)+2)) & 0xe0) >> 5))

/* Returns a byte which is the value of the
   address program counter x is pointing at */
#define FETCH(x)   (*(const unsigned char*)(x))

#define MOVE_SRC(x)   (*(const unsigned char*)((x)+1))
#define MOVE_DST(x)   (((*(const unsigned char*)((x)+1))&0x3f))

#define ALLOC_TYPE(x) ((((*(const unsigned char *)(x))&0x1f) << 2) | \
		       (((*(const unsigned char *)(x+1))&0xc0) >> 6))
#define ALLOC_DST(x)  ((*(const unsigned char *)((x)+1))&0x3f)

#define CALL_VAL(x)   (*(const unsigned char *)(x))
#define CALL_DST(x)   ((*(const unsigned char *)((x)+1)) & 0x1f)
#define CALL_ID(x)    ((((*(const unsigned char *)((x))) & 0x0f) << 3) | \
		       (((*(const unsigned char *)((x)+1)) & 0xe0) >> 5))

#define CALL_ARGS(x)  (extern_functs_args[CALL_ID(x)])
#define CALL_FUNC(x)  (extern_functs[CALL_ID(x)])

#define RESET_LINEAR_JUMP(x) (*(uint32_t*)(pc + 1))

/* ************* TUPLE MACROS ************* */

/* Here is the format of a tuple:
 * [typeID][(arg1)][(arg2)]..[(argx)]
 *    1       x       y          z
 * argument field sizes depends on the type.
 */

#define TYPE_FIELD_SIZE 1
#define TYPE_FIELD_TYPE unsigned char

/* Return the typeid (first byte of a tuple) of tuple x */
#define TUPLE_TYPE(x)   (*(TYPE_FIELD_TYPE *)(x))
/* Returns a pointer to argument at offset (off) of a tuple */
#define TUPLE_FIELD(x,off)					\
      ((Register *)(((unsigned char*)(x)) + TYPE_FIELD_SIZE + (off)))

/* ************* BYTE CODE FILE PARSING ************* */

/* Size of predicate descriptor */
#define TYPE_DESCRIPTOR_SIZE 6

#define NUM_TYPES (meld_prog[0]) /* Number of predicates in program */
#define NUM_RULES (meld_prog[1]) /* Number of rules in program */

/* Offset to predicate descriptor for predicate x */
#define TYPE_OFFSET(x)     (meld_prog[2 + (2 * (x))])
/* Offset to rule descriptor for rule x */
#define RULE_OFFSET(x)     (meld_prog[2 + (2 * (NUM_TYPES) + 2 * (x))])

// PREDICATE DESCRIPTOR
// Descriptor start address
#define TYPE_DESCRIPTOR(x) ((unsigned char *)(meld_prog + TYPE_OFFSET(x)))

/* Returns address of byte code for type x */
#define TYPE_START(x)							\
  ((unsigned char*)(meld_prog + *(unsigned short *)TYPE_DESCRIPTOR(x)))
/* Returns fist byte of byte code for type x */
#define TYPE_START_CHECK(x)			\
  (*(unsigned short *)TYPE_DESCRIPTOR(x))
// Contain tuple's properties (linear/persistent...)
#define TYPE_PROPERTIES(x) (*(TYPE_DESCRIPTOR(x) + 2))
// If tuple is aggregate, contains its aggregate type, 0 otherwise
#define TYPE_AGGREGATE(x)  (*(TYPE_DESCRIPTOR(x) + 3))
// Stratification round
#define TYPE_STRATIFICATION_ROUND(x) (*(TYPE_DESCRIPTOR(x) + 4))
// Number of arguments
#define TYPE_NUMARGS(x)     (*(TYPE_DESCRIPTOR(x) + 5))
// Argument descriptor
#define TYPE_ARGS_DESC(x)					\
  ((unsigned char*)(TYPE_DESCRIPTOR(x)+TYPE_DESCRIPTOR_SIZE))
// Returns type of argument number f for type x
#define TYPE_ARG_DESC(x, f) ((unsigned char *)(TYPE_ARGS_DESC(x)+1*(f)))

// RULE DESCRIPTOR
/* Descriptor start */
#define RULE_DESCRIPTOR(x) ((unsigned char*)(meld_prog + RULE_OFFSET(x)))

/* Returns address of byte code for rule x */
#define RULE_START(x)							\
  ((unsigned char*)(meld_prog + *(unsigned short*)(RULE_DESCRIPTOR(x))))
/* Returns first byte of byte code for rule x */
#define RULE_START_CHECK(x)						\
  (*(unsigned char*)(meld_prog + *(unsigned short*)(RULE_DESCRIPTOR(x))))
/* Offset to rule byte code, pred 0 byte code start is reference */
/* Returns 1 if rule is persistent, 0 otherwise */
#define RULE_ISPERSISTENT(x) (*(RULE_DESCRIPTOR(x) + 2))
/* Number of predicates included in rule x */
#define RULE_NUM_INCLPREDS(x)   (*(RULE_DESCRIPTOR(x) + 3))
/* ID of included predicate at index f  */
#define RULE_INCLPRED_ID(x, f) (*(unsigned char *)(RULE_DESCRIPTOR(x) + 4 + 1*(f)))

/* Returns if a predicate is stratified or not */
#define TYPE_IS_STRATIFIED(x) (TYPE_STRATIFICATION_ROUND(x) > 0)

/* Alternative way of getting name string for type x */
#define TYPE_NAME(x)       (tuple_names[x])
/* Returns type of argument f of predicate x from byte code*/
#define TYPE_ARG_TYPE(x, f) ((unsigned char)(*TYPE_ARG_DESC(x, f)))

/* Returns total size of predicate x */
#define TYPE_SIZE(x)       ((size_t)arguments[(x) * 2 + 1])
/* Returns address of arguments of type x in arguments array */
#define TYPE_ARGS(x)       (arguments + arguments[(x) * 2])

/* Returns argument (type?) number f of type x */
#define TYPE_ARG(x, f)     (TYPE_ARGS(x)+2*(f))
/* Returns size of argument number f of type x */
#define TYPE_ARG_SIZE(x, f) (*TYPE_ARG(x, f))
/* Returns arguments array offset for arg number f of type x */
#define TYPE_ARG_OFFSET(x, f)   (*(TYPE_ARG(x, f) + 1))

/* Set the value of a tuple's field */
#define SET_TUPLE_FIELD(tuple, field, data) \
		memcpy(TUPLE_FIELD(tuple, TYPE_ARG_OFFSET(TUPLE_TYPE(tuple), field)), \
				data, TYPE_ARG_SIZE(TUPLE_TYPE(tuple), field))
/* Get the value of a tuple's field */
#define GET_TUPLE_FIELD(tuple, field) \
		TUPLE_FIELD(tuple, TYPE_ARG_OFFSET(TUPLE_TYPE(tuple), field))
/* Get total size of a tuple */
#define GET_TUPLE_SIZE(tuple, field) \
		TYPE_ARG_SIZE(TUPLE_TYPE(tuple), field)

/* Macros to test the property of a type */
#define TYPE_IS_AGG(x)        (TYPE_PROPERTIES(x) & 0x01)
#define TYPE_IS_PERSISTENT(x) (TYPE_PROPERTIES(x) & 0x02)
#define TYPE_IS_LINEAR(x)     (TYPE_PROPERTIES(x) & 0x04)
#define TYPE_IS_DELETE(x)     (TYPE_PROPERTIES(x) & 0x08)
#define TYPE_IS_ROUTING(x)    (TYPE_PROPERTIES(x) & 0x20)
#define TYPE_IS_ACTION(x)     (TYPE_PROPERTIES(x) & 0x10)

/* x is aggregate byte for a type */
/* Returns aggregate type of aggregate */
#define AGG_AGG(x)    (((x) & (0xf0)) >> 4)
/* Returns field at which aggregate is located */
#define AGG_FIELD(x)  ((x) & 0x0f)

/* ************* MACROS FOR BYTECODE_PROCESS FUNCTION ************* */

/* Strip the type of the element which is being processed, from the state byte */
#define PROCESS_TYPE(x) ((unsigned char)((x) & 0x0f))
#define PROCESS_TUPLE 0
#define PROCESS_ITER 1
#define PROCESS_RULE 2

/* Strip the rule number of the rule being processed from the state byte */
#define RULE_NUMBER(x) ((unsigned char)(((x) & 0xf0) >> 4))

/* Return types for checkRuleState function */
#define INACTIVE_RULE 0x0
#define ACTIVE_RULE   0x1

/* Constants for isLinear */
#define NOT_LINEAR 0
#define IS_LINEAR 1

/* Return types for process function */
#define RET_RET 0
#define RET_NEXT 1
#define RET_LINEAR 2
#define RET_DERIVED 3
#define RET_NO_RET 4
#define RET_ERROR -1

/* ************* EVAL FUNCTIONS ************* */

/* Various macros to get value of byte code */
#define VAL_REG(x) (((const unsigned char)(x)) & 0x1f)
#define VAL_FIELD_NUM(x) ((*(const unsigned char *)(x)) & 0xff)
#define VAL_FIELD_REG(x) ((*(const unsigned char *)((x)+1)) & 0x1f)

/* ************* AGGREGATE TYPES ************* */

#define AGG_NONE 0
#define AGG_FIRST 1
#define AGG_MAX_INT 2
#define AGG_MIN_INT 3
#define AGG_SUM_INT 4
#define AGG_MAX_FLOAT 5
#define AGG_MIN_FLOAT 6
#define AGG_SUM_FLOAT 7
#define AGG_SET_UNION_INT 8
#define AGG_SET_UNION_FLOAT 9
#define AGG_SUM_LIST_INT 10
#define AGG_SUM_LIST_FLOAT 11

/* ************* FIELD TYPES ************* */

#define FIELD_INT 0x0
#define FIELD_FLOAT 0x1
#define FIELD_ADDR 0x2
#define FIELD_OTHER 0x2
#define FIELD_LIST_INT 0x3
#define FIELD_LIST_FLOAT 0x4
#define FIELD_LIST_ADDR 0x5
#define FIELD_SET_INT 0x6
#define FIELD_SET_FLOAT 0x7
#define FIELD_TYPE 0x8
#define FIELD_STRING 0x9
#define FIELD_BOOL 0xa

/* ************* EXTERN FUNCTION IDs ************* */
#define NODE2INT_FUNC 0x1c

typedef Register (*extern_funct_type)();

/******************************************************************************
@Description: MeldInterpretVM is the VM's main file, it initializes the VM,
introduces and updates axioms, sends tuples to other blocks, and triggers
the execution of all tuples and rules through the scheduler loop.
*******************************************************************************/

namespace MeldInterpret {

class MeldInterpretVM;

class MeldInterpretVM {
	private:

		/* This block's ID */
		NodeID blockId;
		/* An array of 32 registers (pointers) */
		Register reg[32];

		int numNeighbors;
		int waiting;
		static bool debugging;
		static bool configured;
		bool firstStart;

      public:
            static map<int, MeldInterpretVM*> vmMap;
            static const unsigned char * meld_prog;
            static char **tuple_names;
            static char **rule_names;

            uint64_t currentLocalDate;
            bool hasWork, polling, deterministicSet;
            NodeID neighbors[6];
            tuple_type TYPE_INIT;
            tuple_type TYPE_EDGE;
            tuple_type TYPE_TERMINATE;
            tuple_type TYPE_NEIGHBORCOUNT;
            tuple_type TYPE_NEIGHBOR;
            tuple_type TYPE_VACANT;
            tuple_type TYPE_TAP;
            tuple_type TYPE_SETCOLOR;
            tuple_type TYPE_SETCOLOR2;

            BlinkyBlocks::BlinkyBlocksBlock *host;

		MeldInterpretVM(BlinkyBlocks::BlinkyBlocksBlock *b);
		~MeldInterpretVM();
		void processOneRule();
		bool isWaiting();
		static bool equilibrium();
		inline static bool isInDebuggingMode() { return debugging; };
		static void setConfiguration(string path, bool d);
		static void readProgram(string path);
		static int characterCount(string in, char character);

		byte updateRuleState(byte rid);
		Time myGetTime();
		void __myassert(string file, int line, string exp);
		NodeID get_neighbor_ID(int face);
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
		void vm_init();
		void vm_alloc();
		byte getNeighborCount();
		Uid down(void);
            Uid up(void);
            Uid north(void);
            Uid south(void);
            Uid east(void);
            Uid west(void);
            void setColor(byte color);
            void setColor(Color color);
            void setLED(byte r, byte g, byte b, byte intensity);
            inline void setColorWrapper(byte color){
                  setColor(color);
            }
		inline void setLEDWrapper(byte r, byte g, byte b, byte intensity){
                  setLED(r, g, b, intensity);
		}
            NodeID getGUID();
            extern_funct_type *extern_functs;
            int *extern_functs_args;
            static unsigned char * arguments;

            void print_newTuples ();
            void print_newStratTuples ();
            void printDebug(string str);
            NodeID getBlockId();
            	/* ************* TUPLE HANDLING FUNCTIONS  ************* */

            /* Queue for tuples to send with delay */
            tuple_pqueue *delayedTuples;
            /* Contains a queue for each type, this is essentially the database */
            tuple_queue *tuples;
            /* Where stratified tuples are enqueued for execution  */
            tuple_pqueue *newStratTuples;
            /* Where non-stratified tuples are enqueued for execution */
            tuple_queue *newTuples;
            /* Received tuples are enqueued both to a normal queue
             * and to this one. Tuples store in this one will be used to remove
             * remove tuples from the database.*/
            tuple_queue receivedTuples[NUM_PORTS];

            /*static Why static again ?*/ inline tuple_t
            tuple_alloc(tuple_type type)
            {
            #ifdef TUPLE_ALLOC_CHECKS
              if(type >= NUM_TYPES || type < 0) {
                fprintf(stderr, "Unrecognized type: %d\n", type);
                exit(EXIT_FAILURE);
              }
            #endif

              tuple_t tuple = ALLOC_TUPLE(TYPE_SIZE(type));

              TUPLE_TYPE(tuple) = type;

            #ifdef TUPLE_ALLOC_DEBUG
              printf("New %s(%d) tuple -- size: %d\n", tuple_names[type],
                   type, TYPE_SIZE(type));
            #endif

                  return tuple;
            }

            void tuple_do_handle(tuple_type type,	void *tuple, int isNew, Register *reg);
            void tuple_print(tuple_t tuple, FILE *fp);

            //static inline void tuple_dump(void *tuple) Why static ?
            inline void tuple_dump(void *tuple)
            {
                  tuple_print(tuple, stderr);
                  fprintf(stderr, "\n");
            }

            /* ************* MISC FUNCTION PROTOTYPES ************* */
            int process_bytecode(tuple_t tuple, const unsigned char *pc, int isNew, int isLinear, Register *reg, byte state);

            void init_fields(void);
            void init_consts(void);

            void facts_dump(void);
            void print_program_info(void);
            char* arg2String(tuple_t tuple, byte index);


            /* ************* LOW LEVEL INSTRUCTION FUNCTION ************* */
            byte val_is_float(const byte x);
            byte val_is_int(const byte x);
            byte val_is_field(const byte x);
            void * eval_field (tuple_t tuple, const unsigned char **pc);
            int execute_iter (const unsigned char *pc, Register *reg, int isNew, int isLinear);
            void execute_run_action (const unsigned char *pc, Register *reg, int isNew);
            void * eval_reg(const unsigned char value, const unsigned char **pc, Register *reg);
            void * eval_int (const unsigned char **pc);
            void * eval_float (const unsigned char **pc);
            void moveTupleToReg (const unsigned char reg_index, tuple_t tuple, Register *reg);
            void execute_addtuple (const unsigned char *pc, Register *reg, int isNew);
            void execute_send_delay (const unsigned char *pc, Register *reg, int isNew);
            void execute_alloc (const unsigned char *pc, Register *reg);
            void execute_update (const unsigned char *pc, Register *reg);
            void execute_send (const unsigned char *pc, Register *reg, int isNew);
            void execute_call1 (const unsigned char *pc, Register *reg);
            void execute_run_action0 (tuple_t action_tuple, tuple_type type, int isNew);
            void execute_remove (const unsigned char *pc, Register *reg, int isNew);
            void execute_mvintfield (const unsigned char *pc, Register *reg);
            void execute_mvintreg (const unsigned char *pc, Register *reg);
            void execute_mvfloatreg (const unsigned char *pc, Register *reg);
            void execute_mvfloatfield (const unsigned char *pc, Register *reg);
            void execute_mvfieldreg (const unsigned char *pc, Register *reg);
            void execute_mvfieldfield (const unsigned char *pc, Register *reg);
            void execute_mvregfield (const unsigned char *pc, Register *reg);
            void execute_mvhostfield (const unsigned char *pc, Register *reg);
            void execute_mvhostreg (const unsigned char *pc, Register *reg);
            void execute_mvregreg (const unsigned char *pc, Register *reg);
            void execute_not (const unsigned char *pc, Register *reg);
            void execute_boolor (const unsigned char *pc, Register *reg);
            void execute_boolequal (const unsigned char *pc, Register *reg);
            void execute_boolnotequal (const unsigned char *pc, Register *reg);
            void execute_addrequal (const unsigned char *pc, Register *reg);
            void execute_addrnotequal (const unsigned char *pc, Register *reg);
            void execute_intequal (const unsigned char *pc, Register *reg);
            void execute_intnotequal (const unsigned char *pc, Register *reg);
            void execute_intgreater (const unsigned char *pc, Register *reg);
            void execute_intgreaterequal (const unsigned char *pc, Register *reg);
            void execute_intlesser (const unsigned char *pc, Register *reg);
            void execute_intlesserequal (const unsigned char *pc, Register *reg);
            void execute_intmul (const unsigned char *pc, Register *reg);
            void execute_intdiv (const unsigned char *pc, Register *reg);
            void execute_intmod (const unsigned char *pc, Register *reg);
            void execute_intplus (const unsigned char *pc, Register *reg);
            void execute_intminus (const unsigned char *pc, Register *reg);
            void execute_floatplus (const unsigned char *pc, Register *reg);
            void execute_floatminus (const unsigned char *pc, Register *reg);
            void execute_floatmul (const unsigned char *pc, Register *reg);
            void execute_floatdiv (const unsigned char *pc, Register *reg);
            void execute_floatequal (const unsigned char *pc, Register *reg);
            void execute_floatnotequal (const unsigned char *pc, Register *reg);
            void execute_floatlesser (const unsigned char *pc, Register *reg);
            void execute_floatlesserequal (const unsigned char *pc, Register *reg);
            void execute_floatgreater (const unsigned char *pc, Register *reg);
            void execute_floatgreaterequal (const unsigned char *pc, Register *reg);
            bool aggregate_accumulate(int agg_type, void *acc, void *obj, int count);
            bool aggregate_changed(int agg_type, void *v1, void *v2);
            void aggregate_seed(int agg_type, void *acc, void *start, int count, size_t size);
            void aggregate_free(tuple_t tuple, unsigned char field_aggregate, unsigned char type_aggregate);
            void aggregate_recalc(tuple_entry *agg, Register *reg, bool first_run);
            void databaseConsistencyChecker();



            /* ************* QUEUE MANAGEMENT PROTOTYPES ************* */

            tuple_entry* queue_enqueue(tuple_queue *queue, tuple_t tuple, record_type isNew);
            bool queue_is_empty(tuple_queue *queue);
            tuple_t queue_dequeue(tuple_queue *queue, int *isNew);
            tuple_t queue_dequeue_pos(tuple_queue *queue, tuple_entry **pos);
            tuple_t queue_pop_tuple(tuple_queue *queue);
            void queue_push_tuple(tuple_queue *queue, tuple_entry *entry);

            static inline void queue_init(tuple_queue *queue)
            {
              queue->head = NULL;
              queue->tail = NULL;
            }

            static inline bool p_empty(tuple_pqueue *q)
            {
                  return q->queue == NULL;
            }

            static inline tuple_pentry* p_peek(tuple_pqueue *q)
            {
                  return q->queue;
            }

            tuple_pentry *p_dequeue(tuple_pqueue *q);
            void p_enqueue(tuple_pqueue *q, Time priority, tuple_t tuple, NodeID rt, record_type isNew);
            int queue_length (tuple_queue *queue);
};

}

#endif /* MELDINTERPVM_H_ */
