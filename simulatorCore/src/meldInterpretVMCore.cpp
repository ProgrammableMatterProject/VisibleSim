#include <math.h>
#include <string.h>
#include "meldInterpretVMCore.h"
#include "blinkyBlocksBlock.h"

#ifdef LOG_DEBUG
// not compiling ...
//#include "../system/log.bbh"
#endif
/******************************************************************************
@Description: core.c represents the core of the VM as its name implies.
It contains all the function implementations for the following tasks:
- Queue management.
- Byte code processing. (Every byte code instruction as an associated execution
function which is called when the instruction is encountered)
- Removing / Adding tuples from/to the database, or just enqueuing them for
execution.
- Printing tuples or the database's content.
- Handling aggregates.
*******************************************************************************/

//#define DEBUG_INSTRS
/* #define DEBUG_ALLOCS */
#define inline

namespace MeldInterpret{

      unsigned char * MeldInterpretVMCore::arguments = NULL;
      const unsigned char * MeldInterpretVMCore::meld_prog = NULL;

    MeldInterpretVMCore::MeldInterpretVMCore(){

            /* Init will always be predicate 0 */
            TYPE_INIT = 0;
            /* Initialize unknown type IDs */
            TYPE_EDGE = -1;
            TYPE_TERMINATE = -1;
            TYPE_NEIGHBORCOUNT = -1;
            TYPE_NEIGHBOR = -1;
            TYPE_VACANT = -1;
            TYPE_TAP = -1;
    }

    MeldInterpretVMCore::~MeldInterpretVMCore(){
          return;
    }

    NodeID MeldInterpretVMCore::getBlockId (void) { return blockId; }

      void MeldInterpretVMCore::setColorWrapper(byte color){
            setColor(color);
      }
      void MeldInterpretVMCore::setLEDWrapper(byte r, byte g, byte b, byte intensity){
            setLED(r, g, b, intensity);
      }

      void MeldInterpretVMCore::setColor(byte color){
      }
      void MeldInterpretVMCore::setLED(byte r, byte g, byte b, byte intensity){
      }

    inline byte MeldInterpretVMCore::val_is_float(const byte x) {
        return x == 0x00;
    }
    inline byte MeldInterpretVMCore::val_is_int(const byte x) {
        return x == 0x01;
    }
    inline byte MeldInterpretVMCore::val_is_field(const byte x) {
        return x == 0x02;
    }

    /* ************* EVAL FUNCTIONS ************* */

    /* Returns the address of field number 'field_num' (extracted from byte code)
     * of the tuple given as argument.
     * Also increment pc past the field.
     */
    inline
    void* MeldInterpretVMCore::eval_field (tuple_t tuple, const unsigned char **pc) {
        const unsigned char field_num = VAL_FIELD_NUM(*pc);
        (*pc) += 2;

        return GET_TUPLE_FIELD(tuple, field_num);
    }
    /* Returns the address of register number 'value'
     * and increment pc past the reg byte.
     */
    inline
    void* MeldInterpretVMCore::eval_reg(const unsigned char value, const unsigned char **pc, Register *reg) {
        ++(*pc);
        return &(reg)[VAL_REG(value)];
    }

    /* Returns a pointer to the meld_int at address pointed at by pc
     * and increment it past the int.
     */
    inline
    void* MeldInterpretVMCore::eval_int (const unsigned char **pc) {
        void *ret = (void *)(*pc);
        *pc += sizeof(meld_int);

        return ret;
    }

    /* Returns a pointer to the meld_float (double) at address pointed at by pc
     * and increment it past the float.
     */
    inline
    void* MeldInterpretVMCore::eval_float (const unsigned char **pc) {
        void *ret = (void *)(*pc);
        *pc += sizeof(meld_float);

        return ret;
    }

    /* Set value of register number 'reg_index' as a pointer to tuple 'tuple' */
    inline
    void MeldInterpretVMCore::moveTupleToReg (const unsigned char reg_index, tuple_t tuple, Register *reg) {
        Register *dst = &(reg)[VAL_REG(reg_index)];
        *dst = (Register)tuple;

#if 0
        printf ("--%d--\t MOVE %s to reg %d\n", getBlockId(),
                tuple_names[TUPLE_TYPE(reg[reg_index])], reg_index);
#endif
    }

    /* ************* INSTRUCTION EXECUTION FUNCTIONS ************* */

    /* Allocates a new tuple of type 'type' and set its type byte */
    inline void
    MeldInterpretVMCore::execute_alloc (const unsigned char *pc, Register *reg) {
        ++pc;
        tuple_type type = FETCH(pc++);
        byte reg_index = FETCH(pc);
        tuple_t *dst = (void**)eval_reg (reg_index, &pc, reg);
        *dst = ALLOC_TUPLE(TYPE_SIZE(type));

#if defined(DEBUG_INSTRS) || defined(DEBUG_ALLOCS)
        {
            printf ("--%d--\t ALLOC %s TO reg %d\n",
                    getBlockId(), tuple_names[type], reg_index);
        }
#endif

        memset (*dst, 0, TYPE_SIZE(type));
        TUPLE_TYPE(*dst) = type;
    }

    /* Either enqueue a tuple for derivation
     * or enqueue it for retraction
     */
    inline void
    MeldInterpretVMCore::execute_addtuple (const unsigned char *pc, Register *reg, int isNew) {
        ++pc;

        byte reg_index = FETCH(pc);
        Register tuple_reg = reg[reg_index];

#ifdef DEBUG_INSTRS
        tuple_type type = TUPLE_TYPE((tuple_t)tuple_reg);
        if (isNew < 0)
            printf ("--%d--\t Enqueue fact reg %d: %s\n",
                    getBlockId(), reg_index, tuple_names[type]);
        else
            printf ("--%d--\t Enqueue RETRACTION fact reg %d: %s\n",
                    getBlockId(), reg_index, tuple_names[type]);

#endif

        enqueueNewTuple((tuple_t)MELD_CONVERT_REG_TO_PTR(tuple_reg), (record_type) isNew);
    }

    /* Only used to notify user that a linear tuple has been updated
     * instead of removed during rule derivation.
     */
    inline void
    MeldInterpretVMCore::execute_update (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg_index = FETCH(pc);
        Register tuple_reg = reg[reg_index];

#ifdef DEBUG_INSTRS
        tuple_type type = TUPLE_TYPE((tuple_t)tuple_reg);
        printf ("--%d--\t UPDATE reg %d: %s\n", getBlockId(), reg_index,
                tuple_names[type]);
#else
        (void)tuple_reg;

#endif
    }

    /* Send tuple pointed at by 'send_reg' to blockID designated by send_rt
     * NO DELAY!
     */
    inline void
    MeldInterpretVMCore::execute_send (const unsigned char *pc, Register *reg, int isNew) {
        ++pc;
        Register send_reg = reg[SEND_MSG(pc)];
        NodeID send_rt = reg[SEND_RT(pc)];

#ifdef DEBUG_INSTRS
        printf("--%d--\t SEND reg %d TO reg %d\n",
               getBlockId(), SEND_MSG(pc), SEND_RT(pc));
#endif

        tuple_send((tuple_t)MELD_CONVERT_REG_TO_PTR(send_reg), send_rt, 0, isNew);
    }

    /* Call an external function with one argument.
     * Not implemented yet, only used to support the node2int function
     * which is useless when using BB as nodeID's are not pointers.
     */
    inline void
    MeldInterpretVMCore::execute_call1 (const unsigned char *pc, Register *reg) {
        ++pc;
        byte functionID = FETCH(pc++);
        byte dst_index = FETCH(pc++);
        Register *dst = (Register*)eval_reg (dst_index, &pc, reg);

        byte arg1_index = FETCH(pc);
        Register *arg1 = (Register*)eval_reg (arg1_index, &pc, reg);

#ifdef DEBUG_INSTRS
        if (functionID == 0x1f)
            /* No need to do anything for this function since VM is already *
             * considering node args as NodeID's, which are int's           */
            printf("--%d--\t CALL1 node2int/%d TO reg %d = (reg %d)\n",
                   getBlockId(), arg1_index, dst_index, arg1_index);
        else
            printf("--%d--\t CALL1 (some func)/%d TO reg %d = (reg %d)\n",
                   getBlockId(), arg1_index, dst_index, arg1_index);
#endif

        if (functionID != 0x1f)
            fprintf(stderr, "--%d--\t Error: call to function not implemented yet!\n",
                    getBlockId());

        /* Do nothing for now since no function are currectly implemented */
        (void)arg1;
        (void)dst;
    }

    /* Similar to send, but with a delay */
    inline void
    MeldInterpretVMCore::execute_send_delay (const unsigned char *pc, Register *reg, int isNew) {
        ++pc;

        const byte tpl = SEND_MSG(pc);
        const byte dst = SEND_RT(pc);
        Register send_reg = reg[tpl];
        NodeID send_rt = reg[dst];
        pc += 2;
        meld_int *delay = (meld_int*)eval_int(&pc);

#ifdef DEBUG_INSTRS
        printf("--%d--\t SEND reg %d TO reg %d(%d) WITH DELAY %dms\n",
               getBlockId(), SEND_MSG(pc), SEND_RT(pc), send_rt, *delay);
#endif

        if(tpl == dst) {
            tuple_send((tuple_t)MELD_CONVERT_REG_TO_PTR(send_reg), getBlockId(), *delay, isNew);
        } else {
            tuple_send((tuple_t)MELD_CONVERT_REG_TO_PTR(send_reg), send_rt, *delay, isNew);
        }
    }

    /* Iterate through the database to find a match with tuple read from byte code
     * If there are matches, process the inside of the ITER with all matches sequentially.
     */
    int
    MeldInterpretVMCore::execute_iter (const unsigned char *pc, Register *reg, int isNew, int isLinear) {
        const unsigned char *inner_jump = pc + ITER_INNER_JUMP(pc);
        const tuple_type type = ITER_TYPE(pc);
        int i, k, length;
        void **list;

        /* Reg in which match will be stored during execution*/
        byte reg_store_index = FETCH(pc+10);

        /* produce a random ordering for all tuples of the appropriate type */
        tuple_entry *entry = TUPLES[type].head;

        length = queue_length(&TUPLES[ITER_TYPE(pc)]);
        list = (void**)malloc(sizeof(tuple_t) * length);

        for (i = 0; i < length; i++) {
            int j = random() % (i+1);

            list[i] = list[j];
            list[j] = entry->tuple;

            entry = entry->next;
        }

#ifdef DEBUG_INSTRS
        printf("--%d--\t ITER %s len=%d TO reg %d\n",
               getBlockId(), tuple_names[type], length, reg_store_index);
#endif

        if(length == 0) {
            /* no need to execute any further code, just jump! */
            return RET_NO_RET;
        }

        /* iterate over all tuples of the appropriate type */
        void *next_tuple;

        for (i = 0; i < length; i++) {
            next_tuple = list[i];

            unsigned char matched = 1;
            unsigned char num_args = ITER_NUM_ARGS(pc);
            const unsigned char *tmppc = pc + PERS_ITER_BASE;

            /* check to see if it matches */
            for (k = 0; k < num_args; ++k) {
                const unsigned char fieldnum = ITER_MATCH_FIELD(tmppc);
                const unsigned char fieldtype = TYPE_ARG_TYPE(type, fieldnum);
                const unsigned char type_size = TYPE_ARG_SIZE(type, fieldnum);
                const unsigned char value_type = ITER_MATCH_VAL(tmppc);

                Register *field = GET_TUPLE_FIELD(next_tuple, fieldnum);
                Register *val;

                if (val_is_int (value_type)) {
                    tmppc += 2;
                    val = (Register *)eval_int(&tmppc);
                } else if (val_is_float (value_type)) {
                    tmppc += 2;
                    val = (Register *)eval_float(&tmppc);
                } else if (val_is_field (value_type)) {
                    tmppc += 2;
                    byte reg_index = FETCH(tmppc+1);
                    tuple_t tpl = (tuple_t)reg[reg_index];
                    val = (Register *)eval_field(tpl, &tmppc);
                }  else {
                    /* Don't know what to do */
                    fprintf (stderr, "Type %d not supported yet - don't know what to do.\n", fieldtype);
                    assert (0);
                    exit (2);
                }

                matched = matched && (memcmp(field, val, type_size) == 0);
            }

#ifdef DEBUG_INSTRS
            printf("--%d--\t MATCHED: %d | length: %d\n", getBlockId(),
                   matched, length);
#endif

            if (matched) {
                /* We've got a match! */
                moveTupleToReg (reg_store_index, next_tuple, reg);
                /* Process it - And if we encounter a return instruction, return
                 * Otherwise, look for another match.
                 */
                int ret = process_bytecode(next_tuple, inner_jump,
                                           isNew, isLinear || TYPE_IS_LINEAR(TUPLE_TYPE(next_tuple)), reg, PROCESS_ITER);
                if(ret == RET_LINEAR) {
                    free(list);
                    return ret;
                }
                if(isLinear && ret == RET_DERIVED) {
                    free(list);
                    return ret;
                }
                if(ret == RET_RET) {
                    free(list);
                    return ret;
                }
            }
        }

        free(list);

        /* process next instructions */
        return RET_NO_RET;
    }

    inline void
    MeldInterpretVMCore::execute_run_action0 (tuple_t action_tuple, tuple_type type, int isNew) {
        switch (type) {
        case TYPE_SETCOLOR:
            if (isNew > 0) {

#ifdef DEBUG_INSTRS
                printf ("--%d--\t RUN ACTION: %s(currentNode, %d, %d, %d, %d)\n",
                        getBlockId(), tuple_names[type],
                        MELD_INT(GET_TUPLE_FIELD(action_tuple, 0)),
                        MELD_INT(GET_TUPLE_FIELD(action_tuple, 1)),
                        MELD_INT(GET_TUPLE_FIELD(action_tuple, 2)),
                        MELD_INT(GET_TUPLE_FIELD(action_tuple, 3)));
#endif

                /* Don't call it directly to avoid having to import led.bbh */
                setLEDWrapper(*(byte *)GET_TUPLE_FIELD(action_tuple, 0),
                              *(byte *)GET_TUPLE_FIELD(action_tuple, 1),
                              *(byte *)GET_TUPLE_FIELD(action_tuple, 2),
                              *(byte *)GET_TUPLE_FIELD(action_tuple, 3));
            }
            FREE_TUPLE(action_tuple);
            return;

        case TYPE_SETCOLOR2:
            if (isNew > 0) {
#ifdef DEBUG_INSTRS
                printf ("--%d--\t RUN ACTION: %s(currentNode, %d)\n",
                        getBlockId(), tuple_names[type],
                        MELD_INT(GET_TUPLE_FIELD(action_tuple, 0)));
#endif

                /* Don't call it directly to avoid having to import led.bbh */
                setColorWrapper(MELD_INT(GET_TUPLE_FIELD(action_tuple, 0)));
            }

            FREE_TUPLE(action_tuple);
            return;
        }
    }

    /* Run an action onto the block */
    inline void
    MeldInterpretVMCore::execute_run_action (const unsigned char *pc, Register *reg, int isNew) {
        ++pc;

        byte reg_index = FETCH(pc);

        tuple_t action_tuple = (tuple_t)reg[reg_index];
        tuple_type type = TUPLE_TYPE(action_tuple);
        execute_run_action0(action_tuple, type, isNew);
    }

    inline void
    MeldInterpretVMCore::execute_remove (const unsigned char *pc, Register *reg, int isNew) {
        if (isNew > 0) {
            ++pc;
            int reg_remove = REMOVE_REG(pc);
            tuple_type type = TUPLE_TYPE(MELD_CONVERT_REG_TO_PTR(reg[reg_remove]));
            int size = TYPE_SIZE(type);

#ifdef DEBUG_INSTRS
            printf ("--%d--\t REMOVE reg %d: %s of size %d\n",
                    getBlockId(), reg_remove, tuple_names[type], size);
#endif

            tuple_handle(memcpy(malloc(size),MELD_CONVERT_REG_TO_PTR(reg[reg_remove]), size), -1, reg);
            reg[REMOVE_REG(pc)] = 0;
        }
    }

    /* ************* MOVE INSTRUCTIONS FUNCTIONS ************* */

    /* Moves an int to a tuple field */
    inline void
    MeldInterpretVMCore::execute_mvintfield (const unsigned char *pc, Register *reg) {
        ++pc;

        Register *src = (Register*)eval_int (&pc);
        byte reg_index = FETCH(pc+1);
        byte field_num = FETCH(pc);

        tuple_t dst_tuple = (tuple_t)reg[reg_index];
        tuple_type type = TUPLE_TYPE(dst_tuple);

        Register *dst = (Register*)eval_field (dst_tuple, &pc);


#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE INT %d TO FIELD %d.%d\n",
                getBlockId(), MELD_INT(src), reg_index, field_num);
#endif

        size_t size = TYPE_ARG_SIZE(type, field_num);

        memcpy(dst, src, size);
    }

    /* Moves pointer to an int to a register */
    inline void
    MeldInterpretVMCore::execute_mvintreg (const unsigned char *pc, Register *reg) {
        ++pc;

        Register *src = (Register*)eval_int (&pc);
        byte reg_index = FETCH(pc);
        Register *dst = (Register*)eval_reg (reg_index, &pc, reg);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE INT %d TO reg %d\n",
                getBlockId(), MELD_INT(src), reg_index);
#endif
        size_t size = sizeof(Register);
        memcpy(dst, src, size);
    }

    /* Moves pointer to a float to a register */
    inline void
    MeldInterpretVMCore::execute_mvfloatreg (const unsigned char *pc, Register *reg) {
        ++pc;

        Register *src = (Register*)eval_float (&pc);
        byte reg_index = FETCH(pc);
        Register *dst = (Register*)eval_reg (reg_index, &pc, reg);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE FLOAT %f TO reg %d\n",
                getBlockId(), MELD_FLOAT(src), reg_index);
#endif

        size_t size = sizeof(Register);
        memcpy(dst, src, size);
    }

    /* Moves pointer to a float to a tuple field */
    inline void
    MeldInterpretVMCore::execute_mvfloatfield (const unsigned char *pc, Register *reg) {
        ++pc;

        Register *src = (Register*)eval_float (&pc);
        byte reg_index = FETCH(pc+1);
        byte field_num = FETCH(pc);

        tuple_t dst_tuple = (tuple_t)reg[reg_index];
        tuple_type type = TUPLE_TYPE(dst_tuple);

        Register *dst = (Register*)eval_field (dst_tuple, &pc);


#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE FLOAT %f TO FIELD %d.%d\n",
                getBlockId(), MELD_FLOAT(src), reg_index, field_num);
#endif

        size_t size = TYPE_ARG_SIZE(type, field_num);

        memcpy(dst, src, size);
    }

    /* Moves pointer to a tuple field to a register */
    inline void
    MeldInterpretVMCore::execute_mvfieldreg (const unsigned char *pc, Register *reg) {
        ++pc;
        byte field_reg = FETCH(pc+1);
        byte field_num = FETCH(pc);

        tuple_t tpl = (tuple_t)reg[field_reg];
        Register *src = (Register*)eval_field (tpl, &pc);

        byte reg_index = FETCH(pc);
        Register *dst = (Register*)eval_reg (reg_index, &pc, reg);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE FIELD %d.%d TO reg %d\n",
                getBlockId(), field_reg, field_num,
                reg_index);
#else
        (void)field_num;
#endif
        size_t size = TYPE_ARG_SIZE(TUPLE_TYPE(tpl), field_num);
        memcpy(dst, src, size);
    }

    /* Moves value pointed at by a register to a field */
    inline void
    MeldInterpretVMCore::execute_mvregfield (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg_index = FETCH(pc);
        Register *src = (Register*)eval_reg (reg_index, &pc, reg);

        byte field_reg = FETCH(pc+1);
        byte field_num = FETCH(pc);

        tuple_t field_tpl = (tuple_t)reg[field_reg];
        tuple_type type = TUPLE_TYPE(field_tpl);
        Register *dst = (Register*)eval_field (field_tpl, &pc);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE REG %d TO FIELD %d.%d\n",
                getBlockId(), reg_index, field_reg, field_num);
#endif

        size_t size = TYPE_ARG_SIZE(type, field_num);

        memcpy(dst, src, size);
    }

    /* Moves content of a tuple field to another */
    inline void
    MeldInterpretVMCore::execute_mvfieldfield (const unsigned char *pc, Register *reg) {
        ++pc;
        byte src_field_reg = FETCH(pc+1);
        byte src_field_num = FETCH(pc);

        tuple_t src_field_tpl = (tuple_t)reg[src_field_reg];
        Register *src = (Register*)eval_field (src_field_tpl, &pc);

        byte dst_field_reg = FETCH(pc+1);
        byte dst_field_num = FETCH(pc);

        tuple_t dst_field_tpl = (tuple_t)reg[dst_field_reg];
        tuple_type type = TUPLE_TYPE(dst_field_tpl);
        Register *dst = (Register*)eval_field (dst_field_tpl, &pc);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE FIELD %d.%d TO FIELD %d.%d\n",
                getBlockId(), src_field_reg, src_field_num,
                dst_field_reg, dst_field_num);
#else
        (void) src_field_num;
#endif

        size_t size = TYPE_ARG_SIZE(type, dst_field_num);

        memcpy(dst, src, size);
    }

    /* Moves blockId to a tuple field */
    inline void
    MeldInterpretVMCore::execute_mvhostfield (const unsigned char *pc, Register *reg) {
        ++pc;

        Register *src = (Register*)EVAL_HOST;

        byte field_reg = FETCH(pc+1);
        byte field_num = FETCH(pc);

        tuple_t field_tpl = (tuple_t)reg[field_reg];
        tuple_type type = TUPLE_TYPE(field_tpl);
        Register *dst = (Register*)eval_field (field_tpl, &pc);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE HOST TO FIELD %d.%d\n",
                getBlockId(), field_reg, field_num);
#endif

        size_t size = TYPE_ARG_SIZE(type, field_num);

        memcpy(dst, src, size);
    }

    /* Moves blockId to a register */
    inline void
    MeldInterpretVMCore::execute_mvhostreg (const unsigned char *pc, Register *reg) {
        ++pc;

        Register *src = (Register*)EVAL_HOST;

        byte reg_index = FETCH(pc);
        Register *dst = (Register*)eval_reg (reg_index, &pc, reg);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE HOST TO reg %d\n",
                getBlockId(), reg_index);
#endif

        size_t size = sizeof(Register);

        memcpy(dst, src, size);
    }

    /* Moves content of a reg to another */
    inline void
    MeldInterpretVMCore::execute_mvregreg (const unsigned char *pc, Register *reg) {
        ++pc;

        byte src_reg_index = FETCH(pc);
        Register *src = (Register*)eval_reg (src_reg_index, &pc, reg);

        byte dst_reg_index = FETCH(pc);
        Register *dst = (Register*)eval_reg (dst_reg_index, &pc, reg);

#ifdef DEBUG_INSTRS
        printf ("--%d--\t MOVE REG %d TO REG %d\n",
                getBlockId(), src_reg_index, dst_reg_index);
#endif

        size_t size = sizeof(Register);

        memcpy(dst, src, size);
    }

    /* ************* OPERATION INSTRUCTIONS FUNCTIONS ************* */

    /* Perform boolean NOT operation */
    inline void
    MeldInterpretVMCore::execute_not (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);

        Register *arg = (Register*)eval_reg (reg1, &pc, reg);
        Register *dest = (Register*)eval_reg (reg2, &pc, reg);

        if (MELD_BOOL(arg) > 0)
            *dest = 0;
        else
            *dest = 1;

#ifdef DEBUG_INSTRS
        printf ("--%d--\t NOT reg %d TO reg %d\n",
                getBlockId(), reg1, reg2);
#endif
    }

    /* Perform boolean OR operation */
    inline void
    MeldInterpretVMCore::execute_boolor (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);

        *dest = (MELD_BOOL(arg1) | MELD_BOOL(arg2));

#ifdef DEBUG_INSTRS
        printf ("--%d--\t BOOL reg %d OR reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_boolequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);

        *dest = (MELD_BOOL(arg1) == MELD_BOOL(arg2));

#ifdef DEBUG_INSTRS
        printf ("--%d--\t BOOL reg %d EQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_boolnotequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);

        *dest = (MELD_BOOL(arg1) != MELD_BOOL(arg2));

#ifdef DEBUG_INSTRS
        printf ("--%d--\t BOOL reg %d NOT EQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    /* Compares two blockId and store the result to 'dest' */
    inline void
    MeldInterpretVMCore::execute_addrequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_NODE_ID(arg1) == MELD_NODE_ID(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t ADDR reg %d EQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    /* Compares two blockId and store the result to 'dest' */
    inline void
    MeldInterpretVMCore::execute_addrnotequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_NODE_ID(arg1) != MELD_NODE_ID(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t ADDR reg %d NOTEQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    /* Same with and int... */
    inline void
    MeldInterpretVMCore::execute_intequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) == MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d EQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intnotequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) != MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d NOTEQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intgreater (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) > MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d GREATER THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intgreaterequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) >= MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d GREATER/EQUAL THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intlesser (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) < MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d LESSER THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intlesserequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) <= MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d LESSER/EQUAL THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intmul (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) * MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d MULTIPLIED BY reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intdiv (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) / MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d DIVIDED BY reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intmod (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) % MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d MOD reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif

    }
    inline void
    MeldInterpretVMCore::execute_intplus (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) + MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d PLUS reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_intminus (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_INT(arg1) - MELD_INT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t INT reg %d MINUS reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatplus (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) + MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d PLUS reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatminus (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) - MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d MINUS reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatmul (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) * MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d MULTIPLIED BY reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatdiv (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) / MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d DIVIDED BY reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) == MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d EQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatnotequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) != MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d NOT EQUAL reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatlesser (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) < MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d LESSER THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatlesserequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) <= MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d LESSER/EQUAL THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatgreater (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) > MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d GREATER THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    inline void
    MeldInterpretVMCore::execute_floatgreaterequal (const unsigned char *pc, Register *reg) {
        ++pc;

        byte reg1 = FETCH(pc);
        byte reg2 = FETCH(pc+1);
        byte reg3 = FETCH(pc+2);

        Register *arg1 = (Register*)eval_reg (reg1, &pc, reg);
        Register *arg2 = (Register*)eval_reg (reg2, &pc, reg);
        Register *dest = (Register*)eval_reg (reg3, &pc, reg);
        *dest = (MELD_FLOAT(arg1) >= MELD_FLOAT(arg2));
#ifdef DEBUG_INSTRS
        printf ("--%d--\t FLOAT reg %d GREATER/EQUAL THAN reg %d TO reg %d\n",
                getBlockId(), reg1, reg2, reg3);
#endif
    }

    /* END OF INSTR EXECUTION FUNCTIONS */

    /* ************* QUEUE MANAGEMENT FUNCTIONS ************* */

    int
    MeldInterpretVMCore::queue_length (tuple_queue *queue) {
        int i;
        tuple_entry *entry = queue->head;

        for (i = 0; entry != NULL; entry = entry->next, i++);

        return i;
    }

    bool
    MeldInterpretVMCore::queue_is_empty(tuple_queue *queue) {
        return queue->head == NULL;
    }

    void
    MeldInterpretVMCore::queue_push_tuple(tuple_queue *queue, tuple_entry *entry) {
        if(queue->head == NULL)
            queue->head = queue->tail = entry;
        else {
            queue->tail->next = entry;
            queue->tail = entry;
        }
    }

    tuple_t
    MeldInterpretVMCore::queue_pop_tuple(tuple_queue *queue) {
        tuple_entry *entry = NULL;

        if (queue->head) {
            entry = queue->head;
            queue->head = queue->head->next;

            if (queue->head == NULL)
                queue->tail = NULL;
        }

        return entry;
    }

    tuple_t
    MeldInterpretVMCore::queue_dequeue_pos(tuple_queue *queue, tuple_entry **pos) {
        tuple_entry *entry = *pos;
        tuple_entry *next = (*pos)->next;
        queue ->length--;

        if (entry == queue->tail) {
            if(entry == queue->head)
                queue->tail = NULL;
            else
                queue->tail = (tuple_entry *)pos; /* previous */
        }

        *pos = next;

        tuple_t tuple = entry->tuple;
        free(entry);

        return tuple;
    }

    tuple_entry*
    MeldInterpretVMCore::queue_enqueue(tuple_queue *queue, tuple_t tuple, record_type isNew) {
        tuple_entry *entry = (tuple_entry*)malloc(sizeof(tuple_entry));

        entry->tuple = tuple;
        entry->records = isNew;
        entry->next = NULL;
        queue ->length++;

        queue_push_tuple(queue, entry);

        return entry;
    }

    tuple_t
    MeldInterpretVMCore::queue_dequeue(tuple_queue *queue, int *isNew) {
        tuple_entry *entry = (tuple_entry*)queue_pop_tuple(queue);
        queue ->length--;

        tuple_t tuple = entry->tuple;

        if(isNew)
            *isNew = entry->records.count;

        free(entry);

        return tuple;
    }

    tuple_pentry*
    MeldInterpretVMCore::p_dequeue(tuple_pqueue *q) {
        tuple_pentry *ret = q->queue;

        if(q->queue != NULL)
            q->queue = q->queue->next;

        return ret;
    }

    void
    MeldInterpretVMCore::p_enqueue(tuple_pqueue *queue, meld_int priority, tuple_t tuple,
              NodeID rt, record_type isNew) {
        tuple_pentry *entry = (tuple_pentry*)malloc(sizeof(tuple_pentry));

        entry->tuple = tuple;
        entry->records = isNew;
        entry->priority = priority;
        entry->rt = rt;

        tuple_pentry **spot;
        for (spot = &(queue->queue);
                *spot != NULL &&
                (*spot)->priority <= priority;
                spot = &((*spot)->next));

        entry->next = *spot;
        *spot = entry;
    }

    /* ************* VM INITIALIZATION FUNCTIONS ************* */

    static int type;
    void
    MeldInterpretVMCore::init_fields(void) {
        size_t total = 2*NUM_TYPES;
        int i, j;

        for(i = 0; i < NUM_TYPES; ++i)
            total += TYPE_NUMARGS(i) * 2;

        arguments = (unsigned char*)malloc(total);
        unsigned char *start = arguments + 2*NUM_TYPES;
        unsigned char offset, size;

        for(i = 0; i < NUM_TYPES; ++i) {
            arguments[i*2] = start - arguments; /* start */
            offset = 0;

            for(j = 0; j < TYPE_NUMARGS(i); ++j) {
                type = TYPE_ARG_TYPE(i, j);
                switch (type) {

                case (int)FIELD_INT:
                case (int)FIELD_TYPE:
                    size = sizeof(meld_int);
                    break;

                case (int)FIELD_FLOAT:
                    size = sizeof(meld_float);
                    break;

                case (int)FIELD_BOOL:
                    size = sizeof(byte);
                    break;

                case (int)FIELD_ADDR:
                    size = sizeof(NodeID);
                    break;

                case (int)FIELD_LIST_INT:
                case (int)FIELD_LIST_FLOAT:
                case (int)FIELD_LIST_ADDR:
                case (int)FIELD_SET_INT:
                case (int)FIELD_SET_FLOAT:
                case (int)FIELD_STRING:
                    size = sizeof(void*);
                    break;

                default:
                    assert(0);
                    size = 0;
                    break;
                }

                start[0] = size; /* argument size */
                start[1] = offset; /* argument offset */

                offset += size;
                start += 2;
            }
            arguments[i*2+1] = offset + TYPE_FIELD_SIZE; /* tuple size */
        }
    }

    /* Get TYPE id for uselful types */
    void MeldInterpretVMCore::init_consts(void) {
        tuple_type i;
        for (i = 0; i < NUM_TYPES; i++) {
            if(strcmp(TYPE_NAME(i), "edge") == 0)
                TYPE_EDGE = i;
            else if(strcmp(TYPE_NAME(i), "terminate") == 0)
                TYPE_TERMINATE = i;
        }
    }

    /* ************* AGGREGATE RELATED FUNCTIONS ************* */

    inline
    bool MeldInterpretVMCore::aggregate_accumulate(int agg_type, void *acc, void *obj, int count) {
        switch (agg_type) {
        case AGG_SET_UNION_INT:
        case AGG_SET_UNION_FLOAT:
            assert(false);
            return false;

        case AGG_FIRST:
            return false;

        case AGG_MAX_INT:
            if (MELD_INT(obj) > MELD_INT(acc)) {
                MELD_INT(acc) = MELD_INT(obj);
                return true;
            } else
                return false;

        case AGG_MIN_INT:
            if (MELD_INT(obj) < MELD_INT(acc)) {
                MELD_INT(acc) = MELD_INT(obj);
                return true;
            } else
                return false;

        case AGG_SUM_INT:
            MELD_INT(acc) += MELD_INT(obj) * count;
            return false;

        case AGG_MAX_FLOAT:
            if(MELD_FLOAT(obj) > MELD_FLOAT(acc)) {
                MELD_FLOAT(acc) = MELD_FLOAT(obj);
                return true;
            } else
                return false;

        case AGG_MIN_FLOAT:
            if(MELD_FLOAT(obj) < MELD_FLOAT(acc)) {
                MELD_FLOAT(acc) = MELD_FLOAT(obj);
                return true;
            } else
                return false;

        case AGG_SUM_FLOAT:
            MELD_FLOAT(acc) += MELD_FLOAT(obj) * (meld_float)count;
            return false;

        case AGG_SUM_LIST_INT:
        case AGG_SUM_LIST_FLOAT:
            assert(false);
            return false;
        }

        assert(0);
        while(1);
    }

    inline bool
    MeldInterpretVMCore::aggregate_changed(int agg_type, void *v1, void *v2) {
        switch(agg_type) {
        case AGG_FIRST:
            return false;

        case AGG_MIN_INT:
        case AGG_MAX_INT:
        case AGG_SUM_INT:
            return MELD_INT(v1) != MELD_INT(v2);

        case AGG_MIN_FLOAT:
        case AGG_MAX_FLOAT:
        case AGG_SUM_FLOAT:
            return MELD_FLOAT(v1) != MELD_FLOAT(v2);

        case AGG_SET_UNION_INT:
        case AGG_SET_UNION_FLOAT:
            return false;

        case AGG_SUM_LIST_INT:
        case AGG_SUM_LIST_FLOAT:
            return false;

        default:
            assert(0);
            return true;
        }

        assert(0);
        while(1);
    }

    inline void
    MeldInterpretVMCore::aggregate_seed(int agg_type, void *acc, void *start, int count, size_t size) {
        switch(agg_type) {
        case AGG_FIRST:
            memcpy(acc, start, size);
            return;
        case AGG_MIN_INT:
        case AGG_MAX_INT:
            MELD_INT(acc) = MELD_INT(start);
            return;
        case AGG_SUM_INT:
            MELD_INT(acc) = MELD_INT(start) * count;
            return;
        case AGG_MIN_FLOAT:
        case AGG_MAX_FLOAT:
            MELD_FLOAT(acc) = MELD_FLOAT(start);
            return;
        case AGG_SUM_FLOAT:
            MELD_FLOAT(acc) = MELD_FLOAT(start) * count;
            return;
        case AGG_SET_UNION_INT:
        case AGG_SET_UNION_FLOAT:
        case AGG_SUM_LIST_INT:
        case AGG_SUM_LIST_FLOAT:
            assert(false);
            return;
        }

        assert(0);
        while(1);
    }

    inline void
    MeldInterpretVMCore::aggregate_free(tuple_t tuple, unsigned char field_aggregate, unsigned char type_aggregate) {
        switch(type_aggregate) {
        case AGG_FIRST:
        case AGG_MIN_INT:
        case AGG_MAX_INT:
        case AGG_SUM_INT:
        case AGG_MIN_FLOAT:
        case AGG_MAX_FLOAT:
        case AGG_SUM_FLOAT:
            /* nothing to do */
            break;

        case AGG_SET_UNION_INT:
        case AGG_SET_UNION_FLOAT:
        case AGG_SUM_LIST_INT:
        case AGG_SUM_LIST_FLOAT:
            assert(false);
            break;

        default:
            assert(0);
            break;
        }
    }

    inline
    void MeldInterpretVMCore::aggregate_recalc(tuple_entry *agg, Register *reg, bool first_run) {
        tuple_type type = TUPLE_TYPE(agg->tuple);

        tuple_entry *cur;

        int agg_type = AGG_AGG(TYPE_AGGREGATE(type));
        int agg_field = AGG_FIELD(TYPE_AGGREGATE(type));
        tuple_queue *agg_queue = agg->records.agg_queue;
        tuple_entry *agg_list = agg_queue->head;
        tuple_t tuple = agg_list->tuple;

        void* start = GET_TUPLE_FIELD(tuple, agg_field);

        /* make copy */
        size_t size = TYPE_ARG_SIZE(type, agg_field);
        void* accumulator = malloc(size);

        aggregate_seed(agg_type, accumulator, start, agg_list->records.count, size);

        /* calculate offsets to copy right side to aggregated tuple */
        size_t size_offset = TYPE_FIELD_SIZE + TYPE_ARG_OFFSET(type, agg_field) + TYPE_ARG_SIZE(type, agg_field);
        size_t total_copy = TYPE_SIZE(type) - size_offset;
        tuple_t target_tuple = NULL;

        if (total_copy > 0)
            target_tuple = tuple;

        for (cur = agg_list->next; cur != NULL; cur = cur->next) {
            if(aggregate_accumulate(agg_type, accumulator,
                                    GET_TUPLE_FIELD(cur->tuple, agg_field), cur->records.count))
                target_tuple = cur->tuple;
        }

        void *acc_area = GET_TUPLE_FIELD(agg->tuple, agg_field);

        if(first_run)
            memcpy(acc_area, accumulator, size);
        else if (aggregate_changed(agg_type, acc_area, accumulator)) {
            process_bytecode(agg->tuple, TYPE_START(type), -1, NOT_LINEAR, reg, PROCESS_TUPLE);
            aggregate_free(agg->tuple, agg_field, agg_type);
            memcpy(acc_area, accumulator, size);
            if (total_copy > 0) /* copy right side from target tuple */
                memcpy(((unsigned char *)agg->tuple) + size_offset, ((unsigned char *)target_tuple) + size_offset, total_copy);
            process_bytecode(agg->tuple, TYPE_START(type), 1, NOT_LINEAR, reg, PROCESS_TUPLE);
        }

        free(accumulator);
    }

    /* ************* PROCESSING FUNCTIONS ************* */

    /* Handles a tuple, if it is a derivation tuple (isNew > 0):
     * -> Process tuple and add it to the database
     * If it is a retraction tuple (isNew < 0):
     * -> Iterate through the database and find corresponding tuple, dequeue and
     * process it with a isNew of -1 to perform retraction, and free both tuples.
     */
    void MeldInterpretVMCore::tuple_do_handle(tuple_type type, tuple_t tuple, int isNew, Register *reg) {
        if(type == TYPE_TERMINATE) {
            FREE_TUPLE(tuple);
            TERMINATE_CURRENT();
            return;
        }

#ifdef BBSIM
        if (isNew == 1) {
            pthread_mutex_lock(&(printMutex));
            fprintf(stderr, "\x1b[1;32m--%d--\tExecuting tuple ", getBlockId());
            tuple_print (tuple, stderr);
            fprintf(stderr, "\x1b[0m\n");
            pthread_mutex_unlock(&(printMutex));
        } else if (isNew == -1) {
            pthread_mutex_lock(&(printMutex));
            fprintf(stderr, "\x1b[1;31m--%d--\tDeleting tuple ", getBlockId());
            tuple_print (tuple, stderr);
            fprintf(stderr, "\x1b[0m\n");
            pthread_mutex_unlock(&(printMutex));
        }
#endif

        if (TYPE_IS_ACTION(type)) {
            if(isNew > 0)
                execute_run_action0(tuple, type, isNew);
            else
                FREE_TUPLE(tuple);
            return;
        }

        if (!TYPE_IS_AGG(type) || TYPE_IS_LINEAR(type)) {
            tuple_queue *queue = &TUPLES[type];
            tuple_entry** current;
            tuple_entry* cur;

            for (current = &queue->head;
                    *current != NULL;
                    current = &(*current)->next) {
                cur = *current;

                if (memcmp(cur->tuple,
                           tuple,
                           TYPE_SIZE(type)) == 0) {
                    cur->records.count += isNew;

                    if (cur->records.count <= 0) {
                        /* Remove fact from database */
                        if (!TYPE_IS_LINEAR(type))
                            process_bytecode(tuple, TYPE_START(TUPLE_TYPE(tuple)), isNew,
                                             NOT_LINEAR, reg, PROCESS_TUPLE);

                        fprintf(stdout,
                                "\x1b[1;32m--%d--\tDelete Iter success for  %s\x1b[0m\n",
                                getBlockId(), tuple_names[type]);
                        FREE_TUPLE(queue_dequeue_pos(queue, current));
                        /* Also free retraction fact */
                        FREE_TUPLE(tuple);

                        return;
                    }

                    if(isNew > 0 && !TYPE_IS_LINEAR(type)) {
                        /* tuple found, no need to rederive */
                        FREE_TUPLE(tuple);
                        return;
                    }
                }
            }

            // if deleting, return
            if (isNew <= 0) {
                fprintf(stdout, "\x1b[1;31m--%d--\tDelete Iter failure for %s\x1b[0m\n", getBlockId(), tuple_names[type]);
                FREE_TUPLE(tuple);
                return;
            }

            queue_enqueue(queue, tuple, (record_type) isNew);

            process_bytecode(tuple, TYPE_START(TUPLE_TYPE(tuple)),
                             isNew, TYPE_IS_LINEAR(TUPLE_TYPE(tuple)), reg, PROCESS_TUPLE);

            return;
        }

        unsigned char type_aggregate = TYPE_AGGREGATE(type);
        unsigned char field_aggregate = AGG_FIELD(type_aggregate);

        tuple_entry **current;
        tuple_entry *cur;
        tuple_queue *queue = &(TUPLES[type]);

        for (current = &queue->head;
                (*current) != NULL;
                current = &(*current)->next) {
            cur = *current;

            size_t sizeBegin = TYPE_FIELD_SIZE + TYPE_ARG_OFFSET(type, field_aggregate);
            char *start = (char*)(cur->tuple);

            if(memcmp(start, tuple, sizeBegin))
                continue;

            /*
            size_t sizeOffset = sizeBegin + TYPE_ARG_SIZE(type, field_aggregate);
             size_t sizeEnd = TYPE_SIZE(type) - sizeOffset;

             if (memcmp(start + sizeOffset, (char*)tuple + sizeOffset, sizeEnd))
             continue;*/

            tuple_queue *agg_queue = cur->records.agg_queue;

            /* AGG_FIRST aggregate optimization */
            if(AGG_AGG(type_aggregate) == AGG_FIRST
                    && isNew > 0
                    && !queue_is_empty(agg_queue)) {
                FREE_TUPLE(tuple);
                return;
            }

            tuple_entry** current2;
            tuple_entry* cur2;

            for (current2 = &agg_queue->head;
                    *current2 != NULL;
                    current2 = &(*current2)->next) {
                cur2 = *current2;

                if (memcmp(cur2->tuple, tuple, TYPE_SIZE(type)) == 0) {
                    cur2->records.count += isNew;

                    if (cur2->records.count <= 0) {
                        // remove it
                        FREE_TUPLE(queue_dequeue_pos(agg_queue, current2));

                        if (queue_is_empty(agg_queue)) {
                            /* aggregate is removed */
                            void *aggTuple = queue_dequeue_pos(queue, current);

                            /* delete queue */
                            free(agg_queue);

                            process_bytecode(aggTuple, TYPE_START(TUPLE_TYPE(aggTuple)),
                                             -1, NOT_LINEAR, reg, PROCESS_TUPLE);
                            aggregate_free(aggTuple, field_aggregate, AGG_AGG(type_aggregate));
                            FREE_TUPLE(aggTuple);
                        } else
                            aggregate_recalc(cur, reg, false);
                    } else
                        aggregate_recalc(cur, reg, false);

                    fprintf(stdout,
                            "\x1b[1;32m--%d--\tAgg delete Iter success for %s\x1b[0m\n",
                            getBlockId(), tuple_names[type]);

                    FREE_TUPLE(tuple);
                    return;
                }
            }

            // if deleting, return
            if (isNew <= 0) {
                fprintf(stdout,
                        "\x1b[1;32m--%d--\tAgg delete Iter failure for %s\x1b[0m\n",
                        getBlockId(), tuple_names[type]);

                FREE_TUPLE(tuple);
                return;
            }

            queue_enqueue(agg_queue, tuple, (record_type) isNew);
            aggregate_recalc(cur, reg, false);

            return;
        }

        // if deleting, return
        if (isNew <= 0) {
            FREE_TUPLE(tuple);
            return;
        }

        // So now we know we have a new tuple
        tuple_t tuple_cpy = ALLOC_TUPLE(TYPE_SIZE(type));
        memcpy(tuple_cpy, tuple, TYPE_SIZE(type));

        /* create aggregate queue */
        tuple_queue *agg_queue = (tuple_queue*)malloc(sizeof(tuple_queue));

        queue_init(agg_queue);

        queue_enqueue(agg_queue, tuple, (record_type) isNew);
        tuple_entry *entry =
            queue_enqueue(&TUPLES[type], tuple_cpy, (record_type)agg_queue);

        aggregate_recalc(entry, reg, true);
        process_bytecode(tuple, TYPE_START(type), isNew, NOT_LINEAR, reg, PROCESS_TUPLE);
    }

#ifdef LOG_DEBUG
#define MAX_NAME_SIZE 30
    void
    MeldInterpretVMCore::print_bytecode(const unsigned char *pc) {
        char  s[MAX_NAME_SIZE];

        snprintf(s, MAX_NAME_SIZE*sizeof(char), "%u", (unsigned) pc - (unsigned) meld_prog);
        printDebug(s);

        switch (*(const unsigned char*)pc) {
        case RETURN_INSTR: 		/* 0x0 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "RETURN_INSTR");
            break;
        case NEXT_INSTR: 		/* 0x1 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "NEXT_INSTR");
            break;
        case PERS_ITER_INSTR: 		/* 0x02 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "PERS_ITER_INSTR");
            break;
        case LINEAR_ITER_INSTR: 		/* 0x05 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "LINEAR_ITER_INSTR");
            break;
        case NOT_INSTR: 		/* 0x07 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "NOT_INSTR");
            break;
        case SEND_INSTR: 		/* 0x08 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "SEND_INSTR");
            break;
        case RULE_INSTR: 		/* 0x10 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "RULE_INSTR");
            break;
        case RULE_DONE_INSTR: 		/* 0x11 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "RULE_DONE_INSTR");
            break;
        case SEND_DELAY_INSTR: 		/* 0x15 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "SEND_DELAY_INSTR");
            break;
        case RETURN_LINEAR_INSTR:		/* 0xd0 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "RETURN_LINEAR_INSTR");
            break;
        case RETURN_DERIVED_INSTR:		/* 0xf0 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "RETURN_DERIVED_INSTR");
            break;
        case MVINTFIELD_INSTR: 		/* 0x1e */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVINTFIELD_INSTR");
            break;
        case MVFIELDFIELD_INSTR: 		/* 0x21 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVFIELDFIELD_INSTR");
            break;
        case MVFIELDREG_INSTR: 		/* 0x22 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVFIELDREG_INSTR");
            break;
        case MVPTRREG_INSTR: 		/* 0x23 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVPTRREG_INSTR");
            break;
        case MVREGFIELD_INSTR: 		/* 0x26 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVFIELDREG_INSTR");
        case MVHOSTFIELD_INSTR: 		/* 0x28 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVHOSTFIELD_INSTR");
            break;
        /* NOT TESTED */
        case MVFLOATFIELD_INSTR: 		/* 0x2d */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVFLOATFIELD_INSTR");
            break;
        /* NOT TESTED */
        case MVFLOATREG_INSTR: 		/* 0x2e */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVFLOATREG_INSTR");
            break;
        /* NOT TESTED */
        case MVHOSTREG_INSTR: 		/* 0x37 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVHOSTREG_INSTR");
            break;
        case ADDRNOTEQUAL_INSTR: 		/* 0x38 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "ADDRNOTEQUAL_INSTR");
            break;
        case ADDREQUAL_INSTR: 		/* 0x39 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "ADDREQUAL_INSTR");
            break;
        case INTMINUS_INSTR: 		/* 0x3a */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTMINUS_INSTR");
            break;
        case INTEQUAL_INSTR: 		/* 0x3b */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTEQUAL_INSTR");
            break;
        case INTNOTEQUAL_INSTR: 		/* 0x3c */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTNOTEQUAL_INSTR");
            break;

        case INTPLUS_INSTR: 		/* 0x3d */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTPLUS_INSTR");
            break;
        case INTLESSER_INSTR: 		/* 0x3e */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTLESSER_INSTR");
            break;

        case INTGREATEREQUAL_INSTR: 		/* 0x3f */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTGREATEREQUAL_INSTR");
            break;
        case ALLOC_INSTR: 		/* 0x40 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "ALLOC_INSTR");
            break;
        /* NOT TESTED */
        case BOOLOR_INSTR: 		/* 0x41 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "BOOLOR_INSTR");
            break;

        case INTLESSEREQUAL_INSTR: 		/* 0x42 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTLESSEREQUAL_INSTR");
            break;
        case INTGREATER_INSTR: 		/* 0x43 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTEGREATER_INSTR");
            break;
        case INTMUL_INSTR: 		/* 0x44 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTMUL_INSTR");
            break;
        case INTDIV_INSTR: 		/* 0x45 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTDIV_INSTR");
            break;
        case FLOATPLUS_INSTR: 		/* 0x46 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATPLUS_INSTR");
            break;
        case FLOATMINUS_INSTR: 		/* 0x47 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATMINUS_INSTR");
            break;
        case FLOATMUL_INSTR: 		/* 0x48 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATMUL_INSTR");
            break;
        case FLOATDIV_INSTR: 		/* 0x49 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATDIV_INSTR");
            break;
        case FLOATEQUAL_INSTR: 		/* 0x4a */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATEQUAL_INSTR");
            break;
        case FLOATNOTEQUAL_INSTR: 		/* 0x4b */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATNOTEQUAL_INSTR");
            break;
        case FLOATLESSER_INSTR: 		/* 0x4c */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATLESSER_INSTR");
            break;
        case FLOATLESSEREQUAL_INSTR: 	/* 0x4d */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATLESSEREQUAL_INSTR");
            break;
        case FLOATGREATER_INSTR: 		/* 0x4e */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATGREATER_INSTR");
            break;
        case FLOATGREATEREQUAL_INSTR: 	/* 0x4f */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "FLOATGREATEREQUAL_INSTR");
            break;
        case MVREGREG_INSTR: 		/* 0x50 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "MVREGREG_INSTR");
            break;
        case BOOLEQUAL_INSTR: 		/* 0x51 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "BOOLEQUAL_INSTR");
            break;

        case BOOLNOTEQUAL_INSTR: 		/* 0x51 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "BOOLNOTEQUAL_INSTR");
            break;
        case IF_INSTR: 		/* 0x60 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "IF_INSTR");
            break;
        case CALL1_INSTR: 		/* 0x69 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "CALL1_INSTR");
            break;
        case ADDLINEAR_INSTR: 		/* 0x77 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "ADDLINEAR_INSTR");
            break;
        case ADDPERS_INSTR: 		/* 0x78 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "ADDPERS_INSTR");
            break;
        case RUNACTION_INSTR: 		/* 0x79 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "RUNACTION_INSTR");
            break;
        case UPDATE_INSTR: 		/* 0x7b */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), " UPDATE_INSTR");
            break;
        case REMOVE_INSTR: 		/* 0x80 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "REMOVE_INSTR");
            break;
        case IF_ELSE_INSTR: 		/* 0x81 */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "IF_ELSE_");
            break;
        /* NOT TESTED */
        case JUMP_INSTR:
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "JUMP_INSTR");
            break;
        case INTMOD_INSTR: 		/* 0x3d */
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "INTMOD_INSTR");
            break;
        default:
            snprintf(s, MAX_NAME_SIZE*sizeof(char), "UNKNOWN");
        }
        printDebug(s);
    }
#endif

    int MeldInterpretVMCore::process_bytecode (tuple_t tuple, const unsigned char *pc, int isNew, int isLinear, Register *reg, byte state) {
#ifdef DEBUG_INSTRS

#ifdef BBSIM
        /* if (PROCESS_TYPE(state) == PROCESS_TUPLE) { */
        /*     pthread_mutex_lock(&(printMutex)); */
        /*     printf ("\n--%d--\tPROCESS TUPLE ", getBlockId()); */
        /*     tuple_print (tuple, stdout); */
        /*     printf ("\n"); */
        /*     pthread_mutex_unlock(&(printMutex)); */
        /*   } */
        /* #else */
        if (PROCESS_TYPE(state) == PROCESS_TUPLE)
            printf ("\n--%d--\tPROCESS TUPLE %s -- isNew = %d\n",
                    getBlockId(), tuple_names[TUPLE_TYPE(tuple)], isNew);
        else
#endif
            if (PROCESS_TYPE(state) == PROCESS_ITER) {
                printf ("--%d--\t PROCESS ITER %s\n", getBlockId(),
                        tuple_names[TUPLE_TYPE(tuple)]);
            }

        /* Dont't print if rule is persistent */
            else if (PROCESS_TYPE(state) == PROCESS_RULE) {
                if (!RULE_ISPERSISTENT(RULE_NUMBER(state)))
                    printf ("--%d--\t PROCESS RULE %d: %s\n", getBlockId(),
                            RULE_NUMBER(state), rule_names[RULE_NUMBER(state)]);

            } else
                printf ("\n--%d--\tERROR: UNKNOWN PROCESS TYPE\n", getBlockId());
#endif

        /* Move tuple to register 0 so it can be accessed */
        if (state == PROCESS_TUPLE)
            moveTupleToReg (0, tuple, reg);
        /* Only if process_bytecode not called by iter, */
        /* because otherwise the tuple is already in a register */

        for (;;) {
eval_loop:
#ifdef DEBUG_INSTRS
#ifdef LOG_DEBUG
            print_bytecode(pc);
#endif
#endif
            switch (*(const unsigned char*)pc) {
            case RETURN_INSTR: {	/* 0x0 */
#ifdef DEBUG_INSTRS
                if (!(PROCESS_TYPE(state) == PROCESS_RULE
                        && RULE_ISPERSISTENT(RULE_NUMBER(state))) )
                    printf ("--%d--\tRETURN\n", getBlockId());
#endif
                return RET_RET;
            }

            case NEXT_INSTR: {	/* 0x1 */
#ifdef DEBUG_INSTRS
                printf ("--%d--\t NEXT\n", getBlockId());
#endif
                return RET_NEXT;
            }

#define DECIDE_NEXT_ITER()                      \
		 if(ret == RET_LINEAR)                  \
  return RET_LINEAR;                  \
		 if(ret == RET_DERIVED && isLinear)     \
  return RET_DERIVED;                 \
		 if(ret == RET_RET)                     \
  return RET_RET;                     \
		 pc = npc; goto eval_loop;

            case PERS_ITER_INSTR: {	/* 0x02 */
                const byte *npc = pc + ITER_OUTER_JUMP(pc);
                const int ret = execute_iter (pc, reg, isNew, isLinear);
                DECIDE_NEXT_ITER();
            }

            case LINEAR_ITER_INSTR: {	/* 0x05 */
                const byte *npc = pc + ITER_OUTER_JUMP(pc);
                const int ret = execute_iter (pc, reg, isNew, isLinear);
                DECIDE_NEXT_ITER();
            }

            case NOT_INSTR: {	/* 0x07 */
                const byte *npc = pc + NOT_BASE;
                execute_not (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case SEND_INSTR: {	/* 0x08 */
                const byte *npc = pc + SEND_BASE;
                execute_send (pc, reg, isNew);
                pc = npc;
                goto eval_loop;
            }

            case RESET_LINEAR_INSTR: { /* 0x0e */
                int ret = process_bytecode(tuple, pc + RESET_LINEAR_BASE, isNew, NOT_LINEAR, reg, PROCESS_ITER);
                (void)ret;
                pc += RESET_LINEAR_JUMP(pc);
                goto eval_loop;
            }
            break;

            case END_LINEAR_INSTR: /* 0x0f */
                return RET_LINEAR;

            case RULE_INSTR: {	/* 0x10 */
                const byte *npc = pc + RULE_BASE;
#ifdef DEBUG_INSTRS
                byte rule_number = FETCH(++pc);
                printf ("--%d--\t RULE %d\n", getBlockId(),
                        rule_number);
#endif
                pc = npc;
                goto eval_loop;
            }

            case RULE_DONE_INSTR: {	/* 0x11 */
#ifdef DEBUG_INSTRS
                printf ("--%d--\t RULE DONE\n", getBlockId());
#endif
                const byte *npc = pc + RULE_DONE_BASE;
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case SEND_DELAY_INSTR: {	/* 0x15 */
                const byte *npc = pc + SEND_DELAY_BASE;
                execute_send_delay (pc, reg, isNew);
                pc = npc;
                goto eval_loop;
            }

            case RETURN_LINEAR_INSTR:		/* 0xd0 */
#ifdef DEBUG_INSTRS
                printf ("--%d--\tRETURN LINEAR\n", getBlockId());
#endif
                return RET_LINEAR;

            case RETURN_DERIVED_INSTR:		/* 0xf0 */
#ifdef DEBUG_INSTRS
                printf ("--%d--\tRETURN DERIVED\n", getBlockId());
#endif
                return RET_DERIVED;

            case MVINTFIELD_INSTR: {	/* 0x1e */
                const byte *npc = pc + MVINTFIELD_BASE;
                execute_mvintfield (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case MVINTREG_INSTR: {	/* 0x1f */
                const byte *npc = pc + MVINTREG_BASE;
                execute_mvintreg (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case MVFIELDFIELD_INSTR: {	/* 0x21 */
                const byte *npc = pc + MVFIELDFIELD_BASE;
                execute_mvfieldfield (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case MVFIELDREG_INSTR: {	/* 0x22 */
                const byte *npc = pc + MVFIELDREG_BASE;
                execute_mvfieldreg (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case MVPTRREG_INSTR: {	/* 0x23 */
                const byte *npc = pc + MVPTRREG_BASE;
#ifdef DEBUG_INSTRS
                printf ("--%d--\tMOVE PTR TO REG -- Do nothing\n", getBlockId());
#endif
                /* TODO: Do something if used elsewhere than axiom derivation */
                pc = npc;
                goto eval_loop;
            }

            case MVREGFIELD_INSTR: {	/* 0x26 */
                const byte *npc = pc + MVREGFIELD_BASE;
                execute_mvregfield (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case MVHOSTFIELD_INSTR: {	/* 0x28 */
                const byte *npc = pc + MVHOSTFIELD_BASE;
                execute_mvhostfield (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case MVFLOATFIELD_INSTR: {	/* 0x2d */
                const byte *npc = pc + MVFLOATFIELD_BASE;
                execute_mvfloatfield (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case MVFLOATREG_INSTR: {	/* 0x2e */
                const byte *npc = pc + MVFLOATREG_BASE;
                execute_mvfloatreg (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case MVHOSTREG_INSTR: {	/* 0x37 */
                const byte *npc = pc + MVHOSTREG_BASE;
                execute_mvhostreg (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case ADDRNOTEQUAL_INSTR: {	/* 0x38 */
                const byte *npc = pc + OP_BASE;
                execute_addrnotequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case ADDREQUAL_INSTR: {	/* 0x39 */
                const byte *npc = pc + OP_BASE;
                execute_addrequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTMINUS_INSTR: {	/* 0x3a */
                const byte *npc = pc + OP_BASE;
                execute_intminus (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTEQUAL_INSTR: {	/* 0x3b */
                const byte *npc = pc + OP_BASE;
                execute_intequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTNOTEQUAL_INSTR: {	/* 0x3c */
                const byte *npc = pc + OP_BASE;
                execute_intnotequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTPLUS_INSTR: {	/* 0x3d */
                const byte *npc = pc + OP_BASE;
                execute_intplus (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTLESSER_INSTR: {	/* 0x3e */
                const byte *npc = pc + OP_BASE;
                execute_intlesser (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTGREATEREQUAL_INSTR: {	/* 0x3f */
                const byte *npc = pc + OP_BASE;
                execute_intgreaterequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case ALLOC_INSTR: {	/* 0x40 */
                const byte *npc = pc + ALLOC_BASE;
                execute_alloc (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case BOOLOR_INSTR: {	/* 0x41 */
                const byte *npc = pc + OP_BASE;
                execute_boolor (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTLESSEREQUAL_INSTR: {	/* 0x42 */
                const byte *npc = pc + OP_BASE;
                execute_intlesserequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTGREATER_INSTR: {	/* 0x43 */
                const byte *npc = pc + OP_BASE;
                execute_intgreater (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTMUL_INSTR: {	/* 0x44 */
                const byte *npc = pc + OP_BASE;
                execute_intmul (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case INTDIV_INSTR: {	/* 0x45 */
                const byte *npc = pc + OP_BASE;
                execute_intdiv (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATPLUS_INSTR: {	/* 0x46 */
                const byte *npc = pc + OP_BASE;
                execute_floatplus (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATMINUS_INSTR: {	/* 0x47 */
                const byte *npc = pc + OP_BASE;
                execute_floatminus (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATMUL_INSTR: {	/* 0x48 */
                const byte *npc = pc + OP_BASE;
                execute_floatmul (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATDIV_INSTR: {	/* 0x49 */
                const byte *npc = pc + OP_BASE;
                execute_floatdiv (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATEQUAL_INSTR: {	/* 0x4a */
                const byte *npc = pc + OP_BASE;
                execute_floatequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATNOTEQUAL_INSTR: {	/* 0x4b */
                const byte *npc = pc + OP_BASE;
                execute_floatnotequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATLESSER_INSTR: {	/* 0x4c */
                const byte *npc = pc + OP_BASE;
                execute_floatlesser (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATLESSEREQUAL_INSTR: {	/* 0x4d */
                const byte *npc = pc + OP_BASE;
                execute_floatlesserequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATGREATER_INSTR: {	/* 0x4e */
                const byte *npc = pc + OP_BASE;
                execute_floatgreater (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case FLOATGREATEREQUAL_INSTR: {	/* 0x4f */
                const byte *npc = pc + OP_BASE;
                execute_floatgreaterequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case MVREGREG_INSTR: {	/* 0x50 */
                const byte *npc = pc + MVREGREG_BASE;
                execute_mvregreg (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case BOOLEQUAL_INSTR: {	/* 0x51 */
                const byte *npc = pc + OP_BASE;
                execute_boolequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            case BOOLNOTEQUAL_INSTR: {	/* 0x51 */
                const byte *npc = pc + OP_BASE;;
                execute_boolnotequal (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case IF_INSTR: {	/* 0x60 */
                const byte *npc = pc + IF_BASE;
                byte *base = (byte*)pc;
                ++pc;
                byte reg_index = FETCH(pc);
                Register *if_reg = (Register*)eval_reg (reg_index, &pc, reg);

                if (!(unsigned char)(*if_reg)) {
#ifdef DEBUG_INSTRS
                    printf ("--%d--\t IF (reg %d) -- Failed\n",
                            getBlockId(), reg_index);
#endif

                    pc = base + IF_JUMP(pc);
                    goto eval_loop;
                }
                /* else process if content */
#ifdef DEBUG_INSTRS
                printf ("--%d--\t IF (reg %d) -- Success\n",
                        getBlockId(), reg_index);
#endif
                pc = npc;
                goto eval_loop;
            }

            case CALL1_INSTR: {	/* 0x69 */
                const byte *npc = pc + CALL1_BASE;
                execute_call1 (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case ADDLINEAR_INSTR: {	/* 0x77 */
                const byte *npc = pc + ADDLINEAR_BASE;
                execute_addtuple (pc, reg, isNew);
                pc = npc;
                goto eval_loop;
            }

            case ADDPERS_INSTR: {	/* 0x78 */
                const byte *npc = pc + ADDPERS_BASE;
                execute_addtuple (pc, reg, isNew);
                pc = npc;
                goto eval_loop;
            }

            case RUNACTION_INSTR: {	/* 0x79 */
                const byte *npc = pc + RUNACTION_BASE;
                execute_run_action (pc, reg, isNew);
                pc = npc;
                goto eval_loop;
            }

            case UPDATE_INSTR: {	/* 0x7b */
                const byte *npc = pc + UPDATE_BASE;
                if (PROCESS_TYPE(state) == PROCESS_ITER)
                    execute_update (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            case REMOVE_INSTR: {	/* 0x80 */
                const byte *npc = pc + REMOVE_BASE;
                execute_remove (pc, reg, isNew);
                pc = npc;
                goto eval_loop;
            }

            /* NOT TESTED */
            /* CAUTION: I have no way to ensure that it is the correct way to handle
             * this instruction at this moment, please review this when you encounter it.
             */
            case IF_ELSE_INSTR: {	/* 0x81 */
                const byte *npc = pc + IF_ELSE_BASE;
                byte *base = (byte*)pc;
                ++pc;
                byte reg_index = FETCH(pc);
                Register *if_reg = (Register*)eval_reg (reg_index, &pc, reg);

                /* If false, jump to else */
                if (!(unsigned char)(*if_reg)) {
#ifdef DEBUG_INSTRS
                    printf ("--%d--\t IF_ELSE (reg %d) -- ELSE\n",
                            getBlockId(), reg_index);
#endif

                    pc = base + IF_JUMP(pc);
                    goto eval_loop;
                } else {
                    /* Else, process if until a jump instruction is encountered
                     * (it seems...)
                     */
#ifdef DEBUG_INSTRS
                    printf ("--%d--\t IF_ELSE (reg %d) -- ELSE\n",
                            getBlockId(), reg_index);
#endif

                    pc = npc;
                    goto eval_loop;
                }
            }

            /* NOT TESTED */
            case JUMP_INSTR: {
                ++pc;
#ifdef DEBUG_INSTRS
                printf ("--%d--\t JUMP TO\n", getBlockId());
#endif
                pc += JUMP_BASE + IF_JUMP(pc);
                goto eval_loop;
            }

            case INTMOD_INSTR: {	/* 0x3d */
                const byte *npc = pc + OP_BASE;
                execute_intmod (pc, reg);
                pc = npc;
                goto eval_loop;
            }

            default:
                printf ("--%d--\t "
                        "INSTRUCTION NOT IMPLEMENTED YET: %#x %#x %#x %#x %#x\n",
                        getBlockId(),
                        (unsigned char)*pc, (unsigned char)*(pc+1),
                        (unsigned char)*(pc+2), (unsigned char)*(pc+3),
                        (unsigned char)*(pc+4));

                exit(-2);
            }
        }
    }

#define MAX_STRING_SIZE 200
    /* Prints a tuple */
    void
    MeldInterpretVMCore::tuple_print(tuple_t tuple, FILE *fp) {
        unsigned char tuple_type = TUPLE_TYPE(tuple);
        int j;
        char str[MAX_STRING_SIZE];
        char tmp[MAX_STRING_SIZE];

        sprintf(str,"%s(", TYPE_NAME(tuple_type));

        for(j = 0; j < TYPE_NUMARGS(tuple_type); ++j) {
            void *field = GET_TUPLE_FIELD(tuple, j);

            if (j > 0)
                strcat(str, ", ");

            switch(TYPE_ARG_TYPE(tuple_type, j)) {
            case FIELD_INT:
                sprintf(tmp, "%d", MELD_INT(field));
                break;
            case FIELD_FLOAT:
                sprintf(tmp, "%f", (double)MELD_FLOAT(field));
                break;
            case FIELD_ADDR:
                sprintf(tmp, "%u", *(uint16_t*)field);
                break;
            case FIELD_LIST_INT:
            case FIELD_LIST_FLOAT:
            case FIELD_LIST_ADDR:
            case FIELD_SET_INT:
            case FIELD_SET_FLOAT:
                assert(false);
                break;
            case FIELD_TYPE:
                sprintf(tmp, "%s", TYPE_NAME(MELD_INT(field)));
                break;
            case FIELD_BOOL:
                if (MELD_BOOL(field))
                    sprintf(tmp, "true");
                else
                    sprintf(tmp, "false");
                break;
            default:
                assert(0);
                break;
            }
            strcat(str,tmp);
        }
        strcat(str, ")");
#ifdef LOG_DEBUG
        printDebug(str);
#else
        fprintf(fp,"%s",str);
#endif
    }

    /* Prints the content of the whole database */
    void MeldInterpretVMCore::facts_dump(void) {
        int i;
        char str[MAX_STRING_SIZE];
        char tmp[MAX_STRING_SIZE];

        for (i = 0; i < NUM_TYPES; i++) {
            // don't print fact types that don't exist
            if (TUPLES[i].head == NULL)
                continue;

            // don't print artificial tuple types
            /*
              if (tuple_names[i][0] == '_')
              continue;
            */

            snprintf(str,MAX_STRING_SIZE*sizeof(char),
                     "tuple %s (type %d, size: %d)",
                     tuple_names[i], i, TYPE_SIZE(i));
#ifdef LOG_DEBUG
            printDebug(str);
#else
            fprintf(stderr, "%s\n",str);
#endif
            tuple_entry *tupleEntry;
            for (tupleEntry = TUPLES[i].head; tupleEntry != NULL; tupleEntry = tupleEntry->next) {
                sprintf(str,"  ");
                tuple_print(tupleEntry->tuple, stderr);
                if (TYPE_IS_AGG(i)) {
                    strcat(str, "\n    [[[");
                    tuple_entry *tpE;
                    for (tpE = tupleEntry->records.agg_queue->head;
                            tpE != NULL;
                            tpE = tpE->next) {
                        tuple_print(tpE->tuple, stderr);
                        sprintf(tmp, "x%d\n       ", tpE->records.count);
                        strcat(str,tmp);
                    }
                    strcat(str, "\b\b\b]]]");
#ifdef LOG_DEBUG
                    printDebug(str);
#else
                    fprintf(stderr, "%s\n",str);
#endif
                } else {
                    sprintf(str, "x%d", tupleEntry->records.count);
#ifdef LOG_DEBUG
                    printDebug(str);
#else
                    fprintf(stderr, "%s\n",str);
#endif
                }
            }
        }
    }

    /* Print program info, this was designed for the oldVM, not sure if it works or not,
     * looks like it should.
     */
    void
    MeldInterpretVMCore::print_program_info(void) {
        int i;
        char s[MAX_STRING_SIZE];
        char tmp[MAX_STRING_SIZE];

        for(i = 0; i < NUM_TYPES; ++i) {
            sprintf(s, "Tuple (%s:%d:%d) ", tuple_names[i], i, TYPE_SIZE(i));
            //printf("Tuple (%s:%d:%d) ", tuple_names[i], i, TYPE_SIZE(i));

            strcat(s,"[");
            if(TYPE_IS_AGG(i))
                strcat(s,"agg");
            if(TYPE_IS_LINEAR(i))
                strcat(s,"linear");
            else
                strcat(s,"per");
            if(TYPE_IS_ROUTING(i))
                strcat(s,"route");
            strcat(s,"] ");


            //printf("num_args:%d off:%d ; args(offset, arg_size): ",
            //	   TYPE_NUMARGS(i), TYPE_OFFSET(i));

            snprintf(tmp, MAX_STRING_SIZE*sizeof(char),"num_args:%d off:%d ; args(offset, arg_size): ",
                     TYPE_NUMARGS(i), TYPE_OFFSET(i));
            strcat(s,tmp);

            int j;
            for (j = 0; j < TYPE_NUMARGS(i); ++j) {
                //printf(" %d:%d", TYPE_ARG_OFFSET(i, j), TYPE_ARG_SIZE(i, j));
                snprintf(tmp, MAX_STRING_SIZE*sizeof(char), " %d:%d", TYPE_ARG_OFFSET(i, j), TYPE_ARG_SIZE(i, j));
                strcat(s,tmp);
            }

            s[MAX_STRING_SIZE-1] = '\0';
#ifdef LOG_DEBUG
            printDebug(s);
#else
            printf("%s\n",s);
#endif
        }
    }

    /* If activated, call each blockTick to ensure that tuples such as vacant, neighbor,
     * or neighborCount or don't have duplicates in the database.
     * i.e. numberOfVacant + numberOfNeighbor < NUM_PORTS :
     * and numberOfNeighborCount == 1 are two conditions which must always be true
     */
    void
    MeldInterpretVMCore::databaseConsistencyChecker() {
        int i;
        byte neighborTCount = 0;
        byte vacantTCount = 0;
        for (i = 0; i < NUM_TYPES; i++) {
            if (TUPLES[i].head == NULL)
                continue;

            byte tupleCount = 0;
            tuple_entry *tupleEntry;
            for (tupleEntry = TUPLES[i].head;
                    tupleEntry != NULL;
                    tupleEntry = tupleEntry->next) {
                if (i == TYPE_NEIGHBOR)
                    ++neighborTCount;
                else if (i == TYPE_VACANT)
                    ++vacantTCount;
                else
                    ++tupleCount;
            }

            if (i == TYPE_NEIGHBORCOUNT) {
                if (!tupleCount > 1) {
                    fprintf(stderr, "\x1b[1;32m--%d--\ttuple %s (type %d, size: %d), count = %d\x1b[0m\n", getBlockId(), tuple_names[i], i, TYPE_SIZE(i), tupleCount);
                    /* *(char *)0 = 0; */
                }
            } else {
                if ( (neighborTCount + vacantTCount) > 6) {
                    fprintf(stderr, "\x1b[31m--%d--\tToo many port tuples! count = %dn +%dv\x1b[0m\n", getBlockId(), neighborTCount, vacantTCount);
                }
                /* *(char *)0 = 0; */
            }
        }
    }
}

