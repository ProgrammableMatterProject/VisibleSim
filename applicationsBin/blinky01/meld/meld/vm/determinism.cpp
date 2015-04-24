#include "vm/determinism.hpp"
#include "api/api.hpp"

// INSTRUCTIONS CYCLES
#define NB_INSTR 250 // used for cycle table instantiation, instr enum is not continuous
#define RETURN_INSTR_CYCLES 100
#define NEXT_INSTR_CYCLES 57.16
#define ELSE_INSTR_CYCLES 100
#define TEST_NIL_INSTR_CYCLES 100
#define CONS_INSTR_CYCLES 100
#define HEAD_INSTR_CYCLES 100
#define TAIL_INSTR_CYCLES 100
#define NOT_INSTR_CYCLES 57.16 // NOP?
#define SEND_INSTR_CYCLES 100
#define FLOAT_INSTR_CYCLES 100
#define SELECT_INSTR_CYCLES 100
#define RETURN_SELECT_INSTR_CYCLES 100
#define COLOCATED_INSTR_CYCLES 100
#define DELETE_INSTR_CYCLES 100
#define RESET_LINEAR_INSTR_CYCLES 100
#define END_LINEAR_INSTR_CYCLES 100
#define RULE_INSTR_CYCLES 100
#define RULE_DONE_INSTR_CYCLES 100
#define NEW_NODE_INSTR_CYCLES 100
#define NEW_AXIOMS_INSTR_CYCLES 100
#define SEND_DELAY_INSTR_CYCLES 100
#define PUSH_INSTR_CYCLES 100
#define POP_INSTR_CYCLES 100
#define PUSH_REGS_INSTR_CYCLES 100
#define POP_REGS_INSTR_CYCLES 100
#define CALLF_INSTR_CYCLES 100
#define CALLE_INSTR_CYCLES 100
#define CALL_INSTR_CYCLES 100
#define MOVE_INSTR_CYCLES 129.88
#define ALLOC_INSTR_CYCLES 396.03
#define IF_INSTR_CYCLES 100
#define MOVE_NIL_INSTR_CYCLES 175
#define REMOVE_INSTR_CYCLES 100
#define ITER_INSTR_CYCLES 100
#define OP_INSTR_CYCLES 100
#define RETURN_LINEAR_INSTR_CYCLES 100
#define RETURN_DERIVED_INSTR_CYCLES 100

// ARITHMETIC & LOGIC OPERATION CYCLES
#define NB_OP 26 // used for the cycle table instantiation
#define OP_NEQF_CYCLES 411.86
#define OP_NEQI_CYCLES 366.97
#define OP_EQF_CYCLES 400.86
#define OP_EQI_CYCLES 364.97
#define OP_LESSF_CYCLES 400
#define OP_LESSI_CYCLES 363.97
#define OP_LESSEQF_CYCLES 400.86
#define OP_LESSEQI_CYCLES 363.97
#define OP_GREATERF_CYCLES 411.86
#define OP_GREATERI_CYCLES 369.97
#define OP_GREATEREQF_CYCLES 411.86
#define OP_GREATEREQI_CYCLES 367.97
#define OP_MODF_CYCLES 1005.13
#define OP_MODI_CYCLES 1093.65
#define OP_PLUSF_CYCLES 451.71
#define OP_PLUSI_CYCLES 361.97
#define OP_MINUSF_CYCLES 456.71
#define OP_MINUSI_CYCLES 359.97
#define OP_TIMESF_CYCLES 502.25
#define OP_TIMESI_CYCLES 408.86
#define OP_DIVF_CYCLES 956.37
#define OP_DIVI_CYCLES 1093.65
#define OP_NEQA_CYCLES 50
#define OP_EQA_CYCLES 50
#define OP_GREATERA_CYCLES 50
#define OP_ORB_CYCLES 50

// Frequency unit: MHz
#define ATMEL_ATXMEGA256A3_FREQUENCY 32// Blinky Block microcontroler
#define CPU_FRENQUENCY ATMEL_ATXMEGA256A3_FREQUENCY

using namespace std;
using namespace vm::instr;

namespace vm {
	
	namespace determinism {
	
		static uint* initOpCycleTab();
	    static uint* initInstrCycleTab();
	    
	    /* TIME :
	     * unit : microseconds
	     * 
	     * constraints : 
	     * uint64_t 1.8*10^19, 
		 * double 1.7*10^308 : 64 bits with 52 bits for the mantissa
		 *		and 11 bits for the exponent.
		 */
		static double currentLocalTime = 0; // in microseconds
		static simulationMode mode = REALTIME;
	  //NOTUSED??? static bool computing = false;	
		static uint *opCycleTab = initOpCycleTab();
		static uint *instrCycleTab = initInstrCycleTab();

		void setSimulationDeterministicMode() {
			mode = DETERMINISTIC;
		}
		
		simulationMode getSimulationMode() {
			return mode;
		}
		
		bool isInDeterministicMode() {
			return (mode == DETERMINISTIC);
		}
		
		void workEnd() {
			api::workEnd();
		}
		
		deterministic_timestamp getCurrentLocalTime() {
			return (deterministic_timestamp)currentLocalTime;
		}
		
		uint* initOpCycleTab() {
			uint *t = new uint[NB_OP];
			t[OP_NEQF] = OP_NEQF_CYCLES;
			t[OP_NEQI] = OP_NEQI_CYCLES;
			t[OP_EQF] = OP_EQF_CYCLES;
			t[OP_EQI] = OP_EQI_CYCLES;
			t[OP_LESSF] = OP_LESSF_CYCLES;
			t[OP_LESSI] = OP_LESSI_CYCLES;
			t[OP_LESSEQF] = OP_LESSEQF_CYCLES;
			t[OP_LESSEQI] = OP_LESSEQI_CYCLES;
			t[OP_GREATERF] = OP_GREATERF_CYCLES;
			t[OP_GREATERI] = OP_GREATERI_CYCLES;
			t[OP_GREATEREQF] = OP_GREATEREQF_CYCLES;
			t[OP_GREATEREQI] = OP_GREATEREQI_CYCLES;
			t[OP_MODF] = OP_MODF_CYCLES;
			t[OP_MODI] = OP_MODI_CYCLES;
			t[OP_PLUSF] = OP_PLUSF_CYCLES;
			t[OP_PLUSI] = OP_PLUSI_CYCLES;
			t[OP_MINUSF] = OP_MINUSF_CYCLES;
			t[OP_MINUSI] = OP_MINUSI_CYCLES;
			t[OP_TIMESF] = OP_TIMESF_CYCLES;
			t[OP_TIMESI] = OP_TIMESI_CYCLES;
			t[OP_DIVF] = OP_DIVF_CYCLES;
			t[OP_DIVI] = OP_DIVI_CYCLES;
			t[OP_NEQA] = OP_NEQA_CYCLES;
			t[OP_EQA] = OP_EQA_CYCLES;
			t[OP_GREATERA] = OP_GREATERA_CYCLES;
			t[OP_ORB] = OP_ORB_CYCLES;
			return t;
		}
		
		uint* initInstrCycleTab() {
			uint *t = new uint[NB_INSTR];
			for(uint i=0; i < NB_INSTR; i++) {
				t[i] = 0;
			}
			t[RETURN_INSTR] = RETURN_INSTR_CYCLES;
			t[NEXT_INSTR] = NEXT_INSTR_CYCLES;
			t[ELSE_INSTR] = ELSE_INSTR_CYCLES;
			t[TEST_NIL_INSTR] = TEST_NIL_INSTR_CYCLES;
			t[CONS_INSTR] = CONS_INSTR_CYCLES;
			t[HEAD_INSTR] = HEAD_INSTR_CYCLES;
			t[TAIL_INSTR] = TAIL_INSTR_CYCLES;
			t[NOT_INSTR] = NOT_INSTR_CYCLES;
			t[SEND_INSTR] = SEND_INSTR_CYCLES;
			t[FLOAT_INSTR] = FLOAT_INSTR_CYCLES;
			t[SELECT_INSTR] = SELECT_INSTR_CYCLES;
			t[RETURN_SELECT_INSTR] = RETURN_SELECT_INSTR_CYCLES;
			t[COLOCATED_INSTR] = COLOCATED_INSTR_CYCLES;
			t[DELETE_INSTR] = DELETE_INSTR_CYCLES;
			t[RESET_LINEAR_INSTR] = RESET_LINEAR_INSTR_CYCLES;
			t[END_LINEAR_INSTR] = END_LINEAR_INSTR_CYCLES;
			t[RULE_INSTR] = RULE_INSTR_CYCLES;
			t[RULE_DONE_INSTR] = RULE_DONE_INSTR_CYCLES;
			t[NEW_NODE_INSTR] = NEW_NODE_INSTR_CYCLES;
			t[NEW_AXIOMS_INSTR] = NEW_AXIOMS_INSTR_CYCLES;
			t[SEND_DELAY_INSTR] = SEND_DELAY_INSTR_CYCLES;
			t[PUSH_INSTR] = PUSH_INSTR_CYCLES;
			t[POP_INSTR] = POP_INSTR_CYCLES;
			t[PUSH_REGS_INSTR] = PUSH_REGS_INSTR_CYCLES;
			t[POP_REGS_INSTR] = POP_REGS_INSTR_CYCLES;
			t[CALLF_INSTR] = CALLF_INSTR_CYCLES;
			t[CALLE_INSTR] = CALLE_INSTR_CYCLES;
			t[CALL_INSTR] = CALL_INSTR_CYCLES;
			t[MOVE_INSTR] = MOVE_INSTR_CYCLES;
			t[ALLOC_INSTR] = ALLOC_INSTR_CYCLES;
			t[IF_INSTR] = IF_INSTR_CYCLES;
			t[MOVE_NIL_INSTR] = MOVE_NIL_INSTR_CYCLES;
			t[REMOVE_INSTR] = REMOVE_INSTR_CYCLES;
			t[ITER_INSTR] = ITER_INSTR_CYCLES;
			t[OP_INSTR] = OP_INSTR_CYCLES;
			t[RETURN_LINEAR_INSTR] = RETURN_LINEAR_INSTR_CYCLES;
			t[RETURN_DERIVED_INSTR] = RETURN_DERIVED_INSTR_CYCLES;
			return t;
		}
		
		void incrCurrentLocalTime(pcounter pc){
			uint cycles = 0;
			instr_type inst = fetch(pc);
			if(inst == OP_INSTR) {
				cycles = opCycleTab[op_op(pc)];
			} else {
				cycles = instrCycleTab[inst];
			}
			currentLocalTime += cycles/CPU_FRENQUENCY;
		}
		
		void setCurrentLocalTime(deterministic_timestamp time) {
			currentLocalTime = max(currentLocalTime, (double)time);
		}
	}
}
