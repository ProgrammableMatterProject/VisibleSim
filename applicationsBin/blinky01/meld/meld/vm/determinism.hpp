#ifndef VM_DETERMINISM_HPP
#define VM_DETERMINISM_HPP

#include <cstdio>
#include "db/node.hpp"
#include "vm/instr.hpp"

namespace vm {
	
	namespace determinism {
		typedef size_t deterministic_timestamp;
		enum simulationMode {DETERMINISTIC = 1, REALTIME = 2};
		
		void setSimulationDeterministicMode();
		simulationMode getSimulationMode();
		bool isInDeterministicMode();
		
		void workEnd();

		deterministic_timestamp getCurrentLocalTime();
		void incrCurrentLocalTime(pcounter pc);
		void setCurrentLocalTime(deterministic_timestamp time);
	}
}
#endif
