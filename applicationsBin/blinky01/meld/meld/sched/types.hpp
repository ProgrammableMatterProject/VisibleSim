
#ifndef SCHED_TYPES_HPP
#define SCHED_TYPES_HPP

#include "conf.hpp"

namespace sched
{
   
enum scheduler_type {
   SCHED_UNKNOWN,
   SCHED_SERIAL,
	SCHED_SERIAL_UI
#ifdef USE_SIM
	, SCHED_SIM
#endif

};

inline bool is_serial_sched(const scheduler_type type)
{

  return type == SCHED_SERIAL || type == SCHED_SERIAL_UI
#ifdef USE_SIM
      || type == SCHED_SIM
#endif
      ;

}
}
#endif
