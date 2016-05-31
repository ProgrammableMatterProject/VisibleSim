
#ifndef SCHED_COMMON_HPP
#define SCHED_COMMON_HPP

#include "vm/defs.hpp"
#include "vm/all.hpp"

namespace sched
{

#ifdef NDEBUG
inline void assert_static_nodes_end_iteration(const vm::process_id, vm::all *) {}
inline void assert_static_nodes_end(const vm::process_id, vm::all *) {}
#else
void assert_static_nodes_end_iteration(const vm::process_id, vm::all *);
void assert_static_nodes_end(const vm::process_id, vm::all *);
#endif

#define iterate_static_nodes()                                                       \
  database::map_nodes::const_iterator it(vm::All->DATABASE->nodes_begin()); \
  database::map_nodes::const_iterator end(vm::All->DATABASE->nodes_end()); \
   for(; it != end; ++it)                                                              \
      node_iteration(it->second)
}

#endif
