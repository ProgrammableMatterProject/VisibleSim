#include "db/database.hpp"
#include "db/node.hpp"
#include "sched/serial.hpp"
#include "sched/common.hpp"
#include "api/api.hpp"

using namespace db;
using namespace vm;
using namespace process;
using namespace std;
using namespace api;

namespace sched
{

  void
  serial_local::new_agg(work& w)
  {
    new_work(w.get_node(), w);
  }
  
/*sending work to another node "to"*/
void
serial_local::new_work(const node *, work& new_work)
{

   serial_node *to(dynamic_cast<serial_node*>(new_work.get_node()));

   to->add_work(new_work.get_tuple());

   if(!to->in_queue()) {
      to->set_in_queue(true);
      queue_nodes.push(to);
   }
}

  void
  serial_local::assert_end(void) const
  {
    assert(!has_work());
    assert_static_nodes_end(id, vm::All);
  }

  void
  serial_local::assert_end_iteration(void) const
  {
    assert(!has_work());
    assert_static_nodes_end_iteration(id, vm::All);
  }


// Look through the scheduler's queue finding any node that has new facts to
// process
node*
  serial_local::get_work(void)
  {
    if(current_node != NULL) {
      if(!current_node->has_work()) {
current_node->set_in_queue(false);
current_node = NULL;
if(!has_work())
return NULL;
if(!queue_nodes.pop(current_node))
return NULL;
      }
    } else {
      if(!has_work())
return NULL;
      if(!queue_nodes.pop(current_node))
return NULL;
      assert(current_node->has_work());
    }

    assert(current_node != NULL);
    assert(current_node->has_work());
    assert(current_node->in_queue());

    return current_node;
  }

  bool
  serial_local::terminate_iteration(void)
  {
    generate_aggs();

    return has_work();
  }

  void
  serial_local::generate_aggs(void)
  {
    iterate_static_nodes();
  }

  void
  serial_local::init(const size_t)
  {
    database::map_nodes::const_iterator it(vm::All->DATABASE->nodes_begin());
    database::map_nodes::const_iterator end(vm::All->DATABASE->nodes_end());

    for(; it != end; ++it)
      {
	serial_node *cur_node(dynamic_cast<serial_node*>(it->second));
	/* MPI init_node only if the node belongs to the current process
	 * */
	if (api::onLocalVM(cur_node->get_id())) {
          init_node(cur_node);

          assert(cur_node->in_queue());
          assert(cur_node->has_work());
	}
      }
  }

void
serial_local::gather_next_tuples(db::node *node, simple_tuple_list& ls)
{
	serial_node *no((serial_node*)node);

    no->queue.top_list(ls);
  }

}


// Local Variables:
// mode: C++
// indent-tabs-mode: nil
// End:
