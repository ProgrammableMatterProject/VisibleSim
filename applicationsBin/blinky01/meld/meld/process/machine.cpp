#include <iostream>
#include <signal.h>
#include "process/machine.hpp"
#include "vm/program.hpp"
#include "vm/state.hpp"
#include "vm/exec.hpp"
#include "runtime/list.hpp"
#include "mem/thread.hpp"
#include "mem/stat.hpp"
#include "stat/stat.hpp"
#include "utils/fs.hpp"
#include "interface.hpp"
#include "sched/serial.hpp"

#include "api/api.hpp"
#include "debug/debug_handler.hpp"
#include "debug/debug_prompt.hpp"


using namespace process;
using namespace db;
using namespace std;
using namespace vm;
using namespace boost;
using namespace sched;
using namespace mem;
using namespace utils;
using namespace statistics;

namespace process
{

  void
  machine::run_action(sched::base *sched, node* node, vm::tuple *tpl, const bool from_other)
  {
    const predicate_id pid(tpl->get_predicate_id());
    int r(0), g(0), b(0);
    assert(tpl->is_action());

    switch(pid) {
    case SETCOLOR_PREDICATE_ID:
      api::set_color(node, tpl->get_int(0), tpl->get_int(1), tpl->get_int(2));
      break;
    case SETCOLOR2_PREDICATE_ID:
      {
        string colorName = "";
        switch(tpl->get_int(0)) {
        case 0: r = 255; colorName = "RED"; break; // RED
        case 1: r = 255; g = 160; colorName = "ORANGE"; break; // ORANGE
        case 2: r = 255; g = 247; colorName = "YELLOW"; break; // YELLOW
        case 3: g = 255; colorName = "GREEN"; break; // GREEN
        case 4: g = 191; b = 255; colorName = "AQUA"; break; // AQUA
        case 5: b = 255; colorName = "BLUE"; break;// BLUE
        case 6: r = 255; g = 255; b = 255;
          colorName = "WHITE"; break; // WHITE
        case 7: r = 139; b = 204; colorName = "PURPLE"; break; // PURPLE
        case 8: r = 255; g = 192; b = 203;
          colorName = "PINK"; break; // PINK
        case -1: printf("Invalid Color(-1).\n");return; break;
        default: printf("Default Color.\n");break;
        }
        string msg = "\t-The color " + colorName +  " has been set\n";
        api::set_color(node, r, g, b);
        debugger::runBreakPoint("action",msg.c_str(),
                                (char*)colorName.c_str(),api::getNodeID());
        break;
      }
    case SETEDGELABEL_PREDICATE_ID:
      break;
    case SET_PRIORITY_PREDICATE_ID:
      assert(sched);
      if(from_other)
        sched->set_node_priority_other(node, tpl->get_float(0));
      else {
        sched->set_node_priority(node, tpl->get_float(0));
      }
      break;
    case ADD_PRIORITY_PREDICATE_ID:
      if(from_other)
        sched->add_node_priority_other(node, tpl->get_float(0));
      else
        sched->add_node_priority(node, tpl->get_float(0));
      break;
    case WRITE_STRING_PREDICATE_ID: {
      runtime::rstring::ptr s(tpl->get_string(0));

      cout << s->get_content() << endl;
    }
      break;
    case SCHEDULE_NEXT_PREDICATE_ID:
      if(!from_other) {
        sched->schedule_next(node);
      } else {
        assert(false);
      }
      break;
    default:
      assert(false);
      break;
    }

    delete tpl;
  }

  void
  machine::route_self(sched::base *sched, node *node, simple_tuple *stpl, const uint_val delay)
  {
    if(delay > 0) {
      work new_work(node, stpl);
      sched->new_work_delay(sched, node, new_work, delay);
    } else {
      assert(!stpl->get_tuple()->is_action());
      sched->new_work_self(node, stpl);
    }
  }

  void
  machine::route(const node* from, sched::base *sched_caller, const node::node_id id, simple_tuple* stpl, const uint_val delay)
  {
    assert(sched_caller != NULL);



    if (api::onLocalVM(id)){
      /* Belongs to the same process, does not require MPI */
      node *node(vm::All->DATABASE->find_node(id));

      sched::base *sched_other(sched_caller->find_scheduler(node));
      const predicate *pred(stpl->get_predicate());

      if(delay > 0) {
        work new_work(node, stpl);
        sched_caller->new_work_delay(sched_caller, from, new_work, delay);
      } else if(pred->is_action_pred()) {
        run_action(sched_other, node, stpl->get_tuple(), sched_caller != sched_other);
        delete stpl;
      } else if(sched_other == sched_caller) {
        work new_work(node, stpl);

        sched_caller->new_work(from, new_work);
      } else {
        work new_work(node, stpl);

        sched_caller->new_work_other(sched_other, new_work);
      }
    } else {

      /* Send to the correct process */
      api::sendMessage(from,id,stpl);
    }
  }




  void
  machine::deactivate_signals(void)
  {
    sigset_t set;

    sigemptyset(&set);
    sigaddset(&set, SIGALRM);
    sigaddset(&set, SIGUSR1);

    sigprocmask(SIG_BLOCK, &set, NULL);
  }

  void
  machine::set_timer(void)
  {
    // pre-compute the number of usecs from msecs
    static long usec = SLICE_PERIOD * 1000;
    struct itimerval t;

    t.it_interval.tv_sec = 0;
    t.it_interval.tv_usec = 0;
    t.it_value.tv_sec = 0;
    t.it_value.tv_usec = usec;

    setitimer(ITIMER_REAL, &t, 0);
  }

  void
  machine::slice_function(void)
  {
    bool tofinish(false);

    // add SIGALRM and SIGUSR1 to sigset
    // to be used by sigwait
    sigset_t set;
    sigemptyset(&set);
    sigaddset(&set, SIGALRM);
    sigaddset(&set, SIGUSR1);

    int sig;

    set_timer();

    while (true) {
      const int ret(sigwait(&set, &sig));
      (void)ret;
		assert(ret == 0);
      switch(sig) {
      case SIGALRM:
        if(tofinish)
          return;
        slices.beat(vm::All);
        set_timer();
        break;
      case SIGUSR1:
        tofinish = true;
        break;
      default: assert(false);
      }
    }

  }

  void
  machine::execute_const_code(void)
  {
    vm::state st;

    // no node or tuple whatsoever
    st.setup(NULL, NULL, 0, 0);
    execute_bytecode(vm::All->PROGRAM->get_const_bytecode(), st);
  }

  // Start all schedulers in the VM
  void
  machine::start(void)
  {
    // execute constants code

    execute_const_code();

    deactivate_signals();


    // Statistics sampling
    if(stat_enabled()) {
      // initiate alarm thread
      alarm_thread = new boost::thread(bind(&machine::slice_function, this));
    }

    vm::All->ALL_THREADS[0]->start();

    if(alarm_thread) {
      kill(getpid(), SIGUSR1);
      alarm_thread->join();
      delete alarm_thread;
      alarm_thread = NULL;
      slices.write(get_stat_file(), sched_type, vm::All);
    }

    const bool will_print(show_database || dump_database);

    if(will_print) {
      if(show_database)
        vm::All->DATABASE->print_db(cout);
      if(dump_database)
        vm::All->DATABASE->dump_db(cout);
    }

    if(memory_statistics) {
#ifdef MEMORY_STATISTICS
      cout << "Total memory in use: " << get_memory_in_use() / 1024 << "KB" << endl;
      cout << "Malloc()'s called: " << get_num_mallocs() << endl;
#else
      cout << "Memory statistics support was not compiled in" << endl;
#endif
    }
  }

  /* Implementation specific function */
  static inline database::create_node_fn
  get_creation_function(const scheduler_type sched_type)
  {

    switch(sched_type) {
    case SCHED_SERIAL:
      return database::create_node_fn(sched::serial_local::create_node);
    case SCHED_UNKNOWN:
      return NULL;
    default:
      return NULL;
    }

    throw machine_error("unknown scheduler type");

  }


  machine::machine(const string& file, const size_t th,
                   const scheduler_type _sched_type, const machine_arguments& margs, const string& data_file):
    filename(file),
    sched_type(_sched_type),
    alarm_thread(NULL),
    slices(th) /* th = number of threads, slices is for statistics */
  {
    bool added_data_file(false);

    vm::All = new vm::all();
    vm::All->PROGRAM = new vm::program(file);
    vm::theProgram = vm::All->PROGRAM;
    if(vm::All->PROGRAM->is_data())
      throw machine_error(string("cannot run data files"));
    if(data_file != string("")) {
      if(file_exists(data_file)) {
        vm::program data(data_file);
        if(!vm::All->PROGRAM->add_data_file(data)) {
          throw machine_error(string("could not import data file"));
        }
        added_data_file = true;
      } else {
        throw machine_error(string("data file ") + data_file + string(" not found"));
      }
    }

    if(margs.size() < vm::All->PROGRAM->num_args_needed())
      throw machine_error(string("this program requires ") + utils::to_string(vm::All->PROGRAM->num_args_needed()) + " arguments");

    vm::All->MACHINE = this;
    vm::All->ARGUMENTS = margs;
    vm::All->DATABASE = new database(added_data_file ? data_file : filename, get_creation_function(_sched_type));
    vm::All->NUM_THREADS = th;

    // Instantiate the scheduler object
    switch(sched_type) {
    case SCHED_SERIAL:
      vm::All->ALL_THREADS.push_back(dynamic_cast<sched::base*>(new sched::serial_local()));
      break;
    case SCHED_UNKNOWN: assert(false); break;
    default: break;
    }

    assert(vm::All->ALL_THREADS.size() == vm::All->NUM_THREADS);

  }

  machine::~machine(void)
  {
    // when deleting database, we need to access the program,
    // so we must delete this in correct order

    delete vm::All->DATABASE;

    for(process_id i(0); i != vm::All->NUM_THREADS; ++i)
      delete vm::All->ALL_THREADS[i];

    delete vm::All->PROGRAM;

    if(alarm_thread)
      delete alarm_thread;

    mem::cleanup(vm::All->NUM_THREADS);

  }

}


// Local Variables:
// mode: C++
// indent-tabs-mode: nil
// End:
