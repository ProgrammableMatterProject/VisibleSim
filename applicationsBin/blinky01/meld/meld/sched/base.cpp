
#include <algorithm>

#include "sched/base.hpp"
#include "process/work.hpp"
#include "db/tuple.hpp"
#include "vm/exec.hpp"
#include "process/machine.hpp"
#include "api/api.hpp"
#include "debug/debug_handler.hpp"
#ifdef SIMD
#include "vm/determinism.hpp"
#endif


using namespace std;
using namespace boost;
using namespace vm;
using namespace process;
using namespace db;

namespace sched
{

  static bool init(void);

  pthread_key_t sched_key;
  static bool started(init());

  static void
  cleanup_sched_key(void)
  {
   pthread_key_delete(sched_key);
 }

 static bool init(void)
 {
   int ret(pthread_key_create(&sched_key, NULL));
   (void)ret;
   (void)started;
   assert(ret == 0);
   atexit(cleanup_sched_key);
   return true;
}

void
base::do_work(db::node *node)
{
   state.run_node(node);
}


// Loop of execution
void
base::do_loop(void)
{
  db::node *node(NULL);
  bool hasComputed = true;
  bool hasWork = true;
  
  while(true) {
      if (debugger::serializationMode){
          debugger::serializedPause();
      }
      while ((node = get_work())) {
          // Current VM has local work, process work
          do_work(node);
          finish_work(node);
          hasComputed = true;
      }
      if (debugger::isInMpiDebuggingMode()||debugger::isInSimDebuggingMode()){
          debugger::receiveMsg();
          if (debugger::isTheSystemPaused()){
              debugger::isPausedInWorkLoop = true;
              debugger::display("PAUSED\n",debugger::PAUSE);
              debugger::pauseIt();
          }
      }

#ifdef SIMD
	if (api::isInBBSimMode()) {
		if (!determinism::isInDeterministicMode()) {
			hasWork = api::pollAndProcess(this);
		}
		if (hasWork && !this->has_work() && debugger::isDebuggerQueueEmpty()) {
			if (hasComputed && determinism::isInDeterministicMode()) {
				determinism::workEnd();
				hasComputed = false;
			}
			hasWork = api::waitAndProcess();
		}
	} else {
		hasWork = api::pollAndProcess(this);
	}
# else
	hasWork = api::pollAndProcess(this);
#endif
	bool ensembleFinished = false;
	if (!hasWork)
		ensembleFinished = api::ensembleFinished();
	//api::serializeEndExec();
	if (ensembleFinished)
		break;
  }
}

void
base::loop(void)
{
   // start process pool
   // multi-thread memory manager / allocator
 mem::ensure_pool();

   // Init is based on scheduler type
 init(vm::All->NUM_THREADS);

 do_loop();

 assert_end();
 end();
   // cout << "DONE " << id << endl;
}

base*
base::get_scheduler(void)
{
 sched::base *s((sched::base*)pthread_getspecific(sched_key));
 return s;
}

void
base::start(void)
{
 pthread_setspecific(sched_key, this);
 if(id == 0) {
       // Main thread
  thread = new boost::thread();
  loop();
 }
}

base::~base(void)
{
	delete thread;
}

base::base(const vm::process_id _id):
id(_id),
thread(NULL),
state(this),
iteration(0)
#ifdef INSTRUMENTATION
, processed_facts(0), sent_facts(0), ins_state(statistics::NOW_ACTIVE)
#endif
{

}

}
