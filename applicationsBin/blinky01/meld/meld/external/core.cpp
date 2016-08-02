
#include "external/core.hpp"
#include "db/database.hpp"
#include "sched/common.hpp"
#include "vm/state.hpp"
#include "vm/all.hpp"

using namespace sched;
using namespace db;
using namespace std;
using namespace vm;

namespace vm
{
  namespace external
  {

    argument
    node_priority(EXTERNAL_ARG(id))
    {
      ignoreUnusedParamWarning(__id);
      //DECLARE_NODE(id);		DOES NOT SEEM TO BE USED
      float_val ret(0.0);

      RETURN_FLOAT(ret);
    }

    argument
    cpu_id(EXTERNAL_ARG(id))
    {
      ignoreUnusedParamWarning(__id);
			
      int_val ret(0);
      //DECLARE_NODE(id);		DOES NOT SEEM TO BE USED

      RETURN_INT(ret);
    }

  }
}


// Local Variables:
// mode: C++
// indent-tabs-mode: nil
// End:
