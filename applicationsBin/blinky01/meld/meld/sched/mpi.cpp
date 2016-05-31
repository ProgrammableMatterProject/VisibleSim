/*
 * MPI Scheduler for Meld VM
 *
 * @author Xing Zhou - xingz
 *
 * The scheduler handles communication between the multiple vms and
 * the outside world.  With the current implementation, there is only one
 * world in the mpi.  Process with rank 0 handles the connections to the
 * outside world, while rank from 1 until N is individual vm.
 *
 * The scheduler uses the Boost::MPI library for the MPI functions.
 */

#include <boost/mpi.hpp>
#include <iostream>
#include <string>
#include <boost/serialization/string.hpp>

/* Necessary Functions
 *
 * poll
 * init
 * sending
 */

namespace sched {
    class mpi {
        private:
            boost::mpi::environment env;
            boost::mpi::communicator world;

        public:
            void init(const size_t) {
            
            }

            void new_agg(work& w) {}

            void new_work(const node *, work& new_work) {}

            void assert_end(void) const {}

            void assert_end_iteration(void) const {}

            node *get_work(void) {}

            bool terminate_iteration(void) {}

            void generate_aggs(void) {}

            simple_tuple_vector gather_active_tuples(db::node *node, const vm::predicate_id pred) {}

            void gather_next_tuples(db::node *node, simple_tuple_list& ls) {}
    };
}

int main(int argc, char *argv[]) {
    mpi::environment env(argc, argv);
    mpi::communicator world;

    if (world.rank() == 0) {
        mpi::request reqs[2];
        std::string msg, out_msg = "Hello";
        reqs[0] = world.isend(1, 0, out_msg);
        reqs[1] = world.irecv(1, 1, msg);
        mpi::wait_all(reqs, reqs + 2);
        std::cout << msg << "!" << std::endl;
    } else {
        mpi::request reqs[2];
        std::string msg, out_msg = "world";
        reqs[0] = world.isend(0, 1, out_msg);
        reqs[1] = world.irecv(0, 0, msg);
        mpi::wait_all(reqs, reqs + 2);
        std::cout << msg << ", ";
    }

    return 0;
}
