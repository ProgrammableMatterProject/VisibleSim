/* ConcurrentQueue : Multiple producers, one consumer */

#include <queue>
#include <boost/thread/mutex.hpp>
#include <boost/thread/condition.hpp>

template<typename Elem>

class ConcurrentQueue {

private:
   std::queue<Elem> queue;
   mutable boost::mutex m;
   boost::condition_variable cond;

public:
    
   void push(Elem const& e) {
      boost::mutex::scoped_lock lock(m);
      queue.push(e);
      lock.unlock();
      cond.notify_all();
   }

   bool empty() const {
      boost::mutex::scoped_lock lock(m);
      return queue.empty();
   }

   void timed_wait() {
      boost::system_time const timeout = boost::get_system_time() + boost::posix_time::seconds(1);
      boost::mutex::scoped_lock lock(m);
      if(queue.empty()) {
         cond.timed_wait(lock,timeout);
      }
   }
    
   Elem& front() {
      boost::mutex::scoped_lock lock(m);
      return queue.front();
   }

   void pop() {
      boost::mutex::scoped_lock lock(m);
      queue.pop();
   }

};
