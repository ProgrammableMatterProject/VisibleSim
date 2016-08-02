/* ConcurrentQueue : Multiple producers, one consumer */

#include <queue>
#include <mutex>
#include <condition_variable>
#include <chrono>

template<typename Elem>

class ConcurrentQueue {

private:
    std::queue<Elem> queue;
    mutable std::mutex m;
    std::condition_variable cond;

public:
    
    void push(Elem const& e) {
        std::unique_lock<std::mutex> lock(m);
        queue.push(e);
        lock.unlock();
        cond.notify_all();
    }

    bool empty() const {
        std::unique_lock<std::mutex> lock(m);
        return queue.empty();
    }

    void timed_wait() {
        std::chrono::time_point<std::chrono::system_clock> const timeout = std::chrono::system_clock::now()
            + std::chrono::seconds(1);
        std::unique_lock<std::mutex> lock(m);
        if(queue.empty()) {
            cond.wait_until(lock,timeout);
        }
    }
    
    Elem& front() {
        std::unique_lock<std::mutex> lock(m);
        return queue.front();
    }

    void pop() {
        std::unique_lock<std::mutex> lock(m);
        queue.pop();
    }

};
