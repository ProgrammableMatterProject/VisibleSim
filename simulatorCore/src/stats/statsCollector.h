/*! @file statsCollector.h
 * @brief Provides a mean to store useful number statistics about VisibleSim at runtime
 * Implemented following the singleton design pattern.
 * @author Pierre Thalamy
 * @date 22/07/2016
 */

#ifndef STATSCOLLECTOR_H__
#define STATSCOLLECTOR_H__

#include <iostream>
#include <cstdint>

#include "../utils/tDefs.h"

namespace BaseSimulator {
namespace utils {

//!< Singleton-based global statistics collection class
//!< @attention Any access to StatsCollector must be done through the getInstance() function
class StatsCollector {
/************************************************************
 *                   Module Description
 ************************************************************/
public:
    //<! @brief Used to get the singleton instance of StatsCollector.
    //!< Allocate it on first call, return existing instance on all subsequent calls
    //<! @return singleton instance of StatsCollector
    static StatsCollector& getInstance() {
        static StatsCollector instance;
        return instance;
    };
private:
    StatsCollector() {};        //!< Constructor. Nothing to be done.
    StatsCollector(StatsCollector const&); //<! Disable copy constructor. (Copying instance is not allowed)
    void operator=(StatsCollector const&); //<! Disable assignment operator. (Copying instance is not allowed)

/************************************************************
 *             Collected Statistics Description
 ************************************************************/
private:
    // Messages
    uint64_t messagesProcessed = 0; //!< Total number of messages processed by VisibleSim
    uint64_t nbLivingMessages = 0; //!< Total number of messages still in memory at scheduler end
    // uint64_t maxiMessageQueueDepth = 0; //!< Total number of messages processed by VisibleSim
    // Motions
    uint64_t motionsProcessed = 0; //!< Total number of motion events processed by VisibleSim
    // Events
    uint64_t eventsProcessed = 0; //!< Total number of events processed by VisibleSim
    uint64_t nbLivingEvents = 0; //!< Total number of events still in memory at scheduler end
    uint64_t largestEventsQueueSize = 0; //!< Largest size of the scheduler's event
    uint64_t endEventsQueueSize = 0; //!< Size of the events queue at scheduler end
    // Time
    Time simulatedElapsedTime = 0; //!< Duration of simulation in discrete simulator time
    double realElapsedTime = 0; //!< Duration of simulation in real time (us)

    //!< @brief Returns the number of events processed per seconds, calculated from realElapsedTime (in microseconds)
    inline double computeEventPerSec() const {  return realElapsedTime ? eventsProcessed / (realElapsedTime / 1000000) : 0; };
public:
    //!< Increments processed message count by 1
    inline void incMsgCount() { messagesProcessed++; };
    //!< Increments processed motion count by 1
    inline void incMotionCount() { motionsProcessed++; };
    //!< Increments processed event count by 1
    inline void incEventsCount() { eventsProcessed++; };
    //!< Updates both elapsed times
    inline void updateElapsedTime(Time simTime, Time realTime)
        { simulatedElapsedTime = simTime; realElapsedTime = realTime; };
    //!< Called before scheduler destruction to collect the state of important queues at end time
    inline void setLivingCounters(uint64_t livingEvents, uint64_t livingMessages)
        { nbLivingEvents = livingEvents; nbLivingMessages = livingMessages; };
    //!< Updates the max events queue size counter if new size is greater than previous size
    inline void updateLargestEventsQueueSize(uint64_t newSize)
        {  if (largestEventsQueueSize < newSize) largestEventsQueueSize = newSize;
        };
    inline void setEndEventsQueueSize(uint64_t endSize)
        {  endEventsQueueSize = endSize; };

    //!< Prints collected statistics to an ouput stream
    friend std::ostream& operator<<(std::ostream& out,const StatsCollector &sc);
};                              // class StatsCollector

} // namespace BaseSimulator::utils
} // namespace BaseSimulator

#endif // STATSCOLLECTOR_H__
