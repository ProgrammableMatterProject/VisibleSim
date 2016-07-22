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
    StatsCollector() {};
    StatsCollector(StatsCollector const&); //<! Disable copy constructor. (Copying instance is not allowed)
    void operator=(StatsCollector const&); //<! Disable assignment operator. (Copying instance is not allowed)

/************************************************************
 *             Collected Statistics Description 
 ************************************************************/
private:
    // Messages
    uint64_t messagesProcessed = 0; //!< Total number of messages processed by VisibleSim
    // uint64_t maxiMessageQueueDepth = 0; //!< Total number of messages processed by VisibleSim
    // Motions
    uint64_t motionsProcessed = 0; //!< Total number of motion events processed by VisibleSim
    // Events
    uint64_t eventsProcessed = 0; //!< Total number of events processed by VisibleSim
    // Time
    uint64_t simulatedElapsedTime = 0; //!< Duration of simulation in discrete simulator time
    double realElapsedTime = 0; //!< Duration of simulation in real time
public:
    //!< Increments processed message count by 1
    inline void incMsgCount() { messagesProcessed++; };
    //!< Increments processed motion count by 1
    inline void incMotionCount() { motionsProcessed++; };
    //!< Increments processed event count by 1
    inline void incEventsCount() { eventsProcessed++; };
    //!< Updates both elapsed times
    inline void updateElapsedTime(uint64_t simTime, uint64_t realTime)
        { simulatedElapsedTime = simTime; realElapsedTime = realTime; };

    //!< Prints collected statistics to an ouput stream
    friend std::ostream& operator<<(std::ostream& out,const StatsCollector &sc);
};                              // class StatsCollector

} // namespace BaseSimulator::utils
} // namespace BaseSimulator

#endif // STATSCOLLECTOR_H__
