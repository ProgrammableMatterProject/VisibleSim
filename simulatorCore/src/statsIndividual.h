/*! @file statsIndividual.h
 * @brief Provides a mean to store useful number statistics about modules at runtime
 * @author Andre Naz
 * @date 10/08/2016
 */

#ifndef STATSINDIVIDUAL_H__
#define STATSINDIVIDUAL_H__

#include <iostream>
#include <cstdint>
#include <string>

//#include "buildingBlock.h"
#include "tDefs.h"

namespace BaseSimulator {
  //class BuildingBlock;
namespace utils {

//!< StatsIndividual class.
//!<  
class StatsIndividual {
private:
    // Messages
    uint64_t sentMessages = 0; //!< Total number of sent messages
    uint64_t receivedMessages = 0; //!< Total number of received messages

    uint64_t outgoingMessageQueueSize = 0;
    uint64_t incommingMessageQueueSize = 0;
    uint64_t messageQueueSize = 0;
    
    uint64_t maxMessageQueueSize = 0; 
    uint64_t maxOutgoingMessageQueueSize = 0;
    uint64_t maxIncommingMessageQueueSize = 0;

    // Motions
    uint64_t motions = 0; //!< Total number of motions
public:
    static bool enable;
    
    StatsIndividual() {};        //!< Default constructor
    StatsIndividual(StatsIndividual const&); //<! Copy constructor
    ~StatsIndividual() {};

    void updateQueueSizeStats();
    
    //!< Increments sent message count by 1
    static void incSentMessageCount(StatsIndividual *s);
    
    //!< Increments received message count by 1
    static void incReceivedMessageCount(StatsIndividual *s);
  
    static void incOutgoingMessageQueueSize(StatsIndividual *s);
    static void decOutgoingMessageQueueSize(StatsIndividual *s);

    static void incIncommingMessageQueueSize(StatsIndividual *s);
    static void decIncommingMessageQueueSize(StatsIndividual *s);
    
    //!< Increments processed motion count by 1
    static void incMotionCount(StatsIndividual *s);
    
    static std::string getStats();
 private:
    static void compute1(uint64_t cs[3], uint64_t v);
    static long double compute2(uint64_t s[3], int n);
    static long double compute3(long double m, uint64_t v);
    static std::string formatStat(std::string n, uint64_t s[3], long double m, long double sd, std::string f);
    
};                              // class StatsIndividual

} // namespace BaseSimulator::utils
} // namespace BaseSimulator

#endif // STATSINDIVIDUAL_H__
