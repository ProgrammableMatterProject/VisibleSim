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

#include "../utils/tDefs.h"

namespace BaseSimulator {
namespace utils {

//!< StatsIndividual class.
class StatsIndividual {
private:
    // Messages
    uint64_t sentMessages = 0; //!< Total number of sent messages
    uint64_t receivedMessages = 0; //!< Total number of received messages

    uint64_t outgoingMessageQueueSize = 0; //!< Current number of messages in all the outgoing message queues
    uint64_t incommingMessageQueueSize = 0; //!< Current number of messages in all the incomming message queues
    uint64_t messageQueueSize = 0; //!< Current number of messages in the message queues (incomming + outgoing)

    uint64_t maxOutgoingMessageQueueSize = 0;  //!< Maximum reached outgoing message queue size
    uint64_t maxIncommingMessageQueueSize = 0;  //!< Maximum reached incomming message queue size
    uint64_t maxMessageQueueSize = 0; //!< Maximum reached message queue size

    // Motions
    uint64_t motions = 0; //!< Total number of perfomed motions
public:
    static bool enable; //!< Activation flag: true if per module statistics are enable, false otherwise

    StatsIndividual() {}; //!< Default constructor
    StatsIndividual(StatsIndividual const&); //<! Copy constructor
    ~StatsIndividual() {}; //!< Destructor

    //!< Increments sent message count by 1
    static void incSentMessageCount(StatsIndividual *s);
    //!< Increments received message count by 1
    static void incReceivedMessageCount(StatsIndividual *s);
    //!< Increments outgoing message queue size by 1
    static void incOutgoingMessageQueueSize(StatsIndividual *s);
    //!< Decrements outgoing message queue size by 1
    static void decOutgoingMessageQueueSize(StatsIndividual *s);
    //!< Increments incomming message queue size by 1
    static void incIncommingMessageQueueSize(StatsIndividual *s);
    //!< Decrements incomming message queue size by 1
    static void decIncommingMessageQueueSize(StatsIndividual *s);
    //!< Increments processed motion count by 1
    static void incMotionCount(StatsIndividual *s);

    //!< Returns a string that contains a summary of the module statistics
    static std::string getStats();
 private:

    //!< Updates queue size statistics
    void updateQueueSizeStats();

    //!< Updates statistics in cs using the value v
    static void compute1(uint64_t cs[3], uint64_t v);
    //!< Returns the mean using the sum contained in s and the cardinality n
    static long double compute2(uint64_t s[3], int n);
    //!< Returns result of a step of the variance computation using the mean m and the value v
    static long double compute3(long double m, uint64_t v);
    //!< Returns a string that summarizes the module statistics for a specific parameter
    static std::string formatStat(std::string n, uint64_t s[3], long double m, long double sd, std::string f);

}; // class StatsIndividual

} // namespace BaseSimulator::utils
} // namespace BaseSimulator

#endif // STATSINDIVIDUAL_H__
