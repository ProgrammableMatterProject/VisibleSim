/*! @file statsCollector.cpp
 * @brief Provides a mean to store useful number statistics about VisibleSim at runtime
 * Implemented following the singleton design pattern.
 * @author Pierre Thalamy
 * @date 22/07/2016
 */

#include "statsCollector.h"
#include "world.h"

using namespace std;

namespace BaseSimulator {
namespace utils {

ostream& operator<<(ostream& out,const StatsCollector &sc) {
    out << endl << "=== GLOBAL STATISTICS ===" << endl;
    out << "Number of robots: " << getWorld()->getSize() << endl;
    out << "Simulator elapsed time: " << sc.simulatedElapsedTime << endl;
    out << "Real elapsed time: " << sc.realElapsedTime << endl;
    out << "Number of events processed: " << sc.eventsProcessed << endl;
    out << "Number of messages processed: " << sc.messagesProcessed << endl;
    out << "Number of motions processed: " << sc.motionsProcessed << endl;
    out << "Maximum sized reached by the events list: " << sc.largestEventsQueueSize << endl;
    out << "Size of the events list at the end: " << sc.endEventsQueueSize << endl;
    out << "Events(s) left in memory before destroying Scheduler: " << sc.nbLivingEvents << endl;
    out << "Message(s) left in memory before destroying Scheduler: " << sc.nbLivingMessages << endl;
    out << "Number of events processed per second: " << sc.computeEventPerSec() << endl;
    return out;
}

} // namespace BaseSimulator::utils
} // namespace BaseSimulator
