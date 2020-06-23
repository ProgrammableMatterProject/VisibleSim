/*! @file statsCollector.cpp
 * @brief Provides a mean to store useful number statistics about VisibleSim at runtime
 * Implemented following the singleton design pattern.
 * @author Pierre Thalamy
 * @date 22/07/2016
 */

#include <iomanip>
#include "statsCollector.h"
#include "../base/world.h"

using namespace std;

namespace BaseSimulator {
namespace utils {

ostream& operator<<(ostream& out,const StatsCollector &sc) {
    out << TermColor::BBlue;
    out << endl << "=== GLOBAL STATISTICS ===" << endl;
    out << TermColor::BWhite << "Number of robots: "
        << TermColor::BMagenta << getWorld()->getSize() << endl;
    out << TermColor::BWhite << "Simulator elapsed time: "
        << TermColor::BMagenta << sc.simulatedElapsedTime << " us" << endl;
    out << TermColor::BWhite << "Real elapsed time: " << std::setprecision(2) << std::fixed
        << TermColor::BMagenta << sc.realElapsedTime << " us" << endl;
    out << TermColor::BWhite << "Number of events processed: "
        << TermColor::BMagenta << sc.eventsProcessed << endl;
    out << TermColor::BWhite << "Number of messages processed: "
        << TermColor::BMagenta << sc.messagesProcessed << endl;
    out << TermColor::BWhite << "Number of motions processed: "
        << TermColor::BMagenta << sc.motionsProcessed << endl;
    out << TermColor::BWhite << "Maximum sized reached by the events list: "
        << TermColor::BMagenta << sc.largestEventsQueueSize << endl;
    out << TermColor::BWhite << "Size of the events list at the end: "
        << TermColor::BMagenta << sc.endEventsQueueSize << endl;
    out << TermColor::BWhite << "Events(s) left in memory before destroying Scheduler: "
        << TermColor::BMagenta << sc.nbLivingEvents << endl;
    out << TermColor::BWhite << "Message(s) left in memory before destroying Scheduler: "
        << TermColor::BMagenta << sc.nbLivingMessages << endl;
    out << TermColor::BWhite << "Number of events processed per second: "
        << TermColor::BMagenta << sc.computeEventPerSec() << endl;
    out << TermColor::Reset;
    return out;
}

} // namespace BaseSimulator::utils
} // namespace BaseSimulator
