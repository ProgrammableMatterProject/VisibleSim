/*! @file statsCollector.cpp
 * @brief Provides a mean to store useful number statistics about VisibleSim at runtime
 * Implemented following the singleton design pattern.
 * @author Pierre Thalamy
 * @date 22/07/2016
 */

#include "statsCollector.h"

using namespace std;

namespace BaseSimulator {
namespace utils {

ostream& operator<<(ostream& out,const StatsCollector &sc) {
    out << "=== COLLECTED STATISTICS ===" << endl;
	out << "Simulator elapsed time : " << sc.simulatedElapsedTime << endl;
    out << "Real elapsed time : " << sc.realElapsedTime << endl;
	out << "Number of events processed : " << sc.eventsProcessed << endl;
    out << "Number of messages processed : " << sc.messagesProcessed << endl;
    out << "Number of motions processed : " << sc.motionsProcessed << endl;

    return out;
}

} // namespace BaseSimulator::utils
} // namespace BaseSimulator
