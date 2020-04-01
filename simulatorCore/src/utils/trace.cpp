/*
 * trace.cpp
 *
 *  Created on: June 22, 2013
 *      Author: Andre
 */

#include "utils/trace.h"
#include "events/scheduler.h"

std::ofstream log_file{};

void ConsoleStream::flush() {
    scheduler->trace(stream.str(),blockId);
    stream.str("");
};
