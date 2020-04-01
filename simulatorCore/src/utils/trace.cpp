/*
 * trace.cpp
 *
 *  Created on: June 22, 2013
 *      Author: Andre
 */
 
#include "trace.h"
#include "scheduler.h"

std::ofstream log_file{};

void ConsoleStream::flush() {
    scheduler->trace(stream.str(),blockId);
    stream.str("");
};
