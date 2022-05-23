/*
 * trace.cpp
 *
 *  Created on: June 22, 2013
 *      Author: Andre
 */

#include "trace.h"
#include "../events/scheduler.h"
#include "../replay/replayExporter.h"

std::ofstream log_file{};

ConsoleStream& ConsoleStream::operator<<(const char* value ) {
    int l=strlen(value);
    if (value[l-1]=='\n') {
        string s(value);
        s = s.substr(0,l-1);
        stream << s;
        flush();
    } else {
        stream << value;
    }
    if (ReplayExporter::isReplayEnabled())
        ReplayExporter::getInstance()->writeConsoleTrace(getScheduler()->now(),
                                                         blockId, value);
    return *this;
}


void ConsoleStream::flush() {
    scheduler->trace(stream.str(),blockId);
    stream.str("");
}
