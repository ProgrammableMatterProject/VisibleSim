/*
 * trace.h
 *
 *  Created on: June 22, 2013
 *      Author: Andre
 */

#ifndef TRACE_H_
#define TRACE_H_

#include <fstream>
#include <iostream>
#include <sstream>
#include <cstring>

#include "utils/tDefs.h"

#define LOGFILE

#ifdef LOGFILE
    extern std::ofstream log_file;
    #define OUTPUT log_file
    #define ERRPUT log_file
#else
    #define OUTPUT cout
    #define ERRPUT cerr
#endif

using namespace std;

namespace BaseSimulator {
class Scheduler;
}

class ConsoleStream {
    BaseSimulator::Scheduler *scheduler;
    bID blockId;
    stringstream stream;
public:

    ConsoleStream() { stream.str(""); };
    void setInfo(BaseSimulator::Scheduler*s,bID id) {
        scheduler=s;
        blockId=id;
    }

    void flush();

    ConsoleStream& operator<<(const char* value ) {
        int l=strlen(value);
        if (value[l-1]=='\n') {
            string s(value);
            s = s.substr(0,l-1);
            stream << s;
            flush();
        } else {
            stream << value;
        }
        return *this;
    }

    template<typename T>
    ConsoleStream& operator<<( T const& value ) {
        stream << value;
        return *this;
    }
};

#endif
