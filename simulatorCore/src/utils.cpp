/**
 * @file   utils.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Aug 10 18:07:23 2018
 * 
 * @brief  Utils functions implementation, with utils.h as a standalone all functions need to be inline and any tweaking to the utils forces the recompilation of the whole project
 * 
 * 
 */


#include "utils.h"

#include <fstream>
#include <ctime>
#include <sstream>
#include <sys/types.h>
#include <unistd.h>
#include <string>
#include <cstdlib>

using namespace BaseSimulator;
using namespace utils;
using namespace std;

std::string Backtrace(int skip = 1);

int utils::m_mod(int l, int mod) {
    VS_ASSERT_MSG(mod != 0, "Modulus cannot be 0!");
    return l < 0 and l % mod != 0 ? mod - (-l % mod) : l % mod;
}

bool utils::assert_handler(bool cond, const char *file, const int line,
                           const char *func, const char* msg) {
    
    cerr << endl << "!!!!!!!!!!!!!!!! VISIBLESIM ASSERT TRIGGERED !!!!!!!!!!!!!" << endl;
    std::cerr << "In fonction " << func
              << " at " << file << ":" << line << std::endl;
    if (msg) std::cerr << "Reason: " << msg << endl;
    cerr << endl;
    assert_stack_print();
    cerr << endl;
    
    awaitKeyPressed();

    exit(1);
    
    return true;
}

bool utils::assert_stack_print() {
    cerr << "--------- StackTrace ---------" << endl;
    cerr << Backtrace(3);
    cerr << "--------- END ---------" << endl;
    
    return true;
}


void utils::awaitKeyPressed() {
    std::cout << "Press any key to continue..." << std::endl; std::cin.ignore();
}

const std::string
utils::generateTimestampedFilename(const std::string& prefix, const std::string& ext) {
    std::ostringstream out;

    time_t now = time(0);
    tm *ltm = localtime(&now);
    
    out << prefix << "_" << ltm->tm_hour << "_"
        << ltm->tm_min << "_" << ltm->tm_sec << "." << ext;
    
    return out.str();
}

const std::string
utils::generateTimestampedDirName(const std::string& dirBasename) {
    std::ostringstream out;

    time_t now = time(0);
    tm *ltm = localtime(&now);
    
    out << dirBasename << "_" << ltm->tm_hour << "_"
        << ltm->tm_min << "_" << ltm->tm_sec;

    return out.str();
}

bool utils::file_exists(const std::string fileName) {
    std::ifstream infile(fileName);
    return infile.good();
}

void utils::swap(int* a, int* b) {
	int temp = *a;
	*a = *b;
	*b = temp;
}

/* 
 * Copyright (c) 2009-2017, Farooq Mela
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.

 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

/**
 * @file   stacktrace.cxx
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jan 29 14:06:15 2019
 * 
 * @brief  https://gist.github.com/fmela/591333
 *  For printing stacktrace, to be used with our custom-defined asserts.
 */

#include <execinfo.h> // for backtrace
#include <dlfcn.h>    // for dladdr
#include <cxxabi.h>   // for __cxa_demangle

// This function produces a stack backtrace with demangled function & method names.
std::string Backtrace(int skip)
{
    void *callstack[128];
    const int nMaxFrames = sizeof(callstack) / sizeof(callstack[0]);
    char buf[1024];
    int nFrames = backtrace(callstack, nMaxFrames);
    char **symbols = backtrace_symbols(callstack, nFrames);

    std::ostringstream trace_buf;
    for (int i = skip; i < nFrames; i++) {
        // printf("%s\n", symbols[i]);

        Dl_info info;
        if (dladdr(callstack[i], &info) && info.dli_sname) {
            char *demangled = NULL;
            int status = -1;
            if (info.dli_sname[0] == '_')
                demangled = abi::__cxa_demangle(info.dli_sname, NULL, 0, &status);
            snprintf(buf, sizeof(buf), "%-3d %*p %s + %zd\n",
                     i - (skip-1), int(2 + sizeof(void*) * 2), callstack[i],
                     status == 0 ? demangled :
                     info.dli_sname == 0 ? symbols[i] : info.dli_sname,
                     (char *)callstack[i] - (char *)info.dli_saddr);
            free(demangled);
        } else {
            snprintf(buf, sizeof(buf), "%-3d %*p %s\n",
                     i, int(2 + sizeof(void*) * 2), callstack[i], symbols[i]);
        }
        trace_buf << buf;
    }
    free(symbols);
    if (nFrames == nMaxFrames)
        trace_buf << "[truncated]\n";
    return trace_buf.str();
}
