#ifndef DEBUG_PROMPT_HPP
#define DEBUG_PROMPT_HPP

#include "vm/state.hpp"
#include "debug/debug_list.hpp"
#include "vm/all.hpp"

namespace debugger {

    void debug(void);
    void *run(void*);
    bool parseline(std::string line, debugList& factBreaks);
    int handle_command(std::string command, debugList& factList);
    void help();

}

#endif
