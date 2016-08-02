#ifndef DEBUG_PROMPT_HPP
#define DEBUG_PROMPT_HPP

#include "debug_Simhandler.hpp"
#include <iostream>

namespace debugger {

    void *run(void *debugAll);
    bool parseline(std::string line);
    int handle_command(std::string command);
    void help();

    typedef void (*ftrp)(message_type*);

	ftrp initDebugger(int (*sendMsg)(int,message_type*,int),
                        void (*pauseSim)(int),
                        void (*unPauseSim)(void),
                        void (*quitDebug)(void),
                      std::ostream& o = std::cout, std::istream& i = std::cin);

    extern int  (*debugSendMsg)(int,message_type*,int);
    extern void (*pauseSimulation)(int);
    extern void (*unPauseSimulation)(void);
    extern void (*quitDebugger)(void);

    void joinThread();
    void detachThread();

}

#endif
