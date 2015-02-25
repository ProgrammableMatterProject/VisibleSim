#ifndef DEBUG_HANDLER_HPP
#define DEBUG_HANDLER_HPP

#include <string>
#include <list>
#include <stdint.h>
#include "concurrentQueue.hpp"


namespace debugger {

    const int DUMP = 1;
    const int CONTINUE = 7;
    const int BREAKPOINT = 2;
    const int NOTHING = 8;
    const int REMOVE = 9;
    const int PAUSE = 4;
    const int UNPAUSE = 3;
    const int BREAKFOUND = 6;
    const int PRINTCONTENT = 5;
    const int FACTDER = 1;
    const int FACTCON = 2;
    const int FACTRET = 3;
    const int ACTION  = 4;
    const int SENSE = 5;
    const int BLOCK = 6;
    const int DEBUG = 16;
    const int DEBUGMPI = 0;
    const int TERMINATE = 10;
    const int PRINTLIST = 11;
    const int RUN = 12;
    const int MODE = 13;
    const int TIME = 14;

    typedef uint64_t message_type;


    extern ConcurrentQueue<message_type*> *messageQueue;
    extern std::list<struct msgListContainer*>* rcvMessageList;
    extern int numberExpected;

    const int MASTER = 0;

    const int MAXLENGTH = 512/sizeof(message_type);

    void debugController(int instruction, std::string specification);
    bool isTheSystemPaused(void);
    void initSimDebug(void);
    void setSimDebuggingMode(bool setting);
    std::string typeInt2String(int type);
    std::string getNode(std::string specification);
    std::string getName(std::string specification);
    std::string getType(std::string specification);
    int characterInStringIndex(std::string str, char character);
    void receiveMsg(void);
    int sendMsg(int destination, int msgType,
                 std::string content, bool broadcast = false);
    void messageQueueInsert(message_type* msg);
    void handlePauseCommand(void);
    message_type* pack(int msgEncode, std::string content, int priority);
    void insertMsg(std::string content, int priority, int instruction, int node);
    std::string buildString(struct msgListContainer* container);
    struct msgListContainer* checkAndGet(void);
    void printRcv();
}

#endif
