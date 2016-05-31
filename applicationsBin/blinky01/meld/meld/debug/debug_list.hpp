
#ifndef DEBUG_LIST_HPP
#define DEBUG_LIST_HPP

namespace debugger {

    struct debugnode{
        struct debugnode* next;
        struct debugnode* prev;
        char* type;//must be dynamically allocated upon insertion
        char* name;//same
        int nodeID;//not dynamically allocated
    };


    struct list_header{
        struct debugnode* front;
        struct debugnode* back;
    };

    typedef struct list_header* debugList;

    bool isInBreakPointList(debugList L, char* type, char* name, int nodeID);
    void insertBreak(debugList L, char* type, char* name, int nodeID);
    void listFree(debugList L);
    debugList newBreakpointList();
    void printList(std::ostream& out, debugList L);
    int removeBreakPoint(debugList L, char* type, char* name, int nodeID);


}

#endif
