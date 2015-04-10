#ifndef _HW_DATALINK_H_
#define _HW_DATALINK_H_

#ifndef IGNORE_IN_PASS1_OFF_COMPILE_BB
#include "../system/defs.h"
#include "../system/data_link.h"
#include "../system/memory.h"
#endif

// use pthread mutex
#ifdef BBSIM
    #define SQ_LOCK     &(this()->sendQueueMutex)
// use AVR mutex
#else
    #define SQ_LOCK     ATOMIC_RESTORESTATE
#endif

// removes the first packet from the send queue and
//    updates pointers
// inputs:  port
// output:  1-success, 0-failure
byte removeFromSq(PRef p, byte response);

// flush a send queue (used when retries fails)
void flushSendQueue(PRef p);

// pulls bytes from the buffer and puts them into Chunks
// also handles ACKs appropriately
void processBuffer(PRef);

// try sending stuff if available and ready
void sendOnSerial(PRef);

// gets data from the global receive queue
Chunk* nextPacket(void);

// initialization
void initHWDataLink(void);

#endif

