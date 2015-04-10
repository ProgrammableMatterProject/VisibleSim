// queues.c
//
// implementation of the queues

#ifndef _QUEUES_C_
#define _QUEUES_C_

#include "queues.bbh"

void retrySend(void)
{
    SendChunkQueue* currSq = ((SQTimeout *)thisTimeout)->sq;
    
    //Try to resend
    currSq->flags |= CLEAR_TO_SEND;
}

#endif
