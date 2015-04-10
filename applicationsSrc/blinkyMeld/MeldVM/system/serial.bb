#include "serial.bbh"
#include "../hw-api/hwSerial.h"

threadvar Port port[NUM_PORTS];

void initPortQueues(Port* p)
{
    // reset send queue status
    p->sq.retry         = 0;
    p->sq.flags         = CLEAR_TO_SEND;
	
	// send queue timers
	p->sq.qtout.sq = &(p->sq);
	p->sq.qtout.tout.callback = (GenericHandler)&retrySend;


	
	// empty the send queue
	//freeChunk(p->sq.head);
    p->sq.head = NULL;
    //freeChunk(p->sq.tail);
    p->sq.tail = NULL;
	
	// reset receive queue status
    p->rq.index         = 0;
	p->rq.checksum      = 0;
    p->rq.flags         = NO_CHUNK_READY;

	// empty receive queues
    //freeReceiveChunk(p->rq.curr);
	p->rq.curr = NULL;
}

void initPorts()
{
	PRef i;
	for(i = 0; i < NUM_PORTS; i++)
	{
		port[i].pnum  = i;
		initPortQueues(&port[i]);
	}
	initHWPorts();
}
