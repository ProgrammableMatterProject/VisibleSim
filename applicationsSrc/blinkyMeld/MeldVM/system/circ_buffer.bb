#include "circ_buffer.bbh"

// HARDWARE INCLUDE
// BOBBY:
// atomic block is a very simple mutex-like thing that turns off interrupts so things don't get corrupted
// this code is all system-level but you may need to define around ATOMIC_BLOCK for the simulator
#ifndef BBSIM
#include "util/atomic.h"
#endif

void push(byte data, CircBuf * b)
{ 
  #ifndef BBSIM
  ATOMIC_BLOCK(ATOMIC_RESTORESTATE)
  {
  #endif

    b->buf[b->end++] = data;

    if(b->end == CIRC_BUF_LEN)
    {
      b->end = 0;
    }

    if(b->end == b->start)
    {
      b->start++;
	
      if(b->start == CIRC_BUF_LEN)
	  {
	    b->start = 0;
	  }
    }
  #ifndef BBSIM
  }
  #endif
}

int pop(CircBuf * b)
{
  uint8_t data;

  if( isEmpty(b) )
  {
	return -1;
  }
  #ifndef BBSIM
  ATOMIC_BLOCK(ATOMIC_RESTORESTATE)
  {
  #endif
    data = b->buf[b->start++];
  
    if(b->start == CIRC_BUF_LEN)
    {
      b->start = 0;
    }
  #ifndef BBSIM
  }
  #endif
  
  return data;
}

byte isEmpty(CircBuf * b)
{
  if(b->start == b->end)
  {
    return 1;
  }
  return 0;
}

