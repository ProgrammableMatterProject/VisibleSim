#include "ensemble.bbh"
#include "message.bbh"
#include "data_link.bbh"

#ifdef CLOCK_SYNC
#include "clock.bbh"
#endif
#ifdef LOG_DEBUG
#include "log.bbh"
#endif

/* #define ENSEMBLE_DEBUG */

//     allocated chunks rather than a user allocated chunk

threadvar Neighborhood thisNeighborhood;
threadvar Timer ttNeighbor[6];
/* used to ensure that two blocks cannot be both waiting infinitely for a 
handshake from the other block */
threadvar byte ignoredHandshakeCount[NUM_PORTS];

// converts a UID into a passed in character stream.  Assumes 2-byte GUIDs.
void GUIDIntoChar(Uid id, byte * c)
{
  c[0] = (id >> 8) & 0x00FF;
  c[1] = (id & 0x00FF);
}

// takes a passed in character array and returns a UID representing the array.  Assumes 2-byte GUIDs.
Uid charToGUID(byte * c)
{
  Uid tmp;
  tmp = (Uid)(c[0]) << 8;
  tmp |= c[1];
  return tmp;
}

// returns number of neighbors that are not VACANT.
byte getNeighborCount()
{
  uint8_t count, i;
	
  for(count = 0, i = 0; i < NUM_PORTS; ++i)
    {
      if(thisNeighborhood.n[i] != VACANT)
	{
	  count++;
	}	
    }

  //	if(count>0)
  //	  printf("NEIGHBORCOUNT = %d\r\n", count);
  return count;
}

// simple functions to access geographic neighbors
Uid down(void) { return thisNeighborhood.n[DOWN]; }
Uid up(void) { return thisNeighborhood.n[UP]; }
Uid north(void) { return thisNeighborhood.n[NORTH]; }
Uid south(void) { return thisNeighborhood.n[SOUTH]; }
Uid east(void) { return thisNeighborhood.n[EAST]; }
Uid west(void) { return thisNeighborhood.n[WEST]; }

// called to set the GUID of a neighbor.
// if the neighbor is the same as before, do nothing.
// if the neighbor has a different GUID (or VACANT), trigger a neighbor change event.
void	updateNeighbor(PRef p, Uid b)
{
#ifdef DEBUG
#if DEBUG == 5 // UP

#define DEBUGPORT 5
#define DEBUGUART USARTD1

#elif DEBUG == 1 // NORTH

#define DEBUGPORT 1
#define DEBUGUART USARTC1

#elif DEBUG == 4 //SOUTH

#define DEBUGPORT 4
#define DEBUGUART USARTE0

#elif DEBUG == 2 //EAST

#define DEBUGPORT 2
#define DEBUGUART USARTF0

#elif DEBUG == 3 //WEST

#define DEBUGPORT 3
#define DEBUGUART USARTC0

#elif DEBUG == 0 //DOWN

#define DEBUGPORT 0
#define DEBUGUART USARTD0

#endif

#ifndef DEBUGPORT
#error Invalid DEBUG option chosen - use a face enum.
#endif

  if(p == DEBUGPORT)
    {
      return;
    }

#endif

#ifdef LOG_DEBUG
  if(isHostPort(p)) {
    return;
  }
#endif

/* #ifdef ENSEMBLE_DEBUG */
/*   printf ("\x1b[32m--%d--\tIn updateNeighbor\x1b[0m\n", getGUID()); */
/* #endif */


  if(p < NUM_PORTS)
    {
      // is the state changing?  Trigger handler
      if(thisNeighborhood.n[p] != b)
	{
	  thisNeighborhood.n[p] = b;
	  triggerHandler(EVENT_NEIGHBOR_CHANGE);
#ifdef CLOCK_SYNC
	  handleNeighborChange(p);
#endif
	}
	  
    }
	
  /* printf("Block %d now has %d neighbors \n",getGUID(),getNeighborCount()); */
}

// called when start handshake message is complete by success or failure
void neighborScanCB(void)
{
#ifdef LOG_DEBUG
  if(isHostPort(faceNum(thisChunk))) {
    //disableTimer(ttNeighbor[faceNum(thisChunk)]);
    freeChunk(thisChunk);
    return;
  }
#endif

  // message was received.  wait for handshake to return, but allow for timeout to restart scan
  if(chunkResponseType(thisChunk) == MSG_RESP_ACK)
    {
      (ttNeighbor[faceNum(thisChunk)]).period = NEIGHBOR_HANDSHAKE_TIMEOUT_PERIOD;
/* #ifdef ENSEMBLE_DEBUG */
/*       printf ("\x1b[33m--%d--\tPotential neighbor\x1b[0m\n", getGUID()); */
/* #endif */
    }
  // else failure
  else
    {
/* #ifdef ENSEMBLE_DEBUG */
/*       printf ("\x1b[33m--%d--\tScan Lost!\x1b[0m\n", getGUID()); */
/* #endif */
      (ttNeighbor[faceNum(thisChunk)]).period = NEIGHBOR_SCAN_PERIOD;
    }

  // re-enable timer with potentially new period
  enableTimer(ttNeighbor[faceNum(thisChunk)]);
 
  freeChunk(thisChunk);
}

// send a start handshake message to a vacant port
void neighborScan(void)
{
  byte buf[4];
  PRef p = thisTimeout->arg;

  // could check for non-vacant neighbor here?
#ifdef DEBUGPORT
  if(p == DEBUGPORT) {
    return;
  }
#endif
#ifdef LOG_DEBUG
  if(isHostPort(p)) {
    return;
  }
#endif

  // prepare message
  buf[0] = NEIGHBOR_MSG;
  buf[1] = MSG_NEIGHBOR_START;
  GUIDIntoChar(getGUID(), &(buf[2]));
  
  // disable timer so no extra time-based calls are executed until sent message callback
  disableTimer(ttNeighbor[p]);
  if(ttNeighbor[p].t.state != INACTIVE)
    {
      deregisterTimeout(&(ttNeighbor[p].t));
    }

  // enqueue message
  sendSystemMessage(p, buf, 4, RES_SYS_HANDLER, (GenericHandler)&neighborScanCB);
}

void neighborKeepaliveCB(void)
{
  // message was received.  re-enable timer and continue as normal
  // note that since we always ACK a received message, this will happen even if the other block 
  // does not understand why the keepalive was sent.  however, the other block will restart a handshake.
/* #ifdef ENSEMBLE_DEBUG */
/*   printf ("\x1b[34m--%d--\tIn neighborKeepaliveCB\x1b[0m\n", getGUID()); */
/* #endif */
  
  if(chunkResponseType(thisChunk) == MSG_RESP_ACK)
    {
      enableTimer(ttNeighbor[faceNum(thisChunk)]);
    }
  else // neighbor lost!  begin rescanning
    {
#ifdef ENSEMBLE_DEBUG
	    printf ("\x1b[33m--%d--\tKeepalive lost!\x1b[0m\n", getGUID());
#endif
      restartScan(faceNum(thisChunk));
    }

  freeChunk(thisChunk);
}

// this is called periodically to verify neighbor is still there and in the right state
void neighborKeepalive(void)
{
  // craft and send a keeplive message of the form:
  // [header] [myGUID][yourGUID]
  byte buf[6];
  buf[0] = NEIGHBOR_MSG;
  buf[1] = MSG_NEIGHBOR_KEEPALIVE;
  GUIDIntoChar(getGUID(), &(buf[2]));
  GUIDIntoChar(thisNeighborhood.n[thisTimeout->arg], &(buf[4]));

/* #ifdef ENSEMBLE_DEBUG */
/*   printf ("\x1b[34m--%d--\tIn neighborKeepalive\x1b[0m\n", getGUID()); */
/* #endif */

  // prevent timer from calling this again, message callback will reenable as necessary
  disableTimer(ttNeighbor[thisTimeout->arg]);

  // send message - CB will either re-add timer, or restart scanning for neighbors
  sendSystemMessage(thisTimeout->arg, buf, 6, RES_SYS_HANDLER, (GenericHandler)&neighborKeepaliveCB);
}

// callback for neighbor_response packet
void neighborHandshakeCB(void)
{
/* #ifdef ENSEMBLE_DEBUG */
/*   printf ("\x1b[34m--%d--\tIn neighborHandshakeCB\x1b[0m\n", getGUID()); */
/* #endif */

  // message was received.  add neighbor to neighborhood and initiate keepalives
  if(chunkResponseType(thisChunk) == MSG_RESP_ACK)
    {
      // pull neighbor from packet, same position as how we initially sent it
      Uid neighbor = charToGUID(&(thisChunk->data[4]));
      
      // re-add timer for keepalive purposes
      ttNeighbor[faceNum(thisChunk)].period = NEIGHBOR_KEEPALIVE_PERIOD;
      if(ttNeighbor[faceNum(thisChunk)].t.state != INACTIVE)
	{
	  deregisterTimeout(&(ttNeighbor[faceNum(thisChunk)].t));
	}
      ttNeighbor[faceNum(thisChunk)].t.callback = (GenericHandler)&neighborKeepalive;
      enableTimer(ttNeighbor[faceNum(thisChunk)]);
      
      // add new neighbor to neighborhood
      updateNeighbor(faceNum(thisChunk), neighbor);

    }
  else // no neighbor ACK!  begin rescanning
    {
#ifdef ENSEMBLE_DEBUG
      printf ("\x1b[33m--%d--\tHandshake Lost!\x1b[0m\n", getGUID());
#endif
      restartScan(faceNum(thisChunk));
    }
  
  freeChunk(thisChunk);
}

// called for all NEIGHBOR_MSG type reserved system handlers
byte handleNeighborMessage(void)
{
  if( thisChunk == NULL ) 
    {
      return 0;
    }

/* #ifdef ENSEMBLE_DEBUG */
/*   printf ("\x1b[34m--%d--\tIn handleNeighborMessage\x1b[0m\n", getGUID()); */
/* #endif */
  
  switch(thisChunk->data[1])
    {
      // a neighbor wants to initiate a response with us, craft a response
    case MSG_NEIGHBOR_START:
      {
	// have we already received an ACK to our own NEIGHBOR_MSG?  If not, respond
	if ( ((ttNeighbor[faceNum(thisChunk)]).period 
	      != NEIGHBOR_HANDSHAKE_TIMEOUT_PERIOD) 
	|| ignoredHandshakeCount[faceNum(thisChunk)] > 1)
	  {
	    // build a neighbor handshake packet
	    // of the form:
	    // [handshake header] [my guid] [your guid]
	    byte buf[6];
	    buf[0] = NEIGHBOR_MSG;
	    buf[1] = MSG_NEIGHBOR_RESPONSE;
	    GUIDIntoChar(getGUID(), &(buf[2]));
	    buf[4] = thisChunk->data[2];
	    buf[5] = thisChunk->data[3];
	
	    // if our neighbor is already vacant, nothing happens.
	    // if this guid == old guid, they got reset somehow, so drop neighbor so user can re-add gracefully
	    // if this guid != old guid, then we got a new block, so drop neighbor so user can re-add gracefully
	    updateNeighbor(faceNum(thisChunk), VACANT);
	    
	    // stop any imminent timeouts such as neighborscanning
	    disableTimer(ttNeighbor[faceNum(thisChunk)]);
	    if(ttNeighbor[faceNum(thisChunk)].t.state != INACTIVE)
	      {
		deregisterTimeout(&(ttNeighbor[faceNum(thisChunk)].t));
	      }

	    /* Reset count*/
	    ignoredHandshakeCount[faceNum(thisChunk)] = 0;

	    // callback will add neighbor or restart scanning, as necessary
	    sendSystemMessage(faceNum(thisChunk), buf, 6, RES_SYS_HANDLER, (GenericHandler)&neighborHandshakeCB);
	  }
	// else we are waiting for handshake, stay in NEIGHBOR_HANDSHAKE and don't send a response message
#ifdef ENSEMBLE_DEBUG
	else {
	  printf ("\x1b[33m--%d--\tAlready waiting for Handshake: %d\x1b[0m\n", 
		  getGUID(), ++ignoredHandshakeCount[faceNum(thisChunk)]);	  
	}
#else
	else ++ignoredHandshakeCount[faceNum(thisChunk)];
#endif
	// (they are probably sending one to us right now)
	break;
      }
      // got a response to our request for a neighbor handshake
    case MSG_NEIGHBOR_RESPONSE:
      {
	/* Count can now be reset */
	ignoredHandshakeCount[faceNum(thisChunk)] = 0;

	// are we waiting for a handshake?  well, here it is!
	if((ttNeighbor[faceNum(thisChunk)]).period == NEIGHBOR_HANDSHAKE_TIMEOUT_PERIOD)
	  {
	    Uid me, neighbor;
	    
	    // pull [your guid][my guid] from the handshake packet - reversed from how it was sent
	    neighbor = charToGUID(&(thisChunk->data[2]));
	    me = charToGUID(&(thisChunk->data[4]));
	    
	    // got a sensible response
	    if(me == getGUID())
	      {
		// stop any imminent rescans
		disableTimer(ttNeighbor[faceNum(thisChunk)]);
		if(ttNeighbor[faceNum(thisChunk)].t.state != INACTIVE)
		  {
		    deregisterTimeout(&(ttNeighbor[faceNum(thisChunk)].t));
		  }
		
		// re-add timer, but now for keepalive purposes
		ttNeighbor[faceNum(thisChunk)].period = NEIGHBOR_KEEPALIVE_PERIOD;
		ttNeighbor[faceNum(thisChunk)].t.callback = (GenericHandler)&neighborKeepalive;
		enableTimer(ttNeighbor[faceNum(thisChunk)]);
		
		// add new neighbor to neighborhood
		updateNeighbor(faceNum(thisChunk), neighbor);

		// a good place to check and store the time, if we wanted to keep track of neighbor uptime
	      }
	    // else we got a bad handshake;  either it will come or we'll start scanning again on timeout
#ifdef ENSEMBLE_DEBUG
	    else  
	      printf ("\x1b[33m--%d--\tBad Handshake 1\x1b[0m\n", getGUID());
#endif
	  }
	// else we got a bad handshake;  either it will come or we'll start scanning again on timeout
#ifdef ENSEMBLE_DEBUG
	  else
	    printf ("\x1b[33m--%d--\tBad Handshake 2\x1b[0m\n", getGUID());
#endif
	break;
      }
    case MSG_NEIGHBOR_KEEPALIVE:
      {
	// are we currently looking for keepalives?  if yes, here it is!
	if((ttNeighbor[faceNum(thisChunk)]).period == NEIGHBOR_KEEPALIVE_PERIOD)
	  {
	    Uid me, neighbor;

	    neighbor = charToGUID(&(thisChunk->data[2]));
	    me = charToGUID(&(thisChunk->data[4]));

	    // got a bad or unexpected response - clear neighbor and restart scan
	    if(me != getGUID() || neighbor != thisNeighborhood.n[faceNum(thisChunk)])
	      {
#ifdef ENSEMBLE_DEBUG
		printf ("\x1b[33m--%d--\tUnexpected keepalive\x1b[0m\n", getGUID());
#endif
		restartScan(faceNum(thisChunk));
	      }
	  }
	// else do nothing;  we are already in another state actively trying to acquire a neighbor
        break;
      }
    default: 
      break;  
    }
  return 1;
}

// sets neighbor to VACANT, and starts a timer/timeout to send handshake initialization messages
void restartScan(PRef i)
{
/* #ifdef ENSEMBLE_DEBUG */
/*   printf ("\x1b[34m--%d--\tIn restartScan\x1b[0m\n", getGUID()); */
/* #endif */

  updateNeighbor(i, VACANT);

  // deregister timer and deregister timeouts if they are already in the lists
  disableTimer(ttNeighbor[i]);
  if(ttNeighbor[i].t.state != INACTIVE)
    {
      deregisterTimeout(&(ttNeighbor[i].t));
    }

  // set up timer to call neighbor scan
  (ttNeighbor[i]).t.arg = i;
  (ttNeighbor[i]).t.callback = (GenericHandler)&neighborScan;
  (ttNeighbor[i]).period = NEIGHBOR_SCAN_PERIOD;
  enableTimer(ttNeighbor[i]);
}

void initEnsemble(void)
{
  int i;

/* #ifdef ENSEMBLE_DEBUG */
/*   printf ("\x1b[34m--%d--\tIn initEnsemble\x1b[0m\n", getGUID()); */
/* #endif */
  
  // restart scanning for neighbors on all ports
  for( i=0; i<NUM_PORTS; ++i)
    {
#ifdef DEBUGPORT
      if(i == DEBUGPORT) {
	continue;
      }
#endif
      ignoredHandshakeCount[i] = 0;

      registerTimer(&(ttNeighbor[i]));
      disableTimer(ttNeighbor[i]);
      restartScan(i);
    }
}

