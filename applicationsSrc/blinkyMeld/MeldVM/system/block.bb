#include "block.bbh"

#ifdef CLOCK_SYNC
#include  "clock.bbh"
#endif
#ifdef LOG_DEBUG
#include "log.bbh"
#endif

threadvar int blockTickRunning = 0;

int accelReady=0;

// BlockTick
//
// Polling-based hack to step through and update block state, as necessary.
//
// Much of this can probably be done via ISRs and other state change triggers and this function eliminated.
void blockTick()
{
  byte i;
  
#ifdef BBSIM
  if (this()->destroyed == 1) {
    // let neighbors know we are gone
    //tellNeighborsDestroyed(this());
    // and never do anything again
    pauseForever();
  }
#endif
  //int input;
  blockTickRunning = 1;
  
  if(accelReady){
    if(newAccelData()){
      updateAccel();
    }
  }
  
  checkTimeout();
  
  checkTimer();
  
  for(i = 0; i < NUM_PORTS; ++i)
    {
      // read from serial
      processBuffer(i);
      
      // active messaging (handle at most one per port)
      handleOneMessage();
      
      //send packets/ACKS
      sendOnSerial(i);
    }
  
  executeHandlers();	
  if(accelReady){
    if(newAccelData()){
      updateAccel();
    }
  }
  executeHandlers();	

#ifdef MELD
  /* databaseConsistencyChecker(); */
#endif

  blockTickRunning = 0;
  
}


// Ties all the horrifying subfunctions together into one simple function
void initBlock()
{
	//software initialization
	initHandlers();

	//hardware related initialization
	initTime();

	initializeMemory();

	initPorts();

#ifdef DEBUG
	initDebug();
    	//printf("System Debug Enabled\r\n");
#endif

	initDataLink();	

   	initHWLED();
	//initAudio();

	initSystemMessage();
	initEnsemble();

	initBlockTick();		// HW INITIALIZATION ROUTINE

	initHWAccel();
	accelReady=1;

#ifdef LOG_DEBUG
	initLogDebug();
#endif

#ifdef MELD
	//allocate MeldVM's data structures
	vm_alloc();
#endif
	delayMS(50);
#ifdef CLOCK_SYNC
	initClock();
#endif

#ifndef BBSIM
    initHWMic();
#endif
}

