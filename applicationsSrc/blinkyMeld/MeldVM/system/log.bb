#include <stdarg.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>

#include "log.bbh"
#include "ensemble.bbh"
#include "data_link.bbh"
#include "serial.bbh"

#define FILENUM 4

#define UNDEFINED_HOST 200

#define LOG_I_AM_HOST			0x01
#define LOG_PATH_TO_HOST		0x02
#define LOG_NEED_PATH_TO_HOST		0x03 // UNNECESSARY AND CAUSING MEMORY ISSUES
#define LOG_DATA			0x04
#define LOG_CMD			        0x05
#define LOG_OUT_OF_MEMORY               0x06
#define LOG_ASSERT                      0x07 

//CMD TYPES
#define COLOR_SET	0x10
#define ENSEMBLE_RESET	0x11
#define SET_ID          0x12

static byte getSize(char* str);
static void freeLogChunk(void);
static Chunk* getLogChunk(void);

//#define FORCE_TRANSMISSION

threadvar byte PCConnection = 0;
threadvar PRef toHost = 200; //= NUM_PORT; // UNDEFINED_HOST (compilation error with SIM)
threadvar byte seq = 0; // sequence number for avoiding loops when broadcasting commands

//////////////////// PUBLIC FUNCTIONS /////////////////////
// Send a log string to host
byte printDebug(char* str) {
  byte size = getSize(str);
  static byte mId = 0;
  byte index = 0;
  byte buf[DATA_SIZE];
  byte s = 0;	
  byte fId = 0;
  byte off = 6;
  //byte random =  rand() % 45 + 1; 
  if (toHost == UNDEFINED_HOST)
    {
      return 0;
    }
	
  buf[0] = LOG_MSG;
  buf[1] = LOG_DATA;
  GUIDIntoChar(getGUID(), &(buf[2]));
  buf[4] = mId;
	
  if (size == 1) 
    {
      off = 7;
      s = strlen(str)+1;
      memcpy(buf+off, str, s);
      buf[5] = 0;	
      buf[6] = size;
      sendLogChunk(toHost, buf, s+off);
    } 
  else
    {	
      for (fId = 0; fId < size; fId++)
	{
	  buf[5] = fId;
	  if (fId == 0)
	    {
	      buf[6] = size;
	      off = 7;
	      s = 10;
	    }
	  else if (fId == (size -1))
	    {
	      s = strlen(str+index)+1 ;
	    } 
	  else
	    {
	      s = 11;
	    }
	  memcpy(buf+off, str+index, s);
	  index += s;
	  //delayMS(random);
	  sendLogChunk(toHost, buf, s+off);
	  off = 6;
	}
    }
  mId++;
  return 1;
}

byte blockingPrintDebug(char *s)
{
  while(toHost == UNDEFINED_HOST)
    {
      delayMS(1);
    }
  return printDebug(s);
}
////////////////// END PUBLIC FUNCTIONS ///////////////////

//////////////////// SYSTEM FUNCTIONS /////////////////////
//
// -------------- HOST DISCOVERY FUNCTIONS

void initLogDebug(void)
{
  //byte buf[2];
	
  toHost = UNDEFINED_HOST;
  PCConnection = 0;
	
  /*buf[0] = LOG_MSG;
  buf[1] = LOG_NEED_PATH_TO_HOST;
  byte p;*/
	
  setColor(ORANGE); // to remember to the user that the block is waiting
  while(toHost == UNDEFINED_HOST)
    {
      /* for( p = 0; p < NUM_PORTS; p++)
	{
	  if ((thisNeighborhood.n[p] == VACANT))
	    {
	      continue;
	    }
	  sendLogChunk(p, buf, 2, __LINE__);
	  }*/
      delayMS(500);
    }
  srand(getGUID());
}

byte isHostPort(PRef p)
{
  return ((p == toHost) && (PCConnection == 1));
}

static
void sendPathToHost(PRef p)
{
  byte buf[2];
	
  buf[0] = LOG_MSG;
  buf[1] = LOG_PATH_TO_HOST;
  sendLogChunk(p, buf, 2);
}

static
void spreadPathToHost(PRef excluded)
{
  byte p;
  
  for( p = 0; p < NUM_PORTS; p++) {
    if ((p == excluded) || (thisNeighborhood.n[p] == VACANT)) {
      continue;
    }
    sendPathToHost(p);
  }
}

static
void forwardToHost(Chunk *c)
{
  if(toHost != UNDEFINED_HOST) {
    sendLogChunk(toHost, c->data, DATA_SIZE);
  }
}

// ------------------ CRITICAL DEBUGGING FUNCTIONS

Chunk emergencyChunk;

void
reportLoggerOutOfMemory(PRef failurePort)
{
  Chunk* c = &emergencyChunk;
  emergencyChunk.next = NULL;
  emergencyChunk.status = CHUNK_USED;
  byte buf[DATA_SIZE];
  buf[0] = LOG_MSG;
  buf[1] = LOG_OUT_OF_MEMORY;
  buf[2] = failurePort;
  GUIDIntoChar(getGUID(), &(buf[3]));
  sendMessageToPort(c, toHost, buf, 5, (MsgHandler)RES_SYS_HANDLER, (GenericHandler)&freeLogChunk);
  return; 
}

void
reportAssert(byte fn, int ln)
{
  Chunk* c = &emergencyChunk;
  emergencyChunk.next = NULL;
  emergencyChunk.status = CHUNK_USED;
  byte buf[DATA_SIZE];
  buf[0] = LOG_MSG;
  buf[1] = LOG_ASSERT; 
  GUIDIntoChar(getGUID(), &(buf[2]));
  buf[4] = fn;
  buf[5] = 0xff & (ln>>8);
  buf[6] = ln & 0xff;
  sendMessageToPort(c, toHost, buf, 7, (MsgHandler)RES_SYS_HANDLER, (GenericHandler)&freeLogChunk);
  return; 
}

// --------------- CHUNK SENDING FUNCTIONS

byte 
sendCmdChunk(PRef p, byte *d, byte s, MsgHandler mh) 
{
  Chunk *c=getLogChunk();
  if (c == NULL) {
    reportLoggerOutOfMemory(p);
    //setColor(PINK);
    return 0;
  }
  if (sendMessageToPort(c, p, d, s, mh, (GenericHandler)&freeLogChunk) == 0) {
    freeChunk(c);
    return 0;
  }
  return 1;
}

// send message in d to port p for logging to host
byte sendLogChunk(PRef p, byte *d, byte s)
{
  return sendCmdChunk(p, d, s, (MsgHandler)RES_SYS_HANDLER);
}

// --------------- MESSAGE HANDLING FUNCTIONS

void 
commandHandler(void)
{
  switch (thisChunk->data[2]) {
    case COLOR_SET:
      callHandler(EVENT_COMMAND_RECEIVED);
      break;
    case SET_ID:
      callHandler(EVENT_COMMAND_RECEIVED);
      break;
    case ENSEMBLE_RESET:
      callHandler(EVENT_COMMAND_RECEIVED);
      break;
    }
}

byte 
handleLogMessage(void)
{
  if( thisChunk == NULL ) 
    {
      return 0;
    }

  switch(thisChunk->data[1])
    {
    case LOG_I_AM_HOST:
	  setColor(WHITE);
      toHost = faceNum(thisChunk);			
      PCConnection = 1;
      spreadPathToHost(faceNum(thisChunk));
      break;
    case LOG_PATH_TO_HOST:
      if (toHost == UNDEFINED_HOST) {
      setColor(WHITE);
	toHost = faceNum(thisChunk);
	spreadPathToHost(faceNum(thisChunk));
      }
      break;
      /*case LOG_NEED_PATH_TO_HOST:
      if (toHost != UNDEFINED_HOST) {
	sendPathToHost(faceNum(thisChunk));
      }
      break;*/
    case LOG_DATA:
      if(toHost != UNDEFINED_HOST) {
	forwardToHost(thisChunk);
      }
      break;		
    case LOG_CMD:
      commandHandler();
      break;
    case LOG_ASSERT:
      if(toHost != UNDEFINED_HOST) forwardToHost(thisChunk);
      break;
    case LOG_OUT_OF_MEMORY:
      if(toHost != UNDEFINED_HOST) forwardToHost(thisChunk);
      break;
    }	
  return 1;
}

// --------------- CHUNK MANAGEMENT

threaddef #define NUM_LOG_CHUNK 35
threadvar Chunk logChunkPool[NUM_LOG_CHUNK];

static Chunk* getLogChunk(void)
{
  //Chunk *p = getSystemTXChunk();
  //return p;
  byte i = 0;
  Chunk *cp = NULL;
  for(i = 0; i < NUM_LOG_CHUNK ; i++) {
     // check top bit to indicate usage
     cp = &logChunkPool[i];
     if(!chunkInUse(cp) ) {
		// indicate in use
        cp->status = CHUNK_USED;
        cp->next = NULL;
        return cp;
     }
  }
  return NULL;
}

static void 
freeLogChunk(void)
{
  freeChunk(thisChunk);
  thisChunk->status = CHUNK_FREE;
}

// ------------- UTILITY

static byte getSize(char* str) {
  byte sizeCar = 0;
  byte sizeChunk = 1;
	
  if (str == NULL) {
    return 0;
  }
	
  sizeCar = strlen(str) + 1;

  if (sizeCar < 11) {
    return 1;
  }
    
  sizeCar -= 10;
  sizeChunk += sizeCar / 11;
  if ((sizeCar % 11) != 0) {
    sizeChunk++;
  }
  return sizeChunk;
}

////////////////// END SYSTEM FUNCTIONS ///////////////////
