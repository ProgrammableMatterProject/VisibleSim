#include "coordinate.bbh" 
#include "block.bbh"

//PRIVATE VARIABLES
threadvar byte countChildren;
threadvar byte numCoord = 1;

//coordination fuction
threadvar signed char coord[30][4];

threadvar signed char xCoord = 0; 
threadvar signed char yCoord = 0;
threadvar signed char zCoord = 0;

threadvar byte haveCoor = 0;
threadvar byte alreadyShared = 0;
threadvar Timeout propagateCoord;
threadvar Timeout logTimeout;
threadvar int numMsg = 0;

#define MSG	    0x15
#define COOR	    0x16
#define SEND_COOR   0x17
#define VIRTUAL     0x18

#define ROUTE_NOT_DEFINED_YET 6

// Chunk management
threaddef #define NUM_COORD_CHUNKS 12
threadvar Chunk coordChunks[NUM_COORD_CHUNKS];
static Chunk* getFreeCoordChunk(void);

//PRIVATE FUNCTIONS
static void startCoordination(void);
static void startCoordination (void); //start giving the coordinates to the ensemble
static void sendCoordination (void);// start sharing the coordinates to each other
static void sendMsg(PRef p, signed char x, signed char y, signed char z);// message sent when giving coordinates
static void sendCoord(PRef p, signed char cx, signed char cy, signed char cz); // message sent during the exchange of coordinates
static byte checkneighbor(PRef p); //check the neighbor on the port p if it exists
static byte coordinateHandler(void); //handler for all the coordinate system
static void sendToVirtualNeighbor(PRef p, byte *data,byte size, signed char cx, signed char cy, signed char cz); //message sent to the virtual neighbor
static byte sendCoordChunk(PRef p, byte *data, byte size, MsgHandler mh);
MsgHandler vhandler;

void sendCoord(PRef p, signed char cx, signed char cy, signed char cz)
{
  byte data[4];
  
  data[0] = SEND_COOR;
  data[1] = cx;
  data[2] = cy;
  data[3] = cz;
  sendCoordChunk(p, data, 4, (MsgHandler)&coordinateHandler);
}

void sendLog(void)
{
#ifdef LOG_DEBUG
  signed char s[10];
  snprintf(s, 10*sizeof(signed char), "%d",numCoord);
  printDebug(s);
#endif 
}

void sendMsg(PRef p, signed char x, signed char y, signed char z)
{ 
  byte data[4];
  
  data[0] = COOR;
  data[1] = x;
  data[2] = y;
  data[3] = z;
  sendCoordChunk(p, data, 4, (MsgHandler)&coordinateHandler); 
}

void sendToVirtualNeighbor(PRef p,byte *data, byte size, signed char cx, signed char cy, signed char cz)
{
  byte buf[size + 5];
  buf[0] = VIRTUAL;
  buf[1] = cx;
  buf[2] = cy;
  buf[3] = cz;
  buf[4] = size;
  memcpy(buf + 5, data, size*sizeof(byte));
  sendCoordChunk(p, buf, size + 5, (MsgHandler)&coordinateHandler);
}

void startCoordination(void)
{
  coord[0][0] = xCoord;
  coord[0][1] = yCoord;
  coord[0][2] = zCoord;
  coord[0][3] = ROUTE_NOT_DEFINED_YET;
  haveCoor = 1;
  setColor(RED);
  if( checkneighbor(UP) == 1){
    sendMsg(UP,xCoord,yCoord+1,zCoord);
  }
  if( checkneighbor(DOWN) == 1){
    sendMsg(DOWN,xCoord,yCoord-1,zCoord);
  }
  if( checkneighbor(WEST) == 1){
    sendMsg(WEST,xCoord+1,yCoord,zCoord);
  }
  if( checkneighbor(EAST) == 1){
    sendMsg(EAST,xCoord-1,yCoord,zCoord);
  }
  if( checkneighbor(NORTH) == 1){
    sendMsg(NORTH,xCoord,yCoord,zCoord+1);
  }
  if( checkneighbor(SOUTH) == 1){
    sendMsg(SOUTH,xCoord,yCoord,zCoord-1);
  }
  //Share his coordinates after 1 second
  propagateCoord.calltime = getTime() + 500;
  registerTimeout(&propagateCoord);
}

void sendCoordination(void)
{
  alreadyShared = 1;
  byte p;
  for (p = 0; p < NUM_PORTS; p++) {
    if (thisNeighborhood.n[p] != VACANT) {
      sendCoord(p, xCoord, yCoord, zCoord);
    }
  }    
}

byte checkneighbor(PRef p)
{
  if (thisNeighborhood.n[p] != VACANT)
    {
      return 1;
    }
  return 0;
}

byte
coordinateHandler(void)
{
  if (thisChunk == NULL) return 0;
  byte messageType = thisChunk->data[0];
  byte chunkSource = faceNum(thisChunk);
  switch (messageType) {
  case COOR:
    {
      if (haveCoor == 0)
	{
	  xCoord = thisChunk->data[1];
	  yCoord = thisChunk->data[2];
	  zCoord = thisChunk->data[3];
  
	  byte p;
	  for(p = 0; p < NUM_PORTS;p++){
	    if (thisNeighborhood.n[p] != VACANT &&  p != chunkSource) {	
	      if( UP == p){
		sendMsg(p,xCoord,yCoord+1,zCoord);
       
	      }
	      if( DOWN == p ){
		sendMsg(p,xCoord,yCoord-1,zCoord);
	      }
	      if(WEST == p) 
		{
		  sendMsg(p,xCoord+1,yCoord,zCoord);
		}
	      if(EAST == p) 
		{
		  sendMsg(p,xCoord-1,yCoord,zCoord);
		}
	      if(NORTH == p)
		{
		  sendMsg(NORTH,xCoord,yCoord,zCoord+1);
		}
	      if(SOUTH == p)
		{
		  sendMsg(NORTH,xCoord,yCoord,zCoord-1);
		}
	    }
	  }
	  haveCoor = 1;
	  setColor(AQUA);
	}
    }
    break;
  case SEND_COOR:
    {
      signed char cx = thisChunk->data[1];
      signed char cy = thisChunk->data[2];
      signed char cz = thisChunk->data[3];
      coord[0][0] = xCoord;
      coord[0][1] = yCoord;
      coord[0][2] = zCoord;
      coord[0][3] = 6;
  
      byte i;
      for(i = 1; i<numCoord; i++)
	{
	  if( coord[i][0] == cx && coord[i][1] == cy && coord[i][2] == cz)
	    {
	      return 0;
	    }
	  else continue;
	}
   
      coord[numCoord][0] = cx;
      coord[numCoord][1] = cy;
      coord[numCoord][2] = cz;
      coord[numCoord++][3] = faceNum(thisChunk);
      byte p;
      for (p = 0; p < NUM_PORTS;p++){
	if (thisNeighborhood.n[p] != VACANT &&  p != chunkSource) {	
	  sendCoord(p, cx, cy,cz);
	}
      } 
      if(alreadyShared == 0)
	{
	  setColor(YELLOW);
	  sendCoordination();
	  /* propagateCoord.calltime = getTime() + 1000;
	     registerTimeout(&propagateCoord); */
	  alreadyShared = 1;
	}
    }
    break;
  case VIRTUAL:
    {
      setColor(AQUA);
      signed char cx = thisChunk->data[1];
      signed char cy = thisChunk->data[2];
      signed char cz = thisChunk->data[3];
      byte size = thisChunk->data[4];
      byte buf[size];
      memcpy(buf, thisChunk->data + 5, size*sizeof(byte));
      if( xCoord == cx && yCoord == cy && zCoord == cz)
	{
	  vhandler();
	  return 0;
	}
  
      byte i;
      for(i=0; i<numCoord; i++)
	{
	  if( coord[i][0] == cx && coord[i][1] == cy && coord[i][2] == cz)
	    {
	      sendToVirtualNeighbor(coord[i][3],buf,size, cx, cy, cz);
	      return 1;
	    }
	  else continue;
	}
    }
    break;
  }
  return 0;
}
 
// find a useable chunk
Chunk* 
getFreeCoordChunk(void)
{
  Chunk* c;
  byte i;

  for(i=0; i<NUM_COORD_CHUNKS; i++) {
    c = &(coordChunks[i]);

    if( !chunkInUse(c) ) {
      return c;
    }
  }
  return NULL;
}

byte sendCoordChunk(PRef p, byte *data, byte size, MsgHandler mh) 
{ 
  numMsg++;
  Chunk *c=getFreeCoordChunk();
  if (c == NULL) 
    {
      c = getSystemTXChunk();
      if(c == NULL){
	return 0;
      }
    }
  if (sendMessageToPort(c, p, data, size, mh, NULL) == 0) {
    freeChunk(c);
    return 0;
  }
  return 1;
}

//PUBLIC FUNCTIONS

//give coordinates to all the blocks and each blocks share their coordinates to each other, the block with the ID id will be the origin with the coordinate (0, 0, 0)
//There is a timeout to know when the blocks should share their coordinates, the variable timeout is the time for all the blocks to get coordinates before shareing these coordinates
//the timeout depends on the number of blocks in the ensemble
void  initCoordination(uint16_t id, MsgHandler donefunc)
{
  setColor(WHITE);
  // We are forced to use a small delay before program execution, otherwise neighborhood may not be initialized yet
  delayMS(300);
  vhandler = donefunc;
  // Initialize chunks
  byte i;
  for(i=0; i < NUM_COORD_CHUNKS; i++) {
    coordChunks[i].status = CHUNK_FREE;
  }
  propagateCoord.callback = (GenericHandler)(&sendCoordination);
  logTimeout.callback = (GenericHandler)(&sendLog);
  logTimeout.calltime = getTime() + 5000;
  registerTimeout(&logTimeout); 
  
  if(getGUID() == id)
    {
      startCoordination();
    }
}

//check if a virtual neighbor exist somewhere, if yes return 1 and if no return 0
byte checkVirtualNeighbor(PRef p)
{
  signed char cx = xCoord;
  signed char cy = yCoord;
  signed char cz = zCoord;
  if (p == UP){ cy = yCoord + 1 ;}
  if (p == DOWN){ cy = yCoord - 1 ;}
  if (p == EAST){ cx = xCoord - 1 ;}
  if (p == WEST){ cx = xCoord + 1 ;}
  if (p == NORTH){ cz = zCoord + 1 ;}
  if (p == SOUTH){ cz = zCoord - 1 ;}
  
  byte i;
  for(i=0; i<numCoord; i++)
    {
      if( coord[i][0] == cx && coord[i][1] == cy && coord[i][2] == cz)
	{
	  return 1;
	}
    }
  return 0;
}


//send a data to a virtual neighbor
void sendDataToVirtualNeighbor(PRef p, byte *data, byte size)
{
  if (checkVirtualNeighbor(p) == 0)
    {
      return ;
    }
  
  signed char cx = xCoord;
  signed char cy = yCoord;
  signed char cz = zCoord;
  if (p == UP){ cy = yCoord + 1 ;}
  if (p == DOWN){ cy = yCoord - 1 ;}
  if (p == EAST){ cx = xCoord - 1 ;}
  if (p == WEST){ cx = xCoord + 1 ;}
  if (p == NORTH){ cz = zCoord + 1 ;}
  if (p == SOUTH){ cz = zCoord - 1 ;}
    
  byte i;
  for(i=0; i<numCoord; i++)
    {
      if( coord[i][0] == cx && coord[i][1] == cy && coord[i][2] == cz)
	{
	  sendToVirtualNeighbor(coord[i][3], data,size, cx, cy, cz);
	  return;
	}
      else continue;
    }
}



