// memory.c
//
// Implement Memory Management protocols

#ifndef _MEMORY_C_
#define _MEMORY_C_

#include "memory.bbh"

#ifdef TESTING
#define FILENUM 1
#define checkMemoryConsistency() _checkMemoryConsistency(FILENUM, __LINE__)
#include "message.bbh"
#include "log.bbh"
#else
#define checkMemoryConsistency()
#endif

threaddef #define NUM_RXCHUNKS 24
threaddef #define NUM_TXCHUNKS 24

// types of chunks to free
#define RXCHUNK 0
#define TXCHUNK 1

threadvar byte numFreeChunks;
threadvar char allocated;	/* track num chunks allocated, signed val  */
threadvar Chunk rxChunks[NUM_RXCHUNKS];
threadvar Chunk txChunks[NUM_TXCHUNKS];

threadvar blockConf EEMEM nv_conf;
threadvar blockConf conf;

//////////////////// PUBLIC FUNCTIONS /////////////////////
// set-up memory
void initializeMemory(void)
{
    uint8_t i;

    // clear all status bits and next pointers
    // clear all status bits for receive chunks
    for( i=0; i<NUM_RXCHUNKS; i++ )
    {
        rxChunks[i].status = CHUNK_FREE;
	rxChunks[i].next = NULL;
    }
    
    // clear all status bits for send chunks
    for( i=0; i<NUM_TXCHUNKS; i++ )
    {
        txChunks[i].status = CHUNK_FREE;
	txChunks[i].next = NULL;
    }

    // init allocation counters
    allocated = 0;

    // load config data
    //TODO: re-enable
    //restore(&conf, &nv_conf, sizeof(blockConf));
}

// this loops through and frees all connected Chunks in the list.
void freeChunk(Chunk * c)
{
  Chunk * tmp;	

  checkMemoryConsistency();
  while(c != NULL) {
    if(chunkInUse(c)) {
      c->status = CHUNK_FREE;
      allocated--;
#ifdef TESTING      
      assert(allocated >= 0);
#endif
    }
    tmp = c->next;
    c->next = NULL;
    c = tmp;
  }
}

//static 
Chunk* getSystemChunk(byte which)
{
    checkMemoryConsistency();
    int8_t i;
    Chunk*  current;

    if(which == RXCHUNK) {
        current = rxChunks;
        
        i = NUM_RXCHUNKS-1;
    } else {
        current = txChunks;
        
        i = NUM_TXCHUNKS-1;
    }

    // look for unused Chunk
    for(; i>=0; i--) {
        // check top bit to indicate usage
        if( !chunkInUse((&(current[i]))) ) {
            // indicate in use
            Chunk* cp = &(current[i]);
            cp->status = CHUNK_USED;
            // clear old next ptr in case non-NULL
#ifdef TESTING 	  
            assert(cp->next == NULL);
#endif
            cp->next = NULL;
            allocated++;
            checkMemoryConsistency();
            return cp;
        }
        // else, in use (supposedly)
    }
    // this assumes NUM_TXCHUNKS <= NUM_RXCHUNKS
#ifdef TESTING    
    assert(allocated >= NUM_TXCHUNKS);
#endif    
    checkMemoryConsistency();
    return NULL;  
}

// return pointer to free memory Chunk
Chunk* getSystemRXChunk(void)
{
  return getSystemChunk(RXCHUNK);
}

Chunk* 
getSystemTXChunk(void)
{
  return getSystemChunk(TXCHUNK);
}

// check a pool for consistency, return number in use
static byte 
checkMemoryPool(Chunk* pool, byte num)
{
  byte used = 0;
  byte i = 0;
  for( i=0; i<num; i++ ) {
    Chunk* cp = &(pool[i]);
    if (chunkInUse(cp)) used++;
#ifdef TESTING
    else assert(cp->next == NULL);
#endif
  }
  return used;
}


// Can be used for further debugging if blocks often get out of memory
/*static void
sendOOM(int cln)
{
    char buffer[64];
    buffer[0] = cln;
    int j=0;
    for( byte i=0; i<NUM_RXCHUNKS; i++ ) {
        if (chunkInUse((&(rxChunks[i])))) {
            buffer[j++] = rxChunks[i].fn;
            buffer[j++] = rxChunks[i].ln;
        }
    }
    for( byte i=0; i<NUM_TXCHUNKS; i++ ) {
        if (chunkInUse((&(txChunks[i])))) {
            buffer[j++] = txChunks[i].fn;
            buffer[j++] = txChunks[i].ln;
        }
    }
    buffer[j] = 0;
    printDebug(buffer);
    setColor(INDIGO);
    delayMS(500);
    }*/

// Memory check function, in case check fails, sends log to host with file and line of failed check
void 
_checkMemoryConsistency(byte cfn, int cln)
{
  int used = checkMemoryPool(rxChunks, NUM_RXCHUNKS);
  used += checkMemoryPool(txChunks, NUM_TXCHUNKS);
#ifdef TESTING
  if (used != allocated) {
      setColor(BROWN);
      while (1) {
          reportAssert(cfn, cln);
          delayMS(1000);
      }
  }
  //if (used > 12) sendOOM(cln);
#endif 
}
////////////////// END PUBLIC FUNCTIONS ///////////////////

#endif

// Local Variables:
// mode: C
// indent-tabs-mode: nil
// c-basic-offset: 4
// End:

