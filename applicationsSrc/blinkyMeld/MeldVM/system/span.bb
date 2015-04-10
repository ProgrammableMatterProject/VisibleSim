#include "span.bbh"
#include "block.bbh"
////////////////////////////////////////////////////////////////
// private functions for spanning tree code, see below for public interface
////////////////////////////////////////////////////////////////

#define MAX_SPANTREE_ID 16

// set by setSpanningTreeDebug
//static int debugmode = 0;

// list of ptrs to spanning tree structures
static SpanningTree* trees[MAX_SPANTREE_ID];

// number of already allocated spanning tree structures.
static int maxSpanId = 0;

// to know if all get into a barrier 
static int allHaveBarrier = 0;
static int reachBarrier = 0;

//variable for debugging
//static int colorDebug = 0;
//static int logDebug = 0;

//private functions
byte sendMySpChunk(byte myport, byte *data, byte size, MsgHandler mh);
void screwyou(void);
void iamyourchild(void);
void spComplete( void);
void treeBroadcastMsg(void);
void treeBroadcastBackMsg(void);
void cstHelper(void);
byte countSPChildren(byte id);
void sendToLeaf(void);
void allHaveBarrierMsg(void);
void finishingSpan(void) ;
void barrierAck(void);

//Timeout for the creation of the spanning tree
Timeout barrierTimeout;
Timeout finishTimeout;

// this is sent by a potential myParent trying to make me a child
// 0: id
// 1: value
void cstHelper(void)
{

  byte potentialID = thisChunk->data[0];
  uint16_t potentialValue = charToGUID(&(thisChunk->data[1]));
  // if we have value that is less, send back a NACK (screwyou)
  // if we have value that is ==, send back a NACK (screwyou)
  if( trees[potentialID]->value >=  potentialValue )
    {
      byte data[3]; 
      data[0] = trees[potentialID]->spantreeid;
      GUIDIntoChar(potentialValue, &(data[1]));
      sendMySpChunk(faceNum(thisChunk), data, 3, (MsgHandler)&screwyou); 
    }
  // otherwise recursively start process for all my other ports (meaning, send a cstHelper msg)
  // when recusrive procedure is done, make sender my myParent and send back an ACK (iamyourchild)
  else
    {
      trees[potentialID]->outstanding = 0;
      trees[potentialID]->value = potentialValue;
      trees[potentialID]->myParent = faceNum(thisChunk);
      trees[potentialID]->state = HAVEPARENT;
      byte i;
      for( i = 0; i<NUM_PORTS; i++)
	{
	  trees[potentialID]->myChildren[i] = 0;
	}
      trees[potentialID]->numchildren = 0;
      trees[potentialID]->state = WAITING;
      byte data[3];
      data[0] = potentialID;
      GUIDIntoChar(trees[potentialID]->value, &(data[1]));
      /* Send add yourself to all neighbors */
      byte p;
      for( p = 0 ; p < NUM_PORTS;p++){
	if ((thisNeighborhood.n[p] != VACANT) 
	    && (p != trees[potentialID]->myParent)) {	
	  trees[potentialID]->outstanding++;
	  sendMySpChunk(p, data,3, (MsgHandler)&cstHelper); 
	}
      }
      
      /* Send iamyourchild to parent */
      sendMySpChunk((trees[potentialID]->myParent), 
		    data, 3, (MsgHandler)&iamyourchild); 
       
      /* Wait until answer from all children has been received */
      /* Save current ST value */
      uint16_t loopValue = trees[potentialID]->value;
      setColor(BROWN);
      while ((trees[potentialID]->state) != DONE && 
	     (trees[potentialID]->value == loopValue));

      /* Identify reason of end of loop, return if value has changed */
      if (trees[potentialID]->value != loopValue)
	return;
      else {

	// if has no children send a back message with the value to the root 
	// to tell the spanning tree is completed
	if( trees[potentialID]->numchildren == 0)
	  {
	    byte data[3];
	    data[0] = trees[potentialID]->spantreeid;
	    GUIDIntoChar(trees[potentialID]->value, &(data[1]));
	    if(!isSpanningTreeRoot(trees[potentialID])){
	      sendMySpChunk( trees[potentialID]->myParent, data, 
			     3, (MsgHandler)&spComplete);   
	    }
	  }
      }
    }    
}
//message back to the root to complete the tree 
void spComplete(void)
{
  byte potentialID = thisChunk->data[0];
  uint16_t potentialValue = charToGUID(&(thisChunk->data[1]));

  // if the value potentialValue is different from the tree value just ignore,
  // otherwise:
  if( trees[potentialID]->value == potentialValue ){ 
    if( trees[potentialID]->outstanding != 0){
      trees[potentialID]->outstanding--;
    }
    //when received all the messages from the children send spComplete to 
    // myParent if root send message to leaves 
    if( trees[potentialID]->outstanding == 0 ){
      if( trees[potentialID]->state == DONE){
	byte data[3];
	data[0] = trees[potentialID]->spantreeid;
	GUIDIntoChar(trees[potentialID]->value, &(data[1]));
	if(!isSpanningTreeRoot(trees[potentialID]) == 0) {
	  sendMySpChunk( trees[potentialID]->myParent, data, 3, 
			 (MsgHandler)&spComplete);   
	}
	else			/* IS ROOT */
	  {
	    // if the root receive the spComplete, send message 
	    // to the leaves to tell them to execute their done function
	    byte i;
	    for(i = 0; i<NUM_PORTS; i++){
	      if(trees[potentialID]->myChildren[i] == 1){
		sendMySpChunk(i , data, 3, (MsgHandler)&sendToLeaf);   
	      }
	    }
	  }
      }
    }
  }
  
}

//send message to the leaves to launch their mydonefunc
void sendToLeaf(void)
{
  byte potentialID = thisChunk->data[0];
  uint16_t potentialValue = charToGUID(&(thisChunk->data[1]));
  byte data[3];
  data[0] = trees[potentialID]->spantreeid;
  GUIDIntoChar(trees[potentialID]->value, &(data[1]));
  
  if(trees[potentialID]->value == potentialValue){

    byte i;
    for( i = 0; i<NUM_PORTS; i++){
      if(trees[potentialID]->myChildren[i] == 1){
	sendMySpChunk(i , data, 3, (MsgHandler)&sendToLeaf);   
      }
    }
    if(trees[potentialID]->numchildren == 0)
      {
	sendMySpChunk(trees[potentialID]->myParent , data, 3,
		      (MsgHandler)&finishingSpan);
	trees[potentialID]->status = COMPLETED;
	deregisterTimeout(&trees[potentialID]->spantimeout);
	trees[potentialID]->mydonefunc(trees[potentialID],COMPLETED);
      }
  }   
}

//function sent to execute the donefunc from the leaves to the root
void finishingSpan(void) 
{
  byte potentialID = thisChunk->data[0];
  uint16_t potentialValue = charToGUID(&(thisChunk->data[1]));
  byte data[3];
  data[0] = trees[potentialID]->spantreeid;
  GUIDIntoChar(trees[potentialID]->value, &(data[1]));
  if(trees[potentialID]->value == potentialValue){
    deregisterTimeout(&trees[potentialID]->spantimeout);
    if(!isSpanningTreeRoot(trees[potentialID])){
      sendMySpChunk(trees[potentialID]->myParent , data, 3, 
		    (MsgHandler)&finishingSpan);
      trees[potentialID]->mydonefunc(trees[potentialID],COMPLETED);
      trees[potentialID]->status = COMPLETED;
    }
    else{
      trees[potentialID]->mydonefunc(trees[potentialID],COMPLETED);
      trees[potentialID]->status = COMPLETED;
    }
  }
}

// a message receives when a block doesn't want to enter a tree and doesn't want to be the block sender child 
void screwyou(void)
{
  byte potentialID = thisChunk->data[0];
  uint16_t potentialValue = charToGUID(&(thisChunk->data[1]));
  if( potentialValue == trees[potentialID]->value ){
    trees[potentialID]->myChildren[faceNum(thisChunk)] = 0;
    trees[potentialID]->numchildren = countSPChildren(potentialID);
    trees[potentialID]->outstanding--;
    if( trees[potentialID]->outstanding == 0)
      {
	trees[potentialID]->state = DONE;
	trees[potentialID]->outstanding = trees[potentialID]->numchildren;
      }
  }
}

// a message receives when a block want to enter a tree and want to be the block sender child 
void iamyourchild(void)
{
  byte potentialID = thisChunk->data[0];
  uint16_t potentialValue = charToGUID(&(thisChunk->data[1]));
  if( potentialValue == trees[potentialID]->value ){
    trees[potentialID]->outstanding--;
    trees[potentialID]->myChildren[faceNum(thisChunk)] = 1;
    trees[potentialID]->numchildren = countSPChildren(potentialID);
    
    if( trees[potentialID]->outstanding == 0)
      {
	trees[potentialID]->state = DONE;
	trees[potentialID]->outstanding = trees[potentialID]->numchildren;
      }
  }
}

byte countSPChildren(byte spId)
{
  byte count = 0;
  byte i;
  for(i = 0 ; i<NUM_PORTS; i++)
    {
      if(trees[spId]->myChildren[i] == 1)
	{
	  count++;
	}
    }	
  return count;
}

byte sendMySpChunk(byte myport, byte *data, byte size, MsgHandler mh) 
{ 
  Chunk *c=getSystemTXChunk();
 
  if (sendMessageToPort(c, myport, data, size, mh, NULL) == 0) {
    freeChunk(c);
    return 0;
  }
  else
    {
      return 1;
    }
}

// called when spanning tree times out 
void spCreation(void)
{
  /* byte d = trees[id]->spantimeout.arg; */
  /* trees[targetid]->mydonefunc(trees[id],TIMEDOUT); */
  /* trees[id]->status = TIMEDOUT; */
}

//message send to myParent if only all the children get into a barrier 
void barrierMsg(void){
  byte  spID = thisChunk->data[0];
 check:
  if( allHaveBarrier == 1 ){
    trees[spID]->outstanding--; 
    byte buf[1];
    buf[0] = spID;
    if( trees[spID]->outstanding == 0)
      {
 
	if (isSpanningTreeRoot(trees[spID]) == 1){//if root received all messages from its children, it will send messages to all the tree to tell that all the block get to a barrier
	  trees[spID]->status = COMPLETED;  // The status is changed to break the while loop inside the treeBarrier function so that the root is able to send allHaveBarrier children to the whole ensemble
	}
	else{
	  sendMySpChunk(trees[spID]->myParent, buf, 1, (MsgHandler)&barrierMsg); // send message to myParent to make their status COMPLETED
	}
      }
  }
  else 
    {
      goto check;
    }
}

//timeout for checking if all the blocks get into a barrier
void checkBarrier(void)
{
  /* byte id = barrierTimeout.arg; */
  /* trees[id]->status = TIMEDOUT; */
}

//message sent from the root to the leaves to say that all the blocks get into a barrier 
void allHaveBarrierMsg(void)
{
  
  byte  spID = thisChunk->data[0];
  byte buf[1];
  buf[0] = spID;
  byte p ;
  if( reachBarrier == 0){
    for( p = 0 ; p < NUM_PORTS ; p++){ //send the message allHaveBarrier to all the neighbor except the sender of this message
      if (thisNeighborhood.n[p] != VACANT && p != faceNum(thisChunk)) {	
	sendMySpChunk(p, buf, 1, (MsgHandler)&allHaveBarrierMsg);   //this message will change the status of the tree into completed and break their while loop inside the treeBarrier function
      }}
    reachBarrier = 1;
  }
  trees[spID]->status = COMPLETED;
  
}


//handler for sending the data to all the tree
void treeBroadcastMsg(void){
  byte  spID = thisChunk->data[0];
  byte  size = thisChunk->data[1];
  byte buf[size + 2];
  buf[0] = spID;
  buf[1] = size;
  memcpy(buf+2, thisChunk->data+2, size*sizeof(byte));//the data will start from buf[2] if users want to use it
  byte p; 
  for( p = 0; p < NUM_PORTS;p++){ //send data to all the children
    if (trees[spID]->myChildren[p] == 1) {	
      sendMySpChunk(p, buf, size + 2, (MsgHandler)&treeBroadcastMsg); 
    }
  }
  if( trees[spID]->numchildren == 0 ){
    byte data[1];
    data[0] = spID;
    sendMySpChunk(trees[spID]->myParent, data, 1, (MsgHandler)&treeBroadcastBackMsg); 
    trees[spID]->broadcasthandler();
  } 
}

//back message handler, when the block receive all the message from its children execute the broadcasthandler
void treeBroadcastBackMsg(void){
  byte  spID = thisChunk->data[0];
  if(trees[spID]->outstanding != 0)
    {
      trees[spID]->outstanding --;
    }
  if( trees[spID]->outstanding == 0 ){
    if(isSpanningTreeRoot(trees[spID]) != 1){
      byte data[1];
      data[0] = spID;
      sendMySpChunk(trees[spID]->myParent, data, 1, (MsgHandler)&treeBroadcastBackMsg); 
      trees[spID]->broadcasthandler();
    }
    else
      {
	trees[spID]->broadcasthandler();
      }
  }
}

////////////////////////////////////////////////////////////////
// public interface to spanning tree code
////////////////////////////////////////////////////////////////

// for debugging to have some determinism
// 0 -> no debugging, random ids
// 1 -> above + colors for states
// 2 -> above + send log msgs back to host
void setSpanningTreeDebug(int val)
{
}

// allocate a set of num spanning trees.  If num is 0, use system default.
// returns the base of num spanning tree to be used ids.
int initSpanningTrees(int num)
{
  delayMS(100);
  if((maxSpanId + num) < MAX_SPANTREE_ID)
    {
      int i ; 
      for( i = 0 ; i<num ;i++)
	{
	  trees[maxSpanId] = allocateSpanningTree(0);
     	}
      return (maxSpanId-num);
    }
  else
    {
      return -1; //the wanted allocation number exceed the maximun
    }
}

// get a new spanning tree structure for tree with id, id.
// if id == 0 -> get me next legal spanning tree
SpanningTree* allocateSpanningTree(int newId)
{
  if( !newId ){ // if newId = 0 return a spanning tree structure with the newId of maxSpanid
    if( maxSpanId < MAX_SPANTREE_ID ){
      SpanningTree* ret = (SpanningTree*)malloc(sizeof(SpanningTree));
      ret->spantreeid = maxSpanId;		/* the newId of this spanning tree */
      ret->state= WAITING;			/* state i am in forming the spanning tree */
      maxSpanId ++;
    
      return ret;
    }
    else
      {
	return NULL;
      }		
  }
  else
    {
      if( trees[newId] == NULL)
	{
	  SpanningTree* ret = (SpanningTree*)malloc(sizeof(SpanningTree));
	  ret->spantreeid = newId;		
	  ret->state= WAITING;
	  return ret;
	}
      else
	{
	  return NULL;
	}
    }
}

// start a spanning tree with id#, id, where I am the root.  Must be starte by only one node.
// if timeout == 0, never time out
void startTreeByParent(SpanningTree* tree, byte spID, SpanningTreeHandler donefunc, int timeout)
{
  //  niy("startTreeByParent");
}

// start a spanning tree with a random root, more than one node can initiate this.  
// if timeout == 0, never time out
void createSpanningTree(SpanningTree* tree, SpanningTreeHandler donefunc, int timeout)
{
  setColor(WHITE);
  byte spId = tree->spantreeid;
  trees[spId] = tree;
  // set the state to STARTED
  trees[spId]->state = STARTED;
  trees[spId]->status = WAIT;
  //done function for the spanning tree
  trees[spId]->mydonefunc = donefunc;
  //initialize myParent, every blocks thinks they are root at the beginning 
  trees[spId]->myParent = 255;
  //initialize children number
  trees[spId]->numchildren = 0;
  int i; 
  for(  i = 0; i<NUM_PORTS; i++)
    {
      trees[spId]->myChildren[i] = 0;
    }

  // pick a tree->value = rand()<<8|myid (unless debug mode, then it is just id)
  trees[spId]->value = rand()<<8|getGUID();

  // set tree->outstanding = counter to number of neighbors.
  // send to all neighbors: tree->id, tree->value, cstHelper
  trees[spId]->outstanding = 0;
  byte data[3];
  data[0] = spId;
  GUIDIntoChar(trees[spId]->value, &(data[1]));

  // if timeout > 0, set a timeout
  if(timeout > 0)
    {
      trees[spId]->spantimeout.callback = (GenericHandler)(&spCreation);
      trees[spId]->spantimeout.arg = spId;
      trees[spId]->spantimeout.calltime = getTime() + timeout;
      registerTimeout(&(trees[spId]->spantimeout)); 
    }
       
  //send message to add children
  byte p;
  for( p = 0; p < NUM_PORTS;p++){
    if (thisNeighborhood.n[p] != VACANT) {	
      trees[spId]->outstanding++;
      sendMySpChunk(p, data, 3, (MsgHandler)&cstHelper); 
    }
    // all done
  }
}

  // send msg in data to everyone in the spanning tree, call handler when everyone has gotten the msg
  void treeBroadcast(SpanningTree* tree, byte* data, byte size, MsgHandler handler)
  {  
    delayMS(500); //wait for the creation of the spanning in case it is not finished
    byte id = tree->spantreeid;      
    trees[id]->broadcasthandler = handler;
     
    //the root will send the data its children and it will be propagate until the leaves
    if( isSpanningTreeRoot(trees[id]) == 1) 
      {
	byte buf[size + 2];
	buf[0] = id;
	buf[1] = size;
	memcpy(buf+2, data, size*sizeof(byte));
	byte p;
	for( p = 0; p < NUM_PORTS;p++){
	  if (trees[id]->myChildren[p] == 1) {
	    sendMySpChunk(p, buf, size + 2 , (MsgHandler)&treeBroadcastMsg); 
	  }
	}
      }
    trees[id]->outstanding = trees[id]->numchildren;
  }


  // wait til everyone gets to a barrier.  I.e., every node in spanning
  // tree calls this function with the same id.  Will not return until
  // done or timeout secs have elapsed.  If timeout is 0, never timeout.
  // return 1 if timedout, 0 if ok.
  int treeBarrier(SpanningTree* tree, byte id, int timeout)
  {
    setColor(RED);
    reachBarrier = 0;
    byte spID = tree->spantreeid;
    trees[spID] = tree;
    trees[spID]->status = WAIT; 
    if( timeout > 0){                                  //timeout for creating the barrier
      barrierTimeout.callback = (GenericHandler)(&checkBarrier);
      barrierTimeout.arg = spID;
      barrierTimeout.calltime = getTime() + timeout;
      registerTimeout(&barrierTimeout); 
    }
    byte buf[1];
    buf[0] = spID;
    trees[spID]->outstanding = trees[spID]->numchildren;
    //the treeBarrier start with the leaves 
    if( trees[spID]->numchildren == 0)
      {
	byte buf[1];
	buf[0] = spID;
	sendMySpChunk(trees[spID]->myParent, buf, 1, (MsgHandler)&barrierMsg); // send message to myParent to make their status COMPLETED
      }
    allHaveBarrier = 1; 

    while (  trees[spID]->status == WAIT ){ setColor(BROWN);  } //wait for the message from the root which is telling that all the block get into a barrier
    if( trees[spID]->status == COMPLETED )
      {
	if(timeout > 0){
	  deregisterTimeout(&barrierTimeout); 
	}
	if( isSpanningTreeRoot(trees[spID]) == 1){
	  byte p;
	  for(p  = 0; p < NUM_PORTS ; p++){
	    if (trees[spID]->myChildren[p] == 1){
	      sendMySpChunk(p, buf, 1, (MsgHandler)&allHaveBarrierMsg);// send messages to all the tree to tell that all the block get to a barrier
	    }}
	}
	return 1;
      }
    else //return 0 if the status is TIMEDOUT
      {
    
	return 0;
      }
  }
  



  // find out if I am root
  byte isSpanningTreeRoot(SpanningTree* tree)
  {
    byte stId = tree->spantreeid;
    if(trees[stId]->myParent == 255)
      {
	return 1;
      }
    else
      {
	return 0;
      }
  }
