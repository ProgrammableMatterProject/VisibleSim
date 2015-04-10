#include "clock.bbh"
#include "ensemble.bbh"
#include "data_link.bbh"
#include "log.bbh"
#include <math.h>

#ifndef BBSIM
#include "util/atomic.h"
#endif

#define PRINT_BBSIM(...) //printf(__VA_ARGS__)
#define PRINT_BBSIM2(...) //printf(__VA_ARGS__)

threadtype typedef struct _syncData {	Time globalTime;	Time localTime;} syncData_t;

// TEST & DEBUG:
//threaddef #define CENTER_COLOR_DEBUG
//threaddef #define DISACTIVATE_TOPOLOGY_CHANGE_HANDLING
//threaddef #define FAKE_BACK_MESSAGE
//threaddef #define CLOCK_PERF_EVAL
//threaddef #define ELECTION_PERF_EVAL
//threaddef #define TEST_TREE_TRAVERSAL

#ifdef CLOCK_PERF_EVAL
Time lastSync = 0;
#endif

#ifdef ELECTION_PERF_EVAL
Time electionBeginning = 0;
Time backTime = 0;
#endif

// NOT DONE, TODO
// threaddef #define STATIC_ID          1
// Working:
//threaddef #define CENTER_ELECTION       2
threaddef #define MIN_ID_ELECTION       3

// TIME LEADER MANAGEMENT
threadvar byte isLeader = 0;
threadvar byte electing = 0;
threadvar Timeout leaderElectionTimeOut;
threadvar syncData_t maxSystemClock;
threadvar uint16_t minId;
threadvar byte parent;
threadvar uint16_t STlevel = 0;

#ifdef MIN_ID_ELECTION
threadvar byte nbNeededAnswers;
	
static byte sendBackMsg(PRef p, byte a, syncData_t m);
static byte broadcastGoMsg(PRef p, uint16_t id, uint16_t l, syncData_t m);
static byte broadcastClockChunk(PRef excludedPort, byte *d, byte s);
static void initMinIdElectionVariables(void);
static void startMinIdLeaderElection(void);
	
#elif defined CENTER_ELECTION

threadvar byte step;
threadvar byte candidate;
threadvar byte role_election;
threadvar uint16_t gradient;
threadvar uint16_t prev_gradient;
threadvar uint16_t distance;
threadvar uint16_t distFromB;
threadvar uint16_t distFromC;
threadvar uint16_t remainingIds[2];
threadvar byte waiting[NUM_PORTS];
threadvar byte children[NUM_PORTS];
threadvar uint16_t minGradient;
threadvar uint16_t maxDistance;
threadvar Neighborhood myNeighborhood;
	
static void initCenterAlgoVariables(byte re, byte s, uint16_t vg);
static void launchCenterAlgo(void);	
static void broadcastGoMsg(PRef p, byte s, byte r,  uint16_t vg,  uint16_t v, uint16_t id, uint16_t d);
static void sendBackMsg(PRef p, byte r, byte s, uint16_t v, uint16_t id, uint16_t d, uint16_t *rid,  byte cand);
static void sendMeAFakeBackMsg(PRef p, byte r, byte s, uint16_t v, uint16_t id, uint16_t d, uint16_t *rid);
static void handleBackMsg(Chunk *c);
static void sendDeleteMsg(PRef p, byte r, byte s, uint16_t v, uint16_t id, uint16_t d);
static byte NoOneToWaitFor(byte *w);	
static void spreadNextStepMsg(byte s);
static void spreadLeaderElectedMsg(byte *w);	
static void sendCenterElectedBackMsg(PRef p, syncData_t m);
	
threaddef #define ELECTION_RECV_EQUAL 0
threaddef #define ELECTION_RECV_BETTER 1
threaddef #define ELECTION_RECV_WORSE 3
threaddef #define ELECTION_RECV_ERROR 4
static byte cmpCandidate(byte re, uint16_t id, uint16_t v, uint16_t mid, uint16_t mg, uint16_t md);
	
threaddef #define CURRENT_STEP 1
threaddef #define NEXT_STEP 2
threaddef #define STEP_ERROR 3
static byte stepConsistency(byte re, byte s, byte recvre, byte recvs);
	
threaddef #define ROLE_N 0
threaddef #define ROLE_A 1
threaddef #define ROLE_B 2
threaddef #define ROLE_C 3
	
threaddef #define NOT_AN_ID 0
static void addIdIfNotInRemaining(uint16_t id, uint16_t *rid);
static byte numDifferentId(uint16_t *rid);
	
threaddef #define UNDEFINED_PARENT 10
#endif

static void setLeader(void);
static void setSlave(void);
static void scheduleLeaderElection(void);
static syncData_t setMaxSystemClock(Chunk *c, syncData_t m);
static Time getChunkMaxSystemClock(Chunk *c);
static Time getMaxSystemClock(syncData_t m);
static void insertMaxSystemClock(byte *d, syncData_t m);

static Time getSendTime(Chunk *c);
static Time getReceiveTime(Chunk *c);

// CLOCK/SYNCHRONIZATION MANAGEMENT:
threadvar long int offset = 0;
threadvar Time localClockMaxReach = 0;
threadvar Timer syncTimer;
threadvar long unsigned int syncRound = 0;
threadvar long unsigned int numSync = 0;
threadvar PRef syncChildren[NUM_PORTS];

static void initSTChildren(void);
static void freeClockChunk(void);
static void initClockChunkPool(void);
static byte sendClockChunk(PRef p, byte *d, byte s);
static byte synchronizeNeighbor(PRef p);
static byte synchronizeNeighbors(void);

#ifdef SPEED_ESTIMATION
#ifdef REAL_LINEAR_REGRESSION
threaddef #define SYNC_DATA_PERIOD 30000
static void initSyncData(void);
static double computeSpeedAvg(Time gl, Time ll);
#endif
threaddef #define NUM_SYNC_DATA 6
threadvar syncData_t syncData[NUM_SYNC_DATA];
threadvar double speedAvg = 1.0;
#endif

#if 0
static byte requestSync(PRef p);
#endif

#ifdef TEST_TREE_TRAVERSAL
threadvar byte numRequestedAnswers = 0;
threadvar Time testTraversalGo = 0;
static byte spreadTestTraversal(void);
static void backTestTraversal(void);
#endif

void
initClock(void)
{
#ifndef BBSIM
  ATOMIC_BLOCK(ATOMIC_RESTORESTATE)
  {
#endif
    offset = 0;
    localClockMaxReach = 0;
    syncRound = 0;
    numSync = 0;
	
    maxSystemClock.globalTime = 0;
    maxSystemClock.localTime = 0;
	
    initClockChunkPool();
   
#ifdef SPEED_ESTIMATION
    speedAvg = 1.0;
#endif

#ifdef MIN_ID_ELECTION
    parent = 255;
    minId = getGUID();
    STlevel = 0;
    PRINT_BBSIM("%u init \n", getGUID());
#elif defined CENTER_ELECTION
    initSTChildren();
    initCenterAlgoVariables(ROLE_N, 1, 0);
    byte i = 0;
    for (i=0;i<NUM_PORTS;i++){
      children[i] = 0;
    }
#endif
    electing = 0;
    setSlave();
#ifdef CLOCK_PERF_EVAL
    lastSync = 0;
#endif
    scheduleLeaderElection();	
#ifndef BBSIM
  }
#endif
}

Time
getClockForTime(Time t)
{
#ifdef SPEED_ESTIMATION
  return ((double)t*speedAvg) + offset;
#else
  return t + offset;
#endif
}

Time
getEstimatedGlobalClock(void)
{
#ifdef SPEED_ESTIMATION
  return ((double)getTime()*speedAvg) + offset;
#else
  return getTime() + offset;
#endif
}

Time
getClock(void) {
#ifdef CLOCK_SYNC
  return fmax(getEstimatedGlobalClock(), localClockMaxReach);
#else
  return getTime();
#endif
}

byte
isTimeLeader(void)
{
  return isLeader;
}

byte
isElecting(void) {
  return electing;
}

byte
handleClockSyncMessage(void)
{
  if (thisChunk == NULL) 
    {
      return 0;
    }
	
		
#ifndef BBSIM
  ATOMIC_BLOCK(ATOMIC_RESTORESTATE)
  {
#endif
	
    switch(thisChunk->data[1])
      {
      case CLOCK_INFO:
	{
	  Time sendTime = getSendTime(thisChunk);
	  Time receiveTime = getReceiveTime(thisChunk);
	  Time estimatedGlobalTime;

	  //PRINT_BBSIM("block %u: clock info\n", getGUID());			
	  localClockMaxReach = fmax(getClock(), localClockMaxReach);
	  estimatedGlobalTime = sendTime + ESTIMATED_TRANSMISSION_DELAY;
	  
#ifdef CLOCK_PERF_EVAL
#ifdef LOG_DEBUG
	  char s[90];
	  snprintf(s, 90*sizeof(char), "s:%lu,r:%lu,c:%lu,d:%u,l:%lu", estimatedGlobalTime, receiveTime, getClockForTime(receiveTime), STlevel, lastSync);
	  s[89] = '\0';
#endif	  
	  lastSync = receiveTime;
#endif
	  numSync++;
#ifdef SPEED_ESTIMATION
	  if (numSync == 1) {
	    offset = estimatedGlobalTime - receiveTime;
	    speedAvg = 1.0;
	    syncData[0].globalTime = estimatedGlobalTime;
	    syncData[0].localTime = receiveTime;
	  } else {
#ifdef REAL_LINEAR_REGRESSION
	    speedAvg = computeSpeedAvg(estimatedGlobalTime, receiveTime);
#else
	    speedAvg = ((double) (estimatedGlobalTime - syncData[0].globalTime))/ ((double) (receiveTime - syncData[0].localTime));
#endif
	    offset = round(estimatedGlobalTime - (speedAvg*((double)getTime())));
	  }
						
	  //PRINT_BBSIM("block %u: clock info at time %lu (clock %lu), speed average %f, off %d\n", getGUID(), getTime(), getClock(), speedAvg, offset);
#else
	  offset = estimatedGlobalTime - receiveTime;
#endif
	  synchronizeNeighbors();
#ifdef CLOCK_PERF_EVAL
#ifdef LOG_DEBUG
	  if ((rand() % 4) == 0) {
	    printDebug(s);
	  }		
#endif
#endif
	  break;
	}		
      case REQUEST_CLOCK_SYNC :
	{
	  if(isSynchronized())
	    {
	      synchronizeNeighbor(faceNum(thisChunk));
	    }
	  break;
	}
#ifdef MIN_ID_ELECTION
      case MIN_ID_TIME_LEADER_ELECTION_GO_MSG :
	{
	  uint16_t id = charToGUID(&(thisChunk->data[ID_INDEX]));
	  uint16_t l = charToGUID(&(thisChunk->data[LEVEL_INDEX]));

	  maxSystemClock = setMaxSystemClock(thisChunk, maxSystemClock);
	  if (!electing)
	    {
	      deregisterTimeout(&leaderElectionTimeOut);
	      if(getGUID() < id) {		
		startMinIdLeaderElection();
	      } else {
		initMinIdElectionVariables();
	      }
	    }
			
	  if (id == minId)
	    {
	      sendBackMsg(faceNum(thisChunk), 0, maxSystemClock);
	    }
	  if (id < minId)
	    {
	      minId = id;
	      parent = faceNum(thisChunk);
	      initSTChildren();
	      STlevel = l;
	      nbNeededAnswers = broadcastGoMsg(faceNum(thisChunk), id, l, maxSystemClock);
	      if (nbNeededAnswers == 0) 
		{
		  electing = 0;
		  if (minId == getGUID())
		    {
		      setLeader();
		    }
		  else 
		    {
		      sendBackMsg(faceNum(thisChunk), 1, maxSystemClock);
		    }
		}
	    }
	  break;
	}
      case MIN_ID_TIME_LEADER_ELECTION_BACK_MSG :
	{
	  uint16_t id = charToGUID(&(thisChunk->data[ID_INDEX]));
	
	  maxSystemClock = setMaxSystemClock(thisChunk, maxSystemClock);
			
	  if (id == minId)
	    {
	      nbNeededAnswers--;
	      syncChildren[faceNum(thisChunk)] = thisChunk->data[ANSWER_INDEX];
	      if (nbNeededAnswers == 0)
		{
		  electing = 0;
		  if (id == getGUID())
		    {
#ifdef ELECTION_PERF_EVAL  
		      backTime = getTime();
#endif
		      setLeader();
		    }
		  else
		    {
		      sendBackMsg(parent, 1, maxSystemClock);
		    }
		}
	    }
	  break;
	}
#elif defined CENTER_ELECTION
      case CENTER_GO_MSG:
	// STEP_INDEX 2
	// ROLE_INDEX 3
	// VALID_GRADIENT_INDEX 4
	// ID_BEST_INDEX 6
	// VALUE_INDEX 8
	// DISTANCE_INDEX 10
	{
	  byte cmp_res = 0;
	  uint16_t local_sec_value = 0;
	  byte recv_step = thisChunk->data[STEP_INDEX]; 
	  byte recv_role = thisChunk->data[ROLE_INDEX];
	  uint16_t recv_valid_gradient =  charToGUID(&(thisChunk->data[VALID_GRADIENT_INDEX]));
	  uint16_t recv_id = charToGUID(&(thisChunk->data[ID_BEST_INDEX]));
	  uint16_t recv_value = charToGUID(&(thisChunk->data[VALUE_INDEX]));
	  uint16_t recv_distance = charToGUID(&(thisChunk->data[DISTANCE_INDEX]));
	  byte consistency = 0;
		
	  if (!electing)
	    {
	      deregisterTimeout(&leaderElectionTimeOut);
	      launchCenterAlgo();
	      electing = 1;
	    }
		
	  consistency = stepConsistency(role_election, step, recv_role, recv_step);

	  if (consistency == STEP_ERROR) {
	    //initCenterAlgoVariables(ROLE_N, 1, 0);

	    break;
	  } else if (consistency == NEXT_STEP) {
	    initCenterAlgoVariables(recv_role, recv_step, recv_valid_gradient);
	    cmp_res = cmpCandidate(recv_role, recv_id, recv_value, minId, minGradient, maxDistance);
	    if (candidate) {
	      if (cmp_res == ELECTION_RECV_WORSE) { // AVOID TO LAUNCH USELESS MSG
		minId = getGUID();
		parent = UNDEFINED_PARENT;
		if(recv_role == ROLE_A) {
		  minGradient = prev_gradient;
		  //broadcastGoMsg(PRef p, byte s, byte r,  uint16_t vg,  uint16_t v, uint16_t id, uint16_t d)
		  broadcastGoMsg(parent, step, role_election, minGradient, minGradient, minId, 0);
		} else {
		  //broadcastGoMsg(parent, step, role_election, recv_valid_gradient, maxDistance, minId, 0);
		  broadcastGoMsg(parent, step, role_election, minGradient, maxDistance, minId, 0);
		}
	      }
	    }
	  }
		
	  if(role_election == ROLE_A) {
	    candidate = candidate && (prev_gradient <= recv_valid_gradient);
	  }
		
	  cmp_res = cmpCandidate(role_election, recv_id, recv_value, minId, minGradient, maxDistance);

	  if ((cmp_res == ELECTION_RECV_BETTER) || 
	      ((cmp_res == ELECTION_RECV_EQUAL) && (distance > (recv_distance+1)))) {
			
	    if ((cmp_res == ELECTION_RECV_EQUAL) && (parent != UNDEFINED_PARENT) && (distance > (recv_distance+1)) ) {
	      // send delete to parent distance-1
	      if (role_election == ROLE_A) {
		local_sec_value = minGradient;
	      } else {
		local_sec_value = maxDistance;
	      }
	      sendDeleteMsg(parent, role_election, step, local_sec_value, minId, distance-1);
	    }
	    parent = faceNum(thisChunk);
	    minId = recv_id;
	    distance = recv_distance+1;
	    switch(role_election) {
	    case ROLE_A:
	      minGradient = recv_value;
	      break;
	    case ROLE_B:
	      maxDistance = recv_value;
	      distFromB = distance;
	      break;
	    case ROLE_C:
	      maxDistance = recv_value;
	      distFromC = distance;
	      gradient = fmax(distFromB, distFromC);
	      break;
	    }
	    //broadcastGoMsg(PRef p, byte s, byte r,  uint16_t vg,  uint16_t v, uint16_t id, uint16_t d)
	    broadcastGoMsg(parent, step, role_election, recv_valid_gradient, recv_value, recv_id, recv_distance+1);
	    if(NoOneToWaitFor(waiting)) {
	      // send back to parent
	      sendBackMsg(parent, role_election, step, recv_value, recv_id, recv_distance, remainingIds, candidate);
	    }
	  } else {
	    if (cmp_res == ELECTION_RECV_EQUAL)
	      sendDeleteMsg(faceNum(thisChunk), recv_role, recv_step, recv_value, recv_id, recv_distance);
	  }
	}
	break;
      case CENTER_BACK_MSG:
	// REMAINING_IDS_INDEX 14
	{
	  handleBackMsg(thisChunk);
	}
	break;
      case CENTER_DELETE_MSG:
	{
	  byte cmp_res = 0;
	  uint16_t recv_id = charToGUID(&(thisChunk->data[ID_BEST_INDEX]));
	  uint16_t recv_value = charToGUID(&(thisChunk->data[VALUE_INDEX]));
	  uint16_t recv_distance = charToGUID(&(thisChunk->data[DISTANCE_INDEX]));
	  byte recv_step = thisChunk->data[STEP_INDEX]; 
	  byte recv_role = thisChunk->data[ROLE_INDEX];
		
	  if (recv_role == role_election) {
	    cmp_res = cmpCandidate(role_election, recv_id, recv_value, minId, minGradient, maxDistance);
	    if ((cmp_res == ELECTION_RECV_EQUAL) && (recv_distance == distance)) {
	      children[faceNum(thisChunk)] = 0;
	      waiting[faceNum(thisChunk)] = 0;
	      if(NoOneToWaitFor(waiting)) {
		if (parent != UNDEFINED_PARENT) { // SHOULD NOT HAPPEN
		  sendBackMsg(parent,  recv_role, recv_step, recv_value, minId, distance-1, remainingIds, candidate);
		}
	      }
	    }
	  }
	}
	break;
      case CENTER_ELECTED_MSG:
	{
	  memcpy(syncChildren, children, NUM_PORTS*sizeof(byte));
	  initCenterAlgoVariables(ROLE_N, 1, 0); // be ready for other election
	  spreadLeaderElectedMsg(waiting);
	  parent = faceNum(thisChunk);
	  if(NoOneToWaitFor(waiting)) {
	    maxSystemClock.localTime = getTime();
	    maxSystemClock.globalTime = getEstimatedGlobalClock();
	    sendCenterElectedBackMsg(parent, maxSystemClock);
	  }
	  electing = 0;
	}
	break;		
      case CENTER_ELECTED_BACK_MSG:
	{
	  maxSystemClock = setMaxSystemClock(thisChunk, maxSystemClock);
	  waiting[faceNum(thisChunk)] = 0;
	  if(NoOneToWaitFor(waiting)) {
	    if (isLeader) {
	      maxSystemClock = setMaxSystemClock(thisChunk, maxSystemClock);
	      setLeader();
	    } else {
	      sendCenterElectedBackMsg(parent, maxSystemClock);
	    }
	  }
	}
	break;
      case CENTER_NEXT_STEP_MSG:
	{
	  byte recv_step = thisChunk->data[NEXT_STEP_INDEX];
	  if (step != recv_step) {
	    if(candidate) {
	      initCenterAlgoVariables(ROLE_A, recv_step, gradient);
	      minGradient = prev_gradient;
	      broadcastGoMsg(UNDEFINED_PARENT, step, role_election, minGradient, minGradient, getGUID(), 0);
	    } else {
	      spreadNextStepMsg(recv_step);
	    }
	  }
	}
	break;
#endif
#ifdef TEST_TREE_TRAVERSAL
      case TEST_GO_TRAVERSAL:
	{
	numRequestedAnswers = spreadTestTraversal();
	if (numRequestedAnswers == 0) {
	  backTestTraversal();
	}
	setColor(BLUE);
      }
      break;
      case TEST_BACK_TRAVERSAL:
      {
	numRequestedAnswers--;
	if (numRequestedAnswers == 0) {
	  if (!isLeader) {
	    backTestTraversal();
	  }
#ifdef LOG_DEBUG
	 else {
	    char s[50];
	    snprintf(s, 49*sizeof(char), "Traversal done at %lu, duration %lu", getTime(), getTime()- testTraversalGo);
	    s[49] = '\0';
	    printDebug(s);
	  }
#endif
	}
      }
      break;
#endif
      default:
	PRINT_BBSIM("%u unknown clock message received\n", getGUID());
      }
    
#ifndef BBSIM
  }
#endif
  return 1;
}

byte
handleNeighborChange(PRef p)
{
  PRINT_BBSIM2("Neighbor change at Time %u\n", getTime());

#ifdef MIN_ID_ELECTION
#ifndef DISACTIVATE_TOPOLOGY_CHANGE_HANDLING 
  electing = 0;
  if (!electing) {
    scheduleLeaderElection();
  }
#endif
#elif defined CENTER_ELECTION
#ifndef DISACTIVATE_TOPOLOGY_CHANGE_HANDLING 
  if (!electing)
    {
      scheduleLeaderElection();
    }
  else
    {
#endif
      if (thisNeighborhood.n[p] == VACANT) {
	uint16_t value = 0;
	switch(role_election) {
	case ROLE_A:
	  value = minGradient;
	  break;
	case ROLE_B:
	case ROLE_C:
	  value = maxDistance;
	  break;
	}
	#ifdef FAKE_BACK_MESSAGE
	sendMeAFakeBackMsg(p, role_election, step, value, minId, distance, remainingIds);
	children[p] = 0;
	syncChildren[p] = 0;
	myNeighborhood.n[p] = VACANT;
	#else
	setColor(PINK);
	#endif
      }
#ifndef DISACTIVATE_TOPOLOGY_CHANGE_HANDLING 
    }
#endif
#endif
  return 0;
}

byte
isAClockSyncMessage(Chunk *c)
{	
  if ((*((MsgHandler*)c->handler) == RES_SYS_HANDLER) && (c->data[0] == CLOCK_SYNC_MSG)) {
    switch(c->data[1]) {
    case CLOCK_INFO:
    case MIN_ID_TIME_LEADER_ELECTION_BACK_MSG:
    case MIN_ID_TIME_LEADER_ELECTION_GO_MSG:
    case CENTER_ELECTED_BACK_MSG:
      return 1;
      break;
    }
  }
  return 0;
}

/******************************************************
 * Clock (Time) Synchronization Functions
 *****************************************************/

static void
insertTimeStamp(byte *d, Time t, byte i) {
  d[i+3] = (byte) (t & 0xFF);
  d[i+2] = (byte) ((t >>  8) & 0xFF);
  d[i+1] = (byte) ((t >> 16) & 0xFF);
  d[i] = (byte) ((t >> 24) & 0xFF);
}

static Time
getTimeStamp(byte *d, byte i) {
	
  Time t = 0;
			
  t = (Time)(d[i+3]) & 0xFF;
  t |= ((Time)(d[i+2]) << 8) & 0xFF00;
  t |= ((Time)(d[i+1]) << 16) & 0xFF0000;
  t |= ((Time)(d[i]) << 24)  & 0xFF000000;
  return t;
}

void
insertReceiveTime(Chunk *c)
{
  insertTimeStamp(c->data, getTime(), RECEIVE_TIME_INDEX);
}

void
insertSendTime(Chunk *c)
{
  // Global Clock
  insertTimeStamp(c->data, getEstimatedGlobalClock(), SEND_TIME_INDEX);
}

static Time
getSendTime(Chunk *c)
{
  return getTimeStamp(c->data, SEND_TIME_INDEX);
}

static Time
getReceiveTime(Chunk *c)
{
  return getTimeStamp(c->data, RECEIVE_TIME_INDEX);
}

#if 0
byte
requestSync(PRef p)
{
  byte data[2];
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = REQUEST_CLOCK_SYNC;
	
  return sendClockChunk(p, data, 2);
}
#endif

static byte
synchronizeNeighbor(PRef p)
{
  byte data[2];
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = CLOCK_INFO;
  if (thisNeighborhood.n[p] != VACANT)
    return sendClockChunk(p, data, 2);
  else
    return 0;
}

static byte
synchronizeNeighbors(void)
{
  byte p;
  byte n = 0;
	
  if (isTimeLeader()) {
    syncRound++;
    if(syncRound == NUM_CALIBRATION) {
      syncTimer.period = SYNC_PERIOD;
#ifdef CLOCK_PERF_EVALUATION
#ifdef LOG_DEBUG
      char s[10];
      snprintf(s, 10*sizeof(char), "calibOk");
      s[9] = '\0';
      printDebug(s);
#endif
#endif
    }
  }
  for( p = 0; p < NUM_PORTS; p++)
    {
      if ((syncChildren[p] == 0) || (thisNeighborhood.n[p] == VACANT))
	{
	  continue;
	}
      synchronizeNeighbor(p);
      n++;
    }
  return n;
}

byte
isSynchronized(void)
{
  return (isTimeLeader() || (numSync > 0));
}

static void
initSTChildren(void)
{
  byte p;
	
  for (p = 0; p < NUM_PORTS; p++)
    {
      syncChildren[p] = 0;
    }
}

#ifdef REAL_LINEAR_REGRESSION
static void
initSyncData(void)
{
  byte p;
	
  for (p = 0; p < NUM_SYNC_DATA; p++)
    {
      syncData[p].globalTime = 0;
      syncData[p].localTime = 0;
    }
}

static byte
insertSyncData(Time gl, Time ll)
{
  byte i = 0;
  byte iMin = 0;
  // Insert if:
  // There is empty data points
  // The oldest one was inserted about SYNC_DATA_PERIOD ago
  // Sum will not overflow (TODO)
	
  for (i=0; i<NUM_SYNC_DATA; i++) {
    if  (syncData[i].globalTime < syncData[iMin].globalTime) {
      iMin = i;
    }
  }
  if ((syncData[iMin].localTime == 0) || ((gl - syncData[iMin].globalTime + SYNC_PERIOD) > SYNC_DATA_PERIOD)) {
    syncData[iMin].globalTime = gl;
    syncData[iMin].localTime = ll;
    return 1;
  } else {
    return 0;
  }
}

static void
printTable(void)
{
  byte i = 0;
  PRINT_BBSIM("\n");
  for(i=0;i<NUM_SYNC_DATA; i++) {
#ifdef LOG_DEBUG
    char s[100];
    snprintf(s, 100*sizeof(char),"%lu, %lu\n", syncData[i].localTime, syncData[i].globalTime);
    s[99] = '\0';
    printDebug(s);
#endif
    PRINT_BBSIM("%u, %u\n", syncData[i].localTime, syncData[i].globalTime);
  }
  PRINT_BBSIM("\n");
}

static double
computeSpeedAvg(Time gl, Time ll)
{
  // Linear Regression:
  // x: local time
  // y: global time
  byte i = 0;
  double xAvg = 0, yAvg = 0;
  double sum1 = 0, sum2 = 0;
  byte inserted = insertSyncData(gl, ll);
	
  for(i=0;i<NUM_SYNC_DATA; i++) {
    if (syncData[i].localTime == 0) {
      break;
    }
    xAvg += syncData[i].localTime;
    yAvg += syncData[i].globalTime;
  }
  if (inserted == 0) {
    xAvg += ll;
    yAvg += gl;
    i++;
  }
  xAvg = xAvg/i;
  yAvg = yAvg/i;
  for (i=0;i<NUM_SYNC_DATA; i++) {
    if (syncData[i].localTime == 0) {
      break;
    }
    sum1 += (syncData[i].localTime - xAvg) * (syncData[i].globalTime - yAvg);
    sum2 += powf(syncData[i].localTime - xAvg,2);
  }
  if (inserted == 0) {
    sum1 += (ll- xAvg) * (gl - yAvg);
    sum2 += powf(ll - xAvg,2);
  }

  return sum1/sum2;
}
#endif

/******************************************************
 * GENERAL Time Leader Election Functions
 *****************************************************/

static void
setLeader(void) {
  // Just in case, but should be the max value of the system (so the local max as well)
  localClockMaxReach = fmax(getMaxSystemClock(maxSystemClock), getClock());
  offset =  fmax(getMaxSystemClock(maxSystemClock), getClock()) - getTime();
  speedAvg = 1.0;

  isLeader = 1;
  syncRound = 0;
  syncTimer.t.callback = (GenericHandler)&synchronizeNeighbors;
  syncTimer.period = CALIBRATION_PERIOD;
#ifdef ELECTION_PERF_EVAL	
#ifdef LOG_DEBUG
  char s[50];
  snprintf(s, 49*sizeof(char), "SetLeader at %lu, back: %lu, duration %lu", getTime(), backTime - electionBeginning, getTime()- electionBeginning);
  s[49] = '\0';
  printDebug(s);
#endif
#endif

  printf("block %u: Leader\n", getGUID());
  
#ifdef TEST_TREE_TRAVERSAL
  numRequestedAnswers = spreadTestTraversal();
  // if numRequestedAnswers == 0 ... (ignore this case)
#endif

#ifndef TEST_TREE_TRAVERSAL
  synchronizeNeighbors();
  registerTimer(&(syncTimer));
  enableTimer(syncTimer);
#endif
}

static void
setSlave(void) {
  isLeader = 0;
  disableTimer(syncTimer);
  deregisterTimer(&syncTimer);
  deregisterTimer(&syncTimer);
  deregisterTimeout(&(syncTimer.t));
}


static void
scheduleLeaderElection(void)
{
  if (!electing)
    {
      deregisterTimeout(&leaderElectionTimeOut);
      srand(getGUID()+getTime());
      leaderElectionTimeOut.calltime = getTime() + LEADER_ELECTION_TIMEOUT + rand()%50;
#ifdef MIN_ID_ELECTION	
      leaderElectionTimeOut.callback = (GenericHandler)(&startMinIdLeaderElection);
#elif defined CENTER_ELECTION
      leaderElectionTimeOut.callback = (GenericHandler)(&launchCenterAlgo);
#endif
      registerTimeout(&leaderElectionTimeOut);
    }
  else
    {
      //printf("too late!\n");
    }
}


/******************************************************
 * MIN ID Time Leader Election Functions
 *****************************************************/

static Time
getMaxSystemClock(syncData_t m)
{
  int32_t off = getTime() - m.localTime;
  return m.globalTime + off;
}

static Time
getChunkMaxSystemClock(Chunk *c)
{
  return getTimeStamp(c->data, MAX_CLOCK_INDEX);
}

static void
insertMaxSystemClock(byte *d, syncData_t m) {
  insertTimeStamp(d, getMaxSystemClock(m), MAX_CLOCK_INDEX);
}

static syncData_t
setMaxSystemClock(Chunk *c, syncData_t m)
{
  Time receiveTime = getReceiveTime(c);
  Time estimatedMaxSystemClock = getChunkMaxSystemClock(c) + ESTIMATED_TRANSMISSION_DELAY;
		
  // Assume that the clocks are going at the same speed. Realistic on a short interval
  if(estimatedMaxSystemClock > getMaxSystemClock(m)) {
    m.localTime = receiveTime;
    m.globalTime = estimatedMaxSystemClock;
  }
  return m;
}

#ifdef MIN_ID_ELECTION
static byte
sendBackMsg(PRef p, byte a, syncData_t m)
{
  byte data[DATA_SIZE];
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = MIN_ID_TIME_LEADER_ELECTION_BACK_MSG;
  GUIDIntoChar(minId, &(data[ID_INDEX]));
  data[ANSWER_INDEX] = a;
  insertMaxSystemClock(data, m);
  return sendClockChunk(p, data, DATA_SIZE);
}

static byte
broadcastGoMsg(PRef p, uint16_t id, uint16_t l, syncData_t m)
{
  byte data[DATA_SIZE];
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = MIN_ID_TIME_LEADER_ELECTION_GO_MSG;
  GUIDIntoChar(id, &(data[ID_INDEX]));
  GUIDIntoChar(l+1, &(data[LEVEL_INDEX]));
  insertMaxSystemClock(data, m);
  return broadcastClockChunk(p, data, DATA_SIZE);
}

static void
initMinIdElectionVariables(void) {
  if (!electing) {
#ifdef ELECTION_PERF_EVAL
    electionBeginning = getTime();
#endif
    setSlave();
    minId = getGUID();
    parent = 255;
    electing = 1;
    STlevel = 0;
    numSync = 0;
    syncRound = 0;
		
    initSTChildren();
#ifdef REAL_LINEAR_REGRESSION
    initSyncData();
#endif
    
    maxSystemClock.localTime = getTime();
    maxSystemClock.globalTime = getEstimatedGlobalClock();
  }
}

static void
startMinIdLeaderElection(void)
{
  if (!electing) {
    initMinIdElectionVariables();
    nbNeededAnswers = broadcastGoMsg(255, getGUID(),0, maxSystemClock);
    if (nbNeededAnswers == 0) 
      {
	electing = 0;
	if (minId == getGUID())
	  {
	    setLeader();
	  }
	else 
	  {
	    //sendBackMsg(parent, 1);
	  }
      }
  }
}

#elif defined CENTER_ELECTION

/******************************************************
 * CENTER Time Leader Election Functions
 *****************************************************/

static byte
isVacant(PRef p) {
  //return (thisNeighborhood.n[p] == VACANT);
  return (myNeighborhood.n[p] == VACANT);
}

static void
initCenterAlgoVariables(byte re, byte s, uint16_t vg)
{
  byte i = 0;	
  step = s;
  role_election = re;
  if (re != ROLE_N) {
    setSlave();
    parent = UNDEFINED_PARENT; // undefined
  }
  minGradient = UINT16_MAX;
  minId = UINT16_MAX;
  maxDistance = 0;
  switch(role_election) {
  case ROLE_A:
  case ROLE_N:
    if (step == 1) {				
      candidate = 1;
      prev_gradient = 0;
      gradient = 0;
      if (role_election == ROLE_A) {
	electionBeginning = getTime();
	setColor(WHITE);
	initSTChildren();
#ifdef REAL_LINEAR_REGRESSION
	initSyncData();
#endif
	STlevel = 0;
	electing = 1;
	memcpy(&myNeighborhood, &thisNeighborhood, sizeof(Neighborhood));
	maxSystemClock.globalTime = 0;
	maxSystemClock.localTime = 0;
      }
    } else {
      prev_gradient = gradient;
    }
    if (candidate) {
      minId = getGUID();
      minGradient = gradient;
    }
    distFromB = 0;
    distFromC = 0;
    gradient = 0;
    break;
  case ROLE_B:
  case ROLE_C:
    if (candidate) {
      minId = getGUID();
      maxDistance = distance; // dist from A or B
    }
    break;
  }
  STlevel = distance;
  distance = 0;
	
  for (i = 0; i < 2; i++) {
    remainingIds[i] = NOT_AN_ID;
  }
	
  for (i = 0; i < NUM_PORTS; i++) {
    waiting[i] = 0;
    if (re != ROLE_N) {
      children[i] = 0;
    }
  }
}

static void
launchCenterAlgo(void)
{
#ifndef BBSIM
  ATOMIC_BLOCK(ATOMIC_RESTORESTATE)
  {
#endif
	
    if (!electing) {
      PRINT_BBSIM("%u: starts center algo\n", getGUID());
      electing = 1;
      initCenterAlgoVariables(ROLE_A, 1, 0);
      //broadcastGoMsg(PRef p, byte s, byte r,  uint16_t vg,  uint16_t v, uint16_t id, uint16_t d)
      broadcastGoMsg(UNDEFINED_PARENT, 1, ROLE_A, 0, 0, getGUID(), 0);
      if(NoOneToWaitFor(waiting)) {
	setLeader();
      }
    }
#ifndef BBSIM
  }
#endif
	
}

static void
addIdIfNotInRemaining(uint16_t id, uint16_t *rid)
{
  int8_t k = - 1;
  byte isIn = 0;
  byte i = 0;
	
  for (i = 0; i < 2; i++) {
    if (rid[i] == NOT_AN_ID) {
      k = i;
      break;
    }
    if (rid[i] == id) {
      isIn = 1;
      break;
    }
  }
  if ((isIn == 0) && (k != -1)) {
    rid[k] = id;
  }
}

static byte numDifferentId(uint16_t *rid)
{
  byte res = 0;
  byte i = 0;
  for (i = 0; i < 2; i++) {
    if (rid[i] != NOT_AN_ID) {
      res++;
    }
  }
  return res;
}	

static void
broadcastGoMsg(PRef p, byte s, byte r,  uint16_t vg,  uint16_t v, uint16_t id, uint16_t d)
{
  byte data[DATA_SIZE];
  byte i;
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = CENTER_GO_MSG;
  data[STEP_INDEX] = s;
  data[ROLE_INDEX] = r;
  GUIDIntoChar(vg, &(data[VALID_GRADIENT_INDEX]));
  GUIDIntoChar(id, &(data[ID_BEST_INDEX]));
  GUIDIntoChar(v, &(data[VALUE_INDEX]));
  GUIDIntoChar(d, &(data[DISTANCE_INDEX]));

  for( i = 0; i < NUM_PORTS; i++)
    {
      if ((i == p) || isVacant(i))
	{
	  waiting[i] = 0;
	  children[i] = 0;
	  continue;
	}
      if(sendClockChunk(i, data, DATA_SIZE)) {
	waiting[i] = 1;
	children[i] = 1;
      } else {
	waiting[i] = 0;
	children[i] = 0;
      }
    }
}

static void	
handleBackMsg(Chunk *c) {
  byte cmp_res = 0;
  byte i = 0;
  uint16_t recv_id = charToGUID(&(c->data[ID_BEST_INDEX]));
  uint16_t recv_value = charToGUID(&(c->data[VALUE_INDEX]));
  uint16_t recv_distance = charToGUID(&(c->data[DISTANCE_INDEX]));
  uint16_t *recv_remaining_ids = (uint16_t*) &(c->data[REMAINING_IDS_INDEX]);
  byte recv_step = c->data[STEP_INDEX]; 
  byte recv_role = c->data[ROLE_INDEX];
		
  if (!electing) {
    return;
  }
		
  if (recv_role == role_election) {
    cmp_res = cmpCandidate(recv_role, recv_id, recv_value, minId, minGradient, maxDistance);
    if ((cmp_res == ELECTION_RECV_EQUAL) && (recv_distance == distance)) {
      waiting[faceNum(thisChunk)] = 0;
      for (i = 0; i < 2; i++) {
	addIdIfNotInRemaining(recv_remaining_ids[i], remainingIds);
      }
      if (NoOneToWaitFor(waiting)) {
	if (recv_id == getGUID()) {
	  // All the nodes have their final distance to me, I can launch the next step
	  switch(role_election) {
	  case ROLE_A:
	    {
	      PRINT_BBSIM("%u A elected\n", getGUID());
	      /*#ifdef LOG_DEBUG
	      char str[50];
	      snprintf(str, 50*sizeof(char), "A:%u", getGUID());
	      str[49] = '\0';
	      printDebug(str);
	      #endif*/
							
#ifdef CENTER_COLOR_DEBUG
	      setColor(BROWN);
#endif
								
	      // returns either 1, 2 (storage inside a message is limited!)
	      if ((numDifferentId(remainingIds)+1) > 2) {
		// Launch B election
		initCenterAlgoVariables(ROLE_B, step, minGradient);											
		broadcastGoMsg(UNDEFINED_PARENT, step, role_election, 0, 0, minId, distance);
	      } else {
		memcpy(syncChildren, children, NUM_PORTS*sizeof(byte));
		maxSystemClock.localTime = getTime();
		maxSystemClock.globalTime = getEstimatedGlobalClock();
#ifdef ELECTION_PERF_EVAL  
		backTime = getTime();
#endif
		spreadLeaderElectedMsg(waiting);
		electing = 0;
		isLeader = 1;
		if (NoOneToWaitFor(waiting)) {
		  initCenterAlgoVariables(ROLE_N, 1, 0);
		  setLeader(); // Start the timer for synchronization messages
		}
	      }
	    }
	    break;
	  case ROLE_B:
	    {
	      /*
#ifdef LOG_DEBUG
	      char str[50];
	      snprintf(str, 50*sizeof(char), "B:%u", getGUID());
	      str[49] = '\0';
	      printDebug(str);
	      #endif*/
								
#ifdef CENTER_COLOR_DEBUG
	      setColor(BLUE);
#endif
								
	      initCenterAlgoVariables(ROLE_C, step, 0);
	      candidate = 0; // B is eliminated, (not a problem: because C != B)
	      broadcastGoMsg(UNDEFINED_PARENT, step, role_election, 0, 0, minId, distance);
	    }
	    break;
	  case ROLE_C:
	    {
	      /*							
#ifdef LOG_DEBUG
	      char str[50];
	      snprintf(str, 50*sizeof(char), "C:%u", getGUID());
	      str[49] = '\0';
	      printDebug(str);
	      #endif*/
								
#ifdef CENTER_COLOR_DEBUG
	      setColor(GREEN);
#endif
								
	      gradient = distFromB;
	      candidate = 0; // C is eliminated
	      spreadNextStepMsg(step+1);
	    }
	    break;
	  }
	} else {
	  if (parent != UNDEFINED_PARENT) {
	    sendBackMsg(parent, recv_role, recv_step, recv_value, minId, distance-1, remainingIds, candidate);
	  }
	}
      }
    }
  }
}

threadextern Port port[NUM_PORTS];

static void
sendMeAFakeBackMsg(PRef p, byte r, byte s, uint16_t v, uint16_t id, uint16_t d, uint16_t *rid)
{
  Chunk c;
  byte * data = c.data;
  byte i = 0;
	
  for(i = 0; i < DATA_SIZE; i++) {
    data[i] = 0;
  }
  // set the flags
  c.status = CHUNK_USED | CHUNK_FILLED | MSG_RESP_SENDING | port[p].pnum;
    
  // clear out next pointer
  c.next = NULL;
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = CENTER_BACK_MSG;
  data[ROLE_INDEX] = r;
  data[STEP_INDEX] = s;
  GUIDIntoChar(id, &(data[ID_BEST_INDEX]));
  GUIDIntoChar(v, &(data[VALUE_INDEX]));
  GUIDIntoChar(d, &(data[DISTANCE_INDEX]));
  memcpy(&data[REMAINING_IDS_INDEX], rid, 2*sizeof(uint16_t));
  handleBackMsg(&c);
}

static void
sendBackMsg(PRef p, byte r, byte s, uint16_t v, uint16_t id, uint16_t d, uint16_t *rid, byte cand)
{
  byte data[DATA_SIZE];
  byte i = 0;
	
  for(i = 0; i < DATA_SIZE; i++) {
    data[i] = 0;
  }

  data[0] = CLOCK_SYNC_MSG;
  data[1] = CENTER_BACK_MSG;
  data[ROLE_INDEX] = r;
  data[STEP_INDEX] = s;
  GUIDIntoChar(id, &(data[ID_BEST_INDEX]));
  GUIDIntoChar(v, &(data[VALUE_INDEX]));
  GUIDIntoChar(d, &(data[DISTANCE_INDEX]));
  if (cand) {
    addIdIfNotInRemaining(getGUID(), rid);
  }	
  memcpy(&data[REMAINING_IDS_INDEX], rid, 2*sizeof(uint16_t));
  sendClockChunk(p, data, DATA_SIZE);
}

static void
sendDeleteMsg(PRef p, byte r, byte s, uint16_t v, uint16_t id, uint16_t d)
{
  byte data[DATA_SIZE];
  byte i = 0;
	
  for(i = 0; i < DATA_SIZE; i++) {
    data[i] = 0;
  }
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = CENTER_DELETE_MSG;
  data[ROLE_INDEX] = r;
  data[STEP_INDEX] = s;
  GUIDIntoChar(id, &(data[ID_BEST_INDEX]));
  GUIDIntoChar(v, &(data[VALUE_INDEX]));
  GUIDIntoChar(d, &(data[DISTANCE_INDEX]));
	
  sendClockChunk(p, data, DATA_SIZE);
}

static byte
NoOneToWaitFor(byte *w)
{
  byte i = 0;
  for (i=0; i<NUM_PORTS; i++) {
    if (w[i]) {
      return 0;
    }
  }
  return 1;
}	

static void
spreadNextStepMsg(byte s)
{
  byte p = 0;
  byte data[DATA_SIZE];
  byte i = 0;
	
  for(i = 0; i < DATA_SIZE; i++) {
    data[i] = 0;
  }
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = CENTER_NEXT_STEP_MSG;
  data[NEXT_STEP_INDEX] = s;
	
  for( p = 0; p < NUM_PORTS; p++)
    {
      if ((children[p] == 0) || isVacant(p))
	{
	  continue;
	}
      sendClockChunk(p, data, DATA_SIZE);
    }
}

static void
spreadLeaderElectedMsg(byte *w)
{
  byte p;
  byte data[DATA_SIZE];
	
  byte i = 0;
	
  for(i = 0; i < DATA_SIZE; i++) {
    data[i] = 0;
  }
	
  data[0] = CLOCK_SYNC_MSG;
  data[1] = CENTER_ELECTED_MSG;
		
  for( p = 0; p < NUM_PORTS; p++)
    {
      if ((children[p] == 0) || isVacant(p))
	{
	  w[p] = 0;
	  continue;
	}
      if(sendClockChunk(p, data, DATA_SIZE)) {
	w[p] = 1;
      } else {
	w[p] = 0;
      }
		
    }
}

static void
sendCenterElectedBackMsg(PRef p, syncData_t m) {
  byte data[DATA_SIZE];
  byte i = 0;
	
  for(i = 0; i < DATA_SIZE; i++) {
    data[i] = 0;
  }

  data[0] = CLOCK_SYNC_MSG;
  data[1] = CENTER_ELECTED_BACK_MSG;
	
  insertMaxSystemClock(data, m);
	
  sendClockChunk(p, data, DATA_SIZE);
}

static byte
cmpCandidate(byte re, uint16_t id, uint16_t v, uint16_t mid, uint16_t mg, uint16_t md)
{
  uint16_t local_sec_value;
			
  switch(re) {
  case ROLE_A:
    // Minimize the gradient
    local_sec_value = mg;
    if (v < local_sec_value) {
      return ELECTION_RECV_BETTER;
    } else if ((v == local_sec_value) && (id < mid)) {
      return ELECTION_RECV_BETTER;
    } else if ((v == local_sec_value) && (id == mid)) {
      return ELECTION_RECV_EQUAL;
    } else {
      return ELECTION_RECV_WORSE;
    }
    break;
  case ROLE_B:
  case ROLE_C:
    // Maximize the distance
    local_sec_value = md;
    if (v > local_sec_value) {
      return ELECTION_RECV_BETTER;
    } else if ((v == local_sec_value) && (id < mid)) {
      return ELECTION_RECV_BETTER;
    } else if ((v == local_sec_value) && (id == mid)) {
      return ELECTION_RECV_EQUAL;
    } else {
      return ELECTION_RECV_WORSE;
    }
    break;
  default:
    return ELECTION_RECV_ERROR;
  }
}

static byte
stepConsistency(byte re, byte s, byte recvre, byte recvs)
{
  byte res = STEP_ERROR;
	
  if ((re == ROLE_N) && (recvre == ROLE_A)) {
    res = NEXT_STEP;
  } else {
    if (s == recvs) {
      if (re == recvre) {
	res = CURRENT_STEP;
      } else if ((re+1) == recvre) {
	res = NEXT_STEP;
      } 
    } else if ((s+1) == recvs) {
      if ((re == ROLE_C) && (recvre == ROLE_A)) {
	res = NEXT_STEP;
      }
    }
  }
  return res;
}

#endif

/******************************************************
 * TEST_TREE_TRAVERSAL
 ******************************************************/
#ifdef TEST_TREE_TRAVERSAL
static byte
spreadTestTraversal(void)
{
  byte p = 0;
  byte data[2];
  byte n = 0;
  
  testTraversalGo = getTime();
  
  data[0] = CLOCK_SYNC_MSG;
  data[1] = TEST_GO_TRAVERSAL;

  for( p = 0; p < NUM_PORTS; p++)
    {
      if ((syncChildren[p] == 0) || (thisNeighborhood.n[p] == VACANT))
	{
	  continue;
	}
      sendClockChunk(p, data, 2);
      n++;
    }
  return n;
}

static void
backTestTraversal(void)
{
  byte data[2];
  PRef p = parent;

  data[0] = CLOCK_SYNC_MSG;
  data[1] = TEST_BACK_TRAVERSAL;
  sendClockChunk(p, data, 2);
}

#endif
/******************************************************
 * Chunk Management Functions
 *****************************************************/
threaddef #define NUM_CLOCK_CHUNK 24

threadvar Chunk clockChunkPool[NUM_CLOCK_CHUNK];

static void
initClockChunkPool(void) {
  byte i = 0;
  for (i = 0; i < NUM_CLOCK_CHUNK; i++) {
    clockChunkPool[i].status = CHUNK_FREE;
    clockChunkPool[i].next = NULL;
  }
}

static void
freeClockChunk(void) {
  if(chunkResponseType(thisChunk) != MSG_RESP_ACK){
    //setColor(PINK);
  }
  freeChunk(thisChunk);
  thisChunk->status = CHUNK_FREE;
}

static Chunk*
getClockChunk(void) {
  byte i = 0;
  Chunk *c = NULL;
  for(i = 0; i < NUM_CLOCK_CHUNK ; i++) {
    c = &clockChunkPool[i];
    if(!chunkInUse(c)) {
            // indicate in use
      c->status = CHUNK_USED;
      c->next = NULL;
      return c;
    }
  }
    return NULL;
}

static byte
sendClockChunk(PRef p, byte *d, byte s)
{
  //Chunk *c=getSystemTXChunk();
  Chunk *c =getClockChunk();
  
  if (c == NULL) {
    return 0;
  }
  if (sendMessageToPort(c, p, d, s, (MsgHandler)RES_SYS_HANDLER, (GenericHandler)&freeClockChunk) == 0) {
    freeChunk(c);
    return 0;
  }
  
  return 1;
}

#ifdef MIN_ID_ELECTION

static byte
broadcastClockChunk(PRef excludedPort, byte *d, byte s)
{
  byte p;
  byte sent = 0;
  
  for( p = 0; p < NUM_PORTS; p++)
    {
      if ((p == excludedPort) || (thisNeighborhood.n[p] == VACANT))
	{
	  continue;
	}
      if(sendClockChunk(p, d, s)) {
	sent++;
      }
    }
  return sent;
}
#endif
