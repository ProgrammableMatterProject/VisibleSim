#include <iostream>
#include <boost/asio.hpp>
#include <boost/asio/ip/address.hpp>
#include <string>

#include "db/database.hpp"
#include "process/remote.hpp"
#include "sched/common.hpp"
#include "process/machine.hpp"
#include "utils/utils.hpp"
#include "api/api.hpp"
#include "sched/serial.hpp"
#include "msg/msg.hpp"
#include "debug/debug_handler.hpp"
#include "vm/determinism.hpp"
#include "compileInfo.hpp"

using namespace db;
using namespace vm;
using namespace vm::determinism;
using namespace utils;
using namespace process;
using namespace debugger;
using boost::asio::ip::tcp;
using sched::serial_node;
using sched::base;
using namespace sched;
using namespace msg;

#define SETID 1
#define DEBUG 16
#define STOP 4
#define ADD_NEIGHBOR 5
#define REMOVE_NEIGHBOR 6
#define TAP 7
#define SET_COLOR 8
#define SEND_MESSAGE 12
#define RECEIVE_MESSAGE 13
#define ACCEL 14
#define SHAKE 15

#define SET_DETERMINISTIC_MODE		20
#define POLL_START					21
#define END_POLL					22
#define	WORK_END					23
#define TIME_INFO					24

// debug messages for simulation
// #define DEBUG

declareCompileInfo(targetArchitecture, "bbsim");

namespace api
{

  const char* apiTarget = "bbsim";

  enum face_t {
    INVALID_FACE = -1,
    BOTTOM = 0,
    NORTH = 1,
    EAST = 2,
    WEST = 3,
    SOUTH = 4,
    TOP = 5
  };

  /*Storing the block's neighbor information*/

  vm::node_val top;
  vm::node_val bottom;
  vm::node_val east;
  vm::node_val west;
  vm::node_val north;
  vm::node_val south;

  bool instantiated_flag(false);
  size_t neighbor_count;
  inline face_t& operator++(face_t &f)
  {
    f = static_cast<face_t>(f + 1);
    return f;
  }

  inline face_t operator++(face_t& f, int) {
    ++f;
    return f;
  }




  /*Making compatible with simulator*/
  static const vm::node_val NO_NEIGHBOR = (vm::node_val)-1;

  static const face_t INITIAL_FACE = BOTTOM;
  static const face_t FINAL_FACE = TOP;
  // returns a pointer to a certain face, allowing modification

  static vm::node_val 
  *get_node_at_face(const face_t face)
  {
    switch(face) {
    case BOTTOM: return &bottom;
    case NORTH: return &north;
    case EAST: return &east;
    case WEST: return &west;
    case SOUTH: return &south;
    case TOP: return &top;
    default: assert(false);
    }
  }

#if 0
  // THIS IS NOT USED?  SHOULD IT BE?

  /*Get the block at a particular face*/
  static face_t 
  get_face(const vm::node_val node) 
  {
    if(node == bottom) return BOTTOM;
    if(node == north) return NORTH;
    if(node == east) return EAST;
    if(node == west) return WEST;
    if(node == south) return SOUTH;
    if(node == top) return TOP;
    return INVALID_FACE;
  }
#endif

  static inline bool 
  has_been_instantiated(void)
  {
    return instantiated_flag;
  }

  static inline void 
  inc_neighbor_count(void)
  {
    ++neighbor_count;
  }

  static inline void 
  dec_neighbor_count(void)
  {
    --neighbor_count;
  }

  static inline size_t 
  get_neighbor_count(void)
  {
    return neighbor_count;
  }

  /*Helper Functions*/
  static const char* msgcmd2str[27];
  static boost::asio::ip::tcp::socket *my_tcp_socket;
  static void processMessage(message_type* reply);
  static void addReceivedTuple(serial_node *no, size_t ts, db::simple_tuple *stpl);
  static void addNeighbor(const size_t ts, serial_node *no, const node_val out, const face_t face, const int count);
  static void addNeighborCount(const size_t ts, serial_node *no, const size_t total, const int count);
  static void addVacant(const size_t ts,  serial_node *no, const face_t face, const int count);
  static void handleSetID(deterministic_timestamp ts, node::node_id node_id);
  static void handleReceiveMessage(const deterministic_timestamp ts, db::node::node_id node,
                                   const face_t face, db::node::node_id dest_id, utils::byte *data, int offset, const int limit);
  static void handleAddNeighbor(const deterministic_timestamp ts, const db::node::node_id in,
                                const db::node::node_id out, const face_t face);
  static void handleRemoveNeighbor(const deterministic_timestamp ts,
                                   const db::node::node_id in, const face_t face);
  static void handleTap(const deterministic_timestamp ts, const db::node::node_id node);
  static void handleAccel(const deterministic_timestamp ts, const db::node::node_id node,
                          const int_val f);
  static void handleShake(const deterministic_timestamp ts, const db::node::node_id node,
                          const int_val x, const int_val y, const int_val z);
  static void   check_pre(sched::base *schedular);
  static bool isReady();
  //static message_type *tcpPool();
  static void initTCP();
  static void handleDebugMessage(utils::byte* reply, size_t totalSize);
  static void sendMessageTCP(message *m);

  static void handleSetDeterministicMode(const deterministic_timestamp ts,
                                         const db::node::node_id node);

  static bool ready(false);
  /* deterministic mode */
  static bool polling(false);
  static std::queue<message_type*> messageQ;
  static uint nbProcessedMsg = 0;
  
  /*Stores the scheduler*/
  static sched::base *sched_state(NULL);
  vm::predicate* neighbor_pred(NULL);
  vm::predicate* tap_pred(NULL);
  vm::predicate* neighbor_count_pred(NULL);
  vm::predicate* accel_pred(NULL);
  vm::predicate* shake_pred(NULL);
  vm::predicate* vacant_pred(NULL);
  bool stop_all(false);
  static db::node::node_id id(0); 

  using namespace std;

  /*Returns the nodeID*/
  int
  getNodeID(void){
    return id;
  }

  /*To initialize the connection to the simulator */
  void 
  init(int argc, char **argv, sched::base* schedular)
  { 
    if (schedular == NULL) return;
    ignoreUnusedParamWarning(argc);
    ignoreUnusedParamWarning(argv);

    for (int i=0; i<25; i++) 
      msgcmd2str[i] = NULL;

    msgcmd2str[SETID] = "SETID";
    msgcmd2str[STOP] = "STOP";
    msgcmd2str[ADD_NEIGHBOR] = "ADD_NEIGHBOR";
    msgcmd2str[REMOVE_NEIGHBOR] = "REMOVE_NEIGHBOR";
    msgcmd2str[TAP] = "TAP";
    msgcmd2str[SET_COLOR] = "SET_COLOR";
    msgcmd2str[SEND_MESSAGE] = "SEND_MESSAGE";
    msgcmd2str[RECEIVE_MESSAGE] = "RECEIVE_MESSAGE";
    msgcmd2str[ACCEL] = "ACCEL";
    msgcmd2str[SHAKE] = "SHAKE";
    msgcmd2str[DEBUG] = "DEBUG";
    msgcmd2str[SET_DETERMINISTIC_MODE] = "SET_DETERMINISTIC_MODE";
    msgcmd2str[WORK_END] = "WORK_END";
    msgcmd2str[END_POLL] = "END_POLL";
    msgcmd2str[POLL_START] = "POLL_START";
    try{
      /* Calling the connect*/
      initTCP();
    } catch(std::exception &e) {
      throw machine_error("can't connect to simulator");
    }
    check_pre(schedular);
    while(!isReady() && waitAndProcess());
  }

  void debugInit(void)
  {
    /*Initilize the debugger*/
    return;
  }


  /*Checks the predicates to be used during execution*/
  void 
  check_pre(sched::base *schedular){

    sched_state=schedular;

    // cout<<"Setting the predicates"<<endl;  
    neighbor_pred = vm::All->PROGRAM->get_predicate_by_name("neighbor");
    if(neighbor_pred) {
      assert(neighbor_pred->num_fields() == 2);
    } else {
      //   cerr << "No neighbor predicate found" << endl;
    }

    tap_pred = vm::All->PROGRAM->get_predicate_by_name("tap");
    if(tap_pred) {
      assert(tap_pred->num_fields() == 0);
    } else {
      //   cerr << "No tap predicate found" << endl;
    }

    neighbor_count_pred = vm::All->PROGRAM->get_predicate_by_name("neighborCount");
    if(neighbor_count_pred) {
      assert(neighbor_count_pred->num_fields() == 1);
    } else {
      //   cerr << "No neighbor_count predicate found" << endl;
    }

    accel_pred = vm::All->PROGRAM->get_predicate_by_name("accel");
    if(accel_pred) {
      assert(accel_pred->num_fields() == 1);
    } else {
      //   cerr << "No accel predicate found" << endl;
    }

    shake_pred = vm::All->PROGRAM->get_predicate_by_name("shake");
    if(shake_pred) {
      assert(shake_pred->num_fields() == 3);
    } else {
      //   cerr << "No shake predicate found" << endl;
    }

    vacant_pred = vm::All->PROGRAM->get_predicate_by_name("vacant");
    if(vacant_pred) {
      assert(vacant_pred->num_fields() == 1);
    } else {
      //   cerr << "No vacant predicate found" << endl;
    }
  }

  /*Used in MPI, For BBSIM, used in the machine::route method*/
  bool 
  onLocalVM(const db::node::node_id id){
    ignoreUnusedParamWarning(id);
    return false;
  }

  /*API function to send the SETCOLOR command to the simulator*/
  void 
  set_color(db::node *n, const int r, const int g, const int b)
  {
    message* colorMessage=(message*)calloc(8, sizeof(message_type));

    cout<<n->get_id() << ":Sending SetColor"<<endl;

    colorMessage->size=7 * sizeof(message_type);
    colorMessage->command=SET_COLOR;
    colorMessage->timestamp=getCurrentLocalTime();
    colorMessage->node=(message_type)n->get_id();
    colorMessage->data.color.r=r;
    colorMessage->data.color.g=g;
    colorMessage->data.color.b=b;
    colorMessage->data.color.i=0;

    sendMessageTCP(colorMessage);
    free(colorMessage);
  }
  
  void workEnd() {
    message* workEndMessage = (message*)calloc(5, sizeof(message_type));
    workEndMessage->size = 4 * sizeof(message_type);
    workEndMessage->command = WORK_END;
    workEndMessage->timestamp = (message_type) getCurrentLocalTime();
	workEndMessage->node = 0; //(message_type)n->get_id();
	workEndMessage->data.workEnd.nbRecMsg = nbProcessedMsg;
    sendMessageTCP(workEndMessage);
    free(workEndMessage);
  }
  
  void pollStart() {
	polling = true;
	message* pollStartMessage = (message*)calloc(4, sizeof(message_type));
    pollStartMessage->size = 3 * sizeof(message_type);
    pollStartMessage->command =  POLL_START;
    pollStartMessage->timestamp = (message_type) getCurrentLocalTime();
	pollStartMessage->node = 0; //(message_type)n->get_id();
    sendMessageTCP(pollStartMessage);
    free(pollStartMessage);
  }
  
  void timeInfo(db::node *n) {
    ignoreUnusedParamWarning(n);
    message* timeInfoMessage=(message*)calloc(4, sizeof(message_type));
    timeInfoMessage->size=3 * sizeof(message_type);
    timeInfoMessage->command = TIME_INFO;
    timeInfoMessage->timestamp = (message_type) getCurrentLocalTime();
    timeInfoMessage->node = 0; //(message_type)n->get_id();
    sendMessageTCP(timeInfoMessage);
    free(timeInfoMessage);
  }
  
  void handleSetDeterministicMode(const deterministic_timestamp ts,
                                  const db::node::node_id node) {
    ignoreUnusedParamWarning(node);
    ignoreUnusedParamWarning(ts);
    setSimulationDeterministicMode();
  }

  static void processNextQueuedMessage() {
	message_type *m = NULL;
	m = messageQ.front();
	processMessage(m);
	messageQ.pop();
	delete[] m;
  }

  bool readAMessage(message_type *msg) {
	try {
      boost::asio::read(*my_tcp_socket, boost::asio::buffer(msg, sizeof(message_type)));
      boost::asio::read(*my_tcp_socket, boost::asio::buffer(msg + 1,  msg[0]));
	} catch(std::exception &e) {
      cout<<"Could not recieve !"<<endl;
      stop_all = true;
      return false;
	}
	return true;
  }

  /* Wait for at least one incoming message. // Read and process all
   * the received messages.
   */
  bool waitAndProcess() {
    static message_type msg[api::MAXLENGTH];
	if (debugger::isInSimDebuggingMode() && !messageQ.empty()) {
      processNextQueuedMessage();
	} else {
      if (readAMessage(msg)) {
        processMessage(msg);
      } else {
        return false;
      }
	}
    if(ensembleFinished()) {
      return false;
    } else {
      return true;
	}
  }

  bool pollAndProcess(sched::base *sched) {
    ignoreUnusedParamWarning(sched);
	static message_type msg[api::MAXLENGTH];
	switch (vm::determinism::getSimulationMode()) {
    case REALTIME :
      while (my_tcp_socket->available()) {
        if (readAMessage(msg)) {
          processMessage(msg);
        } else {
          return false;
        }
      }
      break;
    case DETERMINISTIC :
      pollStart();
      while (polling) {
        if (debugger::isInSimDebuggingMode()) {
          while (polling && !messageQ.empty()) {
            processNextQueuedMessage();
          }
        }
        if (polling && (!debugger::isInSimDebuggingMode() || debugger::isDebuggerQueueEmpty())) {
          if (readAMessage(msg)) {
            processMessage(msg);
          } else {
            return false;
          }
        }
        if (debugger::isInSimDebuggingMode()){
          debugger::receiveMsg(false);
          if (debugger::isTheSystemPaused()){
            debugger::isPausedInDeterministicPollLoop = true;
            debugger::display("PAUSED IN DETERMINISTIC POLL LOOP\n",debugger::PAUSE);
            debugger::pauseIt();
          }
        }
      }
      break;
    default:
      break;
	}
    if(ensembleFinished()) {
      return false;
    } else {
      return true;
	}
  }

  /*Returns the node id for bbsimAPI*/
  int 
  getVMId(const db::node::node_id id)
  {
    return id;
  }

  /*Used in MPI*/
  void 
  serializeBeginExec(void)
  {
    return;
  }
 
  /*Used in MPI*/ 
  void serializeEndExec(void)
  {

  }

  /*Sends the "SEND_MESSAGE" command*/
  void
  sendMessage(const db::node* from, db::node::node_id to, db::simple_tuple* stpl)
  {
    //Find the tuple size
    const size_t stpl_size(stpl->storage_size());
    //  cout<<getNodeID<<":Stpl:"<<*stpl<<":Stpl size:"<<stpl_size<<endl;

    //Compute the size field  
    const size_t msg_size = 5 * sizeof(message_type) + stpl_size;

    //Allocating the buffer for the message   msg_size + the space for msga->size field.
    message* msga=(message*)calloc((msg_size+ sizeof(message_type)), 1);
 
 
    msga->size = (message_type)msg_size;
    msga->command = SEND_MESSAGE;
    msga->timestamp = getCurrentLocalTime();
    msga->node = from->get_id();
    msga->data.send_message.face= 0; //(dynamic_cast<serial_node*>(from))->get_face(to);
    msga->data.send_message.dest_nodeID = to;
    cout << from->get_id() << " Send " << *stpl << "to "<< to<< endl;

    /*Setting the position at the end of header to copy the tuple*/ 
    int pos = 6 * sizeof(message_type);
    stpl->pack((utils::byte*)msga, msg_size + sizeof(message_type), &pos);
    // cout<<"Message Size:"<< msg_size<<" Pos:"<<pos<<" Assertion:"<<msg_size+sizeof(message_type)<<endl;
    assert((size_t)pos == msg_size + sizeof(message_type));

    simple_tuple::wipeout(stpl);
    sendMessageTCP(msga);
    free(msga);
  }


  /*Flags if VM can run now*/
  bool 
  isReady()
  {
    return ready;
  }

  void 
  end(void)
  {
    return;
  }


  /*tcp helper functions begin*/
  static void 
  initTCP()
  {
    try {
      boost::asio::io_service io_service;
      tcp::resolver resolver(io_service);
	  /*Change the arguments from hard-coded to variables*/
      tcp::endpoint e(boost::asio::ip::address::from_string("127.0.0.1"), 5000);
      my_tcp_socket = new tcp::socket(io_service);
      my_tcp_socket->connect(e);
    } catch(std::exception &e) {
      cout<<"Could not connect!"<<endl;
    }
  }

#if 0
  // THIS IS NOT USED ANYWHERE?????

  static message_type *
  tcpPool()
  {
    static message_type msg[api::MAXLENGTH];
    try {
      if(my_tcp_socket->available())
        {
          // shouldn't there be some checking here?
          size_t length = my_tcp_socket->read_some(boost::asio::buffer(msg, sizeof(message_type)));
          length = my_tcp_socket->read_some(boost::asio::buffer(msg + 1,  msg[0]));
          return msg;   
        }
    } catch(std::exception &e) {
      cout<<"Could not recieve! 3"<<endl;
      return NULL;
    }
    return NULL;
  }
#endif

  /*Sends the message over the socket*/
  static void 
  sendMessageTCP(message *msg)
  {
    boost::asio::write(*my_tcp_socket, boost::asio::buffer(msg, msg->size + sizeof(message_type)));
  }


  /*TCP helper functions end*/


  /*Helper function Definitions*/

  static db::node::node_id nodeId = 0;   // the id of this node from setid

  /*Handles the incoming commangs from the simulator*/
  static void 
  processMessage(message_type* reply)
  {
    //printf("%d:Processing %s %lud bytes for %lud\n",id, msgcmd2str[reply[1]], reply[0], reply[3]);
    assert(reply!=NULL);
    message* msg=(message*)reply;
    cout << id << " :Processing " << msgcmd2str[msg->command] << endl;
    assert ((msg->node == 0)||(nodeId == 0)||((db::node::node_id)msg->node == nodeId));
    
	if (msg->command != DEBUG) {
      setCurrentLocalTime((deterministic_timestamp)msg->timestamp);
      nbProcessedMsg++;
	}
	
    switch(msg->command) {
      /*Initilize the blocks's ID*/
    case SETID: 
      handleSetID((deterministic_timestamp) msg->timestamp, (db::node::node_id) msg->node);
      id=(db::node::node_id) msg->node;
      nodeId = id;
      ready=true;
      break;

    case RECEIVE_MESSAGE:
      handleReceiveMessage((deterministic_timestamp)msg->timestamp,
                           (db::node::node_id)msg->node,
                           (face_t)msg->data.receiveMessage.face,
                           (db::node::node_id)msg->data.receiveMessage.from,
                           (utils::byte*)reply,
                           6 * sizeof(message_type),
                           (int)(msg->size + sizeof(message_type)));
      break;

    case ADD_NEIGHBOR:
      if(id==(db::node::node_id) msg->node)
        handleAddNeighbor((deterministic_timestamp)msg->timestamp,
                          (db::node::node_id)msg->node,
                          (db::node::node_id)msg->data.addNeighbor.nid,
                          (face_t)msg->data.addNeighbor.face);
      break;

    case REMOVE_NEIGHBOR:
      // if(id==(db::node::node_id) reply[3])
      handleRemoveNeighbor((deterministic_timestamp)msg->timestamp,
                           (db::node::node_id)msg->node,
                           (face_t)msg->data.delNeighbor.face);
      break;

    case TAP:
      handleTap((deterministic_timestamp)msg->timestamp, (db::node::node_id)msg->node);
      break;

    case ACCEL:
      handleAccel((deterministic_timestamp)msg->timestamp,
                  (db::node::node_id)msg->node,
                  (int)reply[4]);
      break;

    case SHAKE:
      handleShake((deterministic_timestamp)msg->timestamp, (db::node::node_id)msg->node,
                  (int)reply[4], (int)reply[5], (int)reply[6]);
      break;

    case DEBUG:
      handleDebugMessage((utils::byte*)reply, (size_t)reply[0]);
      break;

    case STOP:
      stop_all = true;
      sleep(1);
      usleep(200);
      break;
    
    case SET_DETERMINISTIC_MODE:
      handleSetDeterministicMode((deterministic_timestamp)reply[2],
                                 (db::node::node_id)reply[3]);
      break;
            
    case END_POLL:
      polling = false;
	  break;

    default: cerr << "Unrecognized message " << reply[1] << endl;
    }
  }

  bool
  ensembleFinished()
  {
    return stop_all;
  }

  /*Adds the tuple to the node's work queue*/
  static void 
  addReceivedTuple(serial_node *no, size_t ts, db::simple_tuple *stpl)
  {
    if(ts>0){}

    work new_work(no, stpl);
    sched_state->new_work(no, new_work);

  }

  /*Add the neighbor to the block*/
  static void 
  addNeighbor(const size_t ts, serial_node *no, const node_val out, const face_t face, const int count)
  {
    if(!neighbor_pred)
      return;

    vm::tuple *tpl(new vm::tuple(neighbor_pred));
    tpl->set_node(0, out);
    tpl->set_int(1, static_cast<int_val>(face));

    db::simple_tuple *stpl(new db::simple_tuple(tpl, count));

    addReceivedTuple(no, ts, stpl);
  }


  static void 
  addNeighborCount(const size_t ts, serial_node *no, const size_t total, const int count)
  {
    if(!neighbor_count_pred)
      return;

    vm::tuple *tpl(new vm::tuple(neighbor_count_pred));
    tpl->set_int(0, (int_val)total);
    cout <<id<< ":Adding tuple:" << tpl << endl;

    db::simple_tuple *stpl(new db::simple_tuple(tpl, count));
    cout <<id<< ":Adding simple tuple:" << stpl << endl;

    addReceivedTuple(no, ts, stpl);
  }

#if 0
  // THIS IS NOT USED ANYWHERE???????
  static void 
  remove_neighbor_count(const size_t ts, serial_node *no, const size_t total, const int count)
  {
    vm::tuple *tpl(new vm::tuple(neighbor_count_pred));
    tpl->set_int(0, (int_val)total);
    cout <<id<< ":Adding tuple:" << tpl << endl;

    db::simple_tuple *stpl(new db::simple_tuple(tpl, count));
    cout <<id<< ":Adding simple tuple:" << stpl << endl;

    addReceivedTuple(no, ts, stpl);
  }
#endif

  static void 
  addVacant(const size_t ts,  serial_node *no, const face_t face, const int count)
  {
    if(!vacant_pred)
      return;

    vm::tuple *tpl(new vm::tuple(vacant_pred));
    tpl->set_int(0, static_cast<int_val>(face));

    db::simple_tuple *stpl(new db::simple_tuple(tpl, count));

    addReceivedTuple(no, ts, stpl);
  }


  /*function to set the id of the block */
  static void 
  handleSetID(deterministic_timestamp ts, db::node::node_id node_id)
  {
#ifdef DEBUG
    // cout << "Create node with " << node_id << endl;
#endif
    /*similar to create_n_nodes*/
    db::node *no(vm::All->DATABASE->create_node_id(node_id));
    sched_state->init_node(no);

    serial_node *no_in((serial_node *)no);
    top=NO_NEIGHBOR;
    bottom=NO_NEIGHBOR;
    east=NO_NEIGHBOR;
    west=NO_NEIGHBOR;
    north=NO_NEIGHBOR;
    south=NO_NEIGHBOR;
    neighbor_count=0;

    instantiated_flag=true;
    for(face_t face = INITIAL_FACE; face <= FINAL_FACE; ++face) {
      addVacant(ts, no_in, face, 1);
    }

    addNeighborCount(ts, no_in, 0, 1);
  }

  static void handleReceiveMessage(const deterministic_timestamp ts,
                                   db::node::node_id dest_id,
                                   const face_t face, db::node::node_id node, utils::byte *data, int offset, const int limit)
  {

    if(ts>0&&face==0&&node==0){}
    //  serial_node *origin(dynamic_cast<serial_node*>((sched_state->state).all->DATABASE->find_node(node)));
    serial_node *target(NULL);
    target=dynamic_cast<serial_node*>(vm::All->DATABASE->find_node(dest_id));

    /*
      if(face == INVALID_FACE)
      target = origin;
      else
      target = dynamic_cast<serial_node*>((sched_state->state).all->DATABASE->find_node((db::node::node_id)*(origin->get_node_at_face(face))));
    */

    simple_tuple *stpl(simple_tuple::unpack(data, limit,
                                            &offset, vm::All->PROGRAM));

#ifdef DEBUG
    //  cout << id<<":Received message from" << node << " to " << target->get_id() << " with Tuple" << *stpl << " with priority " << ts << endl;
#endif

    work new_work(target, stpl);
    sched_state->new_work(target, new_work);
  }

  static void
  handleDebugMessage(utils::byte* reply, size_t totalSize)
  {
    ignoreUnusedParamWarning(totalSize);
	message_type *m = new message_type[api::MAXLENGTH];
	message_type *msg = (message_type*) reply;
	memcpy(m, msg, msg[0]+sizeof(message_type));
	while (!ready) { 
      waitAndProcess();
	}
	debugger::messageQueue->push((message_type*)m);
  }

  static void 
  handleAddNeighbor(const deterministic_timestamp ts, const db::node::node_id in,
                    const db::node::node_id out, const face_t face)
  {
#ifdef DEBUG
    // cout << id << ":Added neighbor("<<out << " on face " << face << ")" << endl;
#endif
   
    serial_node *no_in(dynamic_cast<serial_node*>(vm::All->DATABASE->find_node(in)));
    node_val *neighbor(get_node_at_face(face));

    if(*neighbor == NO_NEIGHBOR) {
      // remove vacant first, add 1 to neighbor count
      if(has_been_instantiated()) {
        addVacant(ts, no_in, face, -1);
        addNeighborCount(ts, no_in, get_neighbor_count(), -1);
      }
      inc_neighbor_count();
#ifdef DEBUG
      //cout << id << ":neighbor count=" << get_neighbor_count() << endl;
#endif
      if(has_been_instantiated())
        addNeighborCount(ts, no_in, get_neighbor_count(), 1);
      *neighbor = out;
      if(has_been_instantiated())
        addNeighbor(ts, no_in, out, face, 1);
    } else {
      if(*neighbor != out) {
        // remove old node
        if(has_been_instantiated())
          addNeighbor(ts, no_in, *neighbor, face, -1);
        *neighbor = out;
        if(has_been_instantiated())
          addNeighbor(ts, no_in, out, face, 1);
      }
    }
  }

  static void
  handleRemoveNeighbor(const deterministic_timestamp ts,
                       const db::node::node_id in, const face_t face)
  {
  


    serial_node *no_in(dynamic_cast<serial_node*>(vm::All->DATABASE->find_node(in)));
    node_val *neighbor(get_node_at_face(face));

#ifdef DEBUG
    //  cout << id << ":Remove neighbor(" << *neighbor << ", " << face << ")" << endl;
#endif
    if(*neighbor == NO_NEIGHBOR) {
      // remove vacant first, add 1 to neighbor count
      cerr << "Current face is vacant, cannot remove node!" << endl;
      assert(false);
    } else {
      // remove old node
      if(has_been_instantiated())
        addNeighborCount(ts, no_in, get_neighbor_count(), -1);
      dec_neighbor_count();
      addVacant(ts, no_in, face, 1);
      if(has_been_instantiated())
        addNeighborCount(ts, no_in, get_neighbor_count(), 1);
    }

    addNeighbor(ts, no_in, *neighbor, face, -1);

    *neighbor = NO_NEIGHBOR;
  }

  static void 
  handleTap(const deterministic_timestamp ts, const db::node::node_id node)
  {
    //cout << id << ":tap(" << node << ")" << endl;

    serial_node *no(dynamic_cast<serial_node*>(vm::All->DATABASE->find_node(node)));

    if(tap_pred) {
      vm::tuple *tpl(new vm::tuple(tap_pred));
      db::simple_tuple *stpl(new db::simple_tuple(tpl, 1));

      addReceivedTuple(no, ts, stpl);
    }
  }

  static void 
  handleAccel(const deterministic_timestamp ts, const db::node::node_id node,
              const int_val f)
  {
    //cout << id << ":accel(" << node << ", " << f << ")" << endl;

    serial_node *no(dynamic_cast<serial_node*>(vm::All->DATABASE->find_node(node)));

    if(accel_pred) {
      vm::tuple *tpl(new vm::tuple(accel_pred));
      tpl->set_int(0, f);

      db::simple_tuple *stpl(new db::simple_tuple(tpl, 1));

      addReceivedTuple(no, ts, stpl);
    }
  }


  static void 
  handleShake(const deterministic_timestamp ts, const db::node::node_id node,
              const int_val x, const int_val y, const int_val z)
  {
    // cout << id << ":shake(" << node << ", " << x << ", " << y << ", " << z << ")" << endl;

    serial_node *no(dynamic_cast<serial_node*>(vm::All->DATABASE->find_node(node)));

    if(shake_pred) {
      vm::tuple *tpl(new vm::tuple(shake_pred));
      tpl->set_int(0, x);
      tpl->set_int(1, y);
      tpl->set_int(2, z);

      db::simple_tuple *stpl(new db::simple_tuple(tpl, 1));

      addReceivedTuple(no, ts, stpl);
    }
  }

  /*Helper functions end*/

  /*Debugger Messages*/
  void 
  debugGetMsgs(void)
  {
	static message_type msg[api::MAXLENGTH];
	message_type *m;
	if (stop_all) {
      exit(0);
	}
	switch (vm::determinism::getSimulationMode()) {
    case REALTIME :
      if (!pollAndProcess(NULL)) {
        exit(0);
      }
      break;
    case DETERMINISTIC :
      while (my_tcp_socket->available()) {
        if (readAMessage(msg)) {
          message* c =(message*)msg;
          if (c->command == DEBUG) {
            processMessage(msg);
          } else {
            m = new message_type[api::MAXLENGTH];
            memcpy(m, msg, msg[0]+sizeof(message_type));
            messageQ.push(m);
          }
        } else {
          exit(0);
        }
      }
      break;
    default:
      break;
	}
  }

  void 
  debugBroadcastMsg(message_type *msg, size_t messageSize)
  {
    ignoreUnusedParamWarning(msg);
    ignoreUnusedParamWarning(messageSize);
  }

  void 
  debugWaitMsg(void)
  {
	static message_type msg[api::MAXLENGTH];
	message_type *m;
	bool debugMsgReceived = false;
	switch (vm::determinism::getSimulationMode()) {
    case REALTIME :
      if (!waitAndProcess()) {
        exit(0);
      }
      break;
    case DETERMINISTIC :
      while (!debugMsgReceived) {
        if (readAMessage(msg)) {
          message* c =(message*)msg;
          if (c->command == DEBUG) {
            processMessage(msg);
            debugMsgReceived = true;
          } else {
            m = new message_type[api::MAXLENGTH];
            memcpy(m, msg, msg[0]+sizeof(message_type));
            messageQ.push(m);
          }
        } else {
          exit(0);
        }
      }
      break;
    default:
      break;
	}
  }

#if defined(INCLUDE_DEBUGGER)
  /*NEXT PROCESS --when in Mpi debugging mode, find the next process*/
  int nextProcessForDebugger(void) {
    assert(0);
  }
#endif


  /* Output the database in a synchronized manner */
  void 
  dumpDB(std::ostream &out, const db::database::map_nodes &nodes)
  {
    ignoreUnusedParamWarning(out);
    ignoreUnusedParamWarning(nodes);

  }
 
  /*Print the database*/  
  void 
  printDB(std::ostream &out, const db::database::map_nodes &nodes)
  {
    ignoreUnusedParamWarning(out);
    ignoreUnusedParamWarning(nodes);

  }

  void 
  debugSendMsg(int destination,message_type* msg, size_t messageSize)
  {
    ignoreUnusedParamWarning(destination);
    ignoreUnusedParamWarning(messageSize);

    msg[2] = (message_type) getCurrentLocalTime();
    sendMessageTCP((message*)msg);
    delete[] msg;
  }

  void regularPollAndProcess(sched::base *sched) {
	static uint i = 0;
	
	if ( (i%5) == 0) {
      pollAndProcess(sched);
	}
	i++;
	/*
      switch (vm::determinism::getSimulationMode()) {
      case REALTIME:
      if ( (i%5) == 0) {
      pollAndProcess(sched, all);
      }
      i++;
      break;
      case DETERMINISTIC :
      pollAndProcess(sched, all);
      break;
      }*/
  }

  bool isInBBSimMode() {return true;}

}

// Local Variables:
// tab-width: 4
// indent-tabs-mode: nil
// End:

