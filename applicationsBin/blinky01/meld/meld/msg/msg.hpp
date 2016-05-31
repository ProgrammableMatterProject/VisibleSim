#define MAX_MSG 128
#include "api/api.hpp"

using namespace api;

namespace msg{
  static const size_t MAXLENGTH = 512 / sizeof(api::message_type);

  typedef struct _message_format {
    message_type size;
    message_type command;
    message_type timestamp;
    message_type node;
    union {
    // CMD_CREATE
      struct {
        message_type num;
        message_type start;
      } create;
	//CMD_SEND_MESSAGE
      struct{
        message_type face;
        message_type dest_nodeID;
      }send_message;

  //CMD RECEIVE_MESSAGE
      struct{
        message_type face;
        message_type from;
      }receiveMessage;   

    // CMD_COLOR
      struct {
        message_type r;
        message_type g;
        message_type b;
        message_type i;
      } color;

    // CMD_ADD_NBR
      struct {
        message_type nid;
        message_type face;
      } addNeighbor;

    // CMD_ADD_NCOUNT
    // CMD_DEL_NCOUNT
      struct {
        message_type total;
      } neighborCount;

    // CMD_HAS_RUN
      struct {
        message_type num;
        message_type other[];
      } runtil;

    // CMD_ADD_VACANT
    // CMD_DEL_VACANT
      struct {
        message_type face;
      } vacant;

    // CMD_NBR_DEL
      struct {
        message_type face;
      } delNeighbor;
      
      // CMD_WORK_END (DETERMINISM)
      struct {
		message_type nbRecMsg;
	  } workEnd;

    //CMD TAP
      struct{
      }tap;

      message_type units[MAX_MSG];

    } data;
  } message;  
}
