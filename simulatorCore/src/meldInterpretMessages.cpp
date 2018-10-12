#include "meldInterpretMessages.h"
#include "meldInterpretVM.h"

namespace MeldInterpret{

AddTupleMessage::AddTupleMessage(tuple_t tpl, unsigned int s) : Message(){
      tuple = tpl;
      messageSize = s;
      type = ADD_TUPLE_MSG_ID;
}

unsigned int AddTupleMessage::size(){
      return messageSize;
}

string AddTupleMessage::getMessageName(){
      return "Add Tuple Message";
}

RemoveTupleMessage::RemoveTupleMessage(tuple_t tpl, unsigned int s) : Message(){
      tuple = tpl;
      messageSize = s;
      type = REMOVE_TUPLE_MSG_ID;
}

unsigned int RemoveTupleMessage::size(){
      return messageSize;
}

string RemoveTupleMessage::getMessageName(){
      return "Remove Tuple Message";
}

}
