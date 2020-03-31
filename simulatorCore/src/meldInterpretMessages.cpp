#include "meldInterpretMessages.h"
#include "meldInterpretVM.h"

namespace MeldInterpret{

AddTupleMessage::AddTupleMessage(tuple_t tpl, unsigned int s) : Message(){
      tuple = tpl;
      messageSize = s;
      type = ADD_TUPLE_MSG_ID;
}

unsigned int AddTupleMessage::size() const {
      return messageSize;
}

string AddTupleMessage::getMessageName() const {
      return "Add Tuple Message";
}

RemoveTupleMessage::RemoveTupleMessage(tuple_t tpl, unsigned int s) : Message(){
      tuple = tpl;
      messageSize = s;
      type = REMOVE_TUPLE_MSG_ID;
}

unsigned int RemoveTupleMessage::size() const {
      return messageSize;
}

string RemoveTupleMessage::getMessageName() const {
      return "Remove Tuple Message";
}

}
