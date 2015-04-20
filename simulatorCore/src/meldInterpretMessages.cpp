#include "meldInterpretMessages.h"
#include "meldInterpretVM.h"


namespace MeldInterpret{

AddTupleMessage::AddTupleMessage(tuple_t tpl) : Message(){
      tuple = tpl;
      type = ADD_TUPLE_MSG_ID;
}

unsigned int AddTupleMessage::size(){
      return TYPE_SIZE(TUPLE_TYPE(tuple));
}

string AddTupleMessage::getMessageName(){
      return "Add Tuple Message";
}


RemoveTupleMessage::RemoveTupleMessage(tuple_t tpl) : Message(){
      tuple = tpl;
      type = REMOVE_TUPLE_MSG_ID;
}

unsigned int RemoveTupleMessage::size(){
      return TYPE_SIZE(TUPLE_TYPE(tuple));
}

string RemoveTupleMessage::getMessageName(){
      return "Remove Tuple Message";
}

}
