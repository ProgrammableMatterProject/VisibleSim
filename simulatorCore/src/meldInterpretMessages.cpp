#include "meldInterpretMessages.h"
#include "meldInterpretVM.h"

class AddTupleMessage{
      AddTupleMessage(tuple_t tpl) : Message(){
            tuple_t t = tpl;
            type = ADD_TUPLE_MSG_ID;
      }

      int size(){
            return TYPE_SIZE(TUPLE_TYPE(t));
      }

      string getMessageName(){
            return "Add Tuple Message";
      }
}

class RemoveTupleMessage{
      RemoveTupleMessage(tuple_t tpl) : Message(){
            tuple_t t = tpl;
            type = REMOVE_TUPLE_MSG_ID;
      }

      int size(){
            return TYPE_SIZE(TUPLE_TYPE(t));
      }

      string getMessageName(){
            return "Remove Tuple Message";
      }
}
