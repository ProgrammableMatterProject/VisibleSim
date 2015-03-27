
/*doubly linked lists to store breakpoints*/

#include <string>
#include <stdlib.h>
#include <string.h>
#include <iostream>
#include "debug/debug_list.hpp"

using namespace std;

namespace debugger {

/*returns an empty breakpoint list - an empty breakpoint list has
  one node but NULL data*/
debugList newBreakpointList(){
  debugList newList = (debugList)malloc(sizeof(debugList));
  newList->back = (struct debugnode *)malloc(sizeof(struct debugnode));
  newList->front = newList->back;
  newList->front->next = NULL;
  newList->front->prev = NULL;
  newList->front->type = NULL;
  newList->front->name = NULL;
  newList->front->nodeID = -1;
  return newList;
}

/*frees entire list including the incduding the
  type and name objects*/
void listFree(debugList L){

  if (L == NULL) return;

  struct debugnode* tmp;
  struct debugnode* ptr = L->front;

  while(ptr!=NULL){
    tmp = ptr;
    ptr = ptr->next;
    if (tmp->type!=NULL)
      free(tmp->type);
    if(tmp->name!=NULL)
      free(tmp->name);
    free(tmp);
  }
  free(L);
}

//insert object at end of list
//object must be dynamically allocated
void insertBreak(debugList L, char* type, char* name, int  nodeID){


    if (!isInBreakPointList(L,type,name,nodeID)){

        /*insert the data*/
        L->back->type = type;
        L->back->name = name;
        L->back->nodeID = nodeID;

        /*instantiate a tailing blank node*/
        L->back->next = (struct debugnode*)malloc(sizeof(struct debugnode));
        L->back->next->prev = L->back;
        L->back->next->next = NULL;
        L->back->next->type = NULL;
        L->back->next->nodeID = -1;
        L->back->next->name = NULL;
        L->back = L->back->next;

    }

}

/*returns whether the debugList is empty*/
bool isListEmpty(debugList L){
  return L->front == L->back;
}

/*checks to see whether the the parameters inputed for a breakpoint
  are a hit in the list*/
bool isInBreakPointList(debugList L, char* type, char* name, int nodeID){

  if(isListEmpty(L))
    return false;

  for (struct debugnode* ptr = L->front; ptr->next!=NULL; ptr=ptr->next){
    if (!strcmp(ptr->type,type)&&
	//if "" the name or nodeId doesn't matter
	(!strcmp(ptr->name,name)||!strcmp(ptr->name,""))&&
	(ptr->nodeID == nodeID ||ptr->nodeID == -1))
      return true;
  }
  return false;
}

//removes a breakpoint from the list
//returns 0 on success, -1 if specified is not in breakpoint list
int removeBreakPoint(debugList L, char* type, char* name, int nodeID){

  struct debugnode* tmp;

  if (isListEmpty(L))
    return -1;

  for (struct debugnode* ptr = L->front;
       ptr->next!=NULL; ptr = ptr->next){
    if (!strcmp(ptr->type,type)&&
	//if "" the name or nodeId doesn't matter
	(!strcmp(ptr->name,name)||!strcmp(ptr->name,""))&&
	(ptr->nodeID == nodeID ||ptr->nodeID == -1)){
      if (ptr == L->front){
	L->front = ptr->next;
	L->front->prev = NULL;
	tmp = ptr;
	free(tmp->type);
	free(tmp->name);
	free(tmp);
	return 0;
      } else if (ptr == L->back){
	L->back = ptr->prev;
	L->back->next = NULL;
	tmp = ptr;
	free(tmp->type);
	free(tmp->name);
	free(tmp);
	return 0;
      } else {
	ptr->next->prev = ptr->prev;
	ptr->prev->next = ptr->next;
	tmp = ptr;
	free(tmp->type);
	free(tmp->name);
	free(tmp);
	return 0;
      }
    }
  }
  return -1;
}


    void printList(std::ostream& out,debugList L){
      int count  = 1;
      if (isListEmpty(L)){
          out << ": (empty)" << endl << endl;
          return;
      } else
          out << endl;

      out << "\tTYPE\t\t\tNAME\t\t\tNODEID" << endl;
      for (struct debugnode* ptr = L->front; ptr->next!=NULL; ptr=ptr->next){
          out << count << ". ";
          out << "\t" << ptr->type;
          if (strcmp(ptr->name,"")!=0)
              out << "\t\t\t" << ptr->name;
          else
              out << "\t\t\t" << "(nil)";
          if (ptr->nodeID != -1)
              out << "\t\t\t" << ptr->nodeID << endl;
          else
              out << "\t\t\t" << "(nil)" << endl;
          count++;
      }
  }


}


