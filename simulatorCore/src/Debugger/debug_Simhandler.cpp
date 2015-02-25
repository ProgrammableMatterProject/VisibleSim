/*DEBUGGER FOR BLINKY BLOCK SIMULATOR*/

#include <string.h>
#include <pthread.h>
#include <iostream>
#include <sstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "debug_Simprompt.hpp"
#include "debug_Simhandler.hpp"
#include "serialization.hpp"
#include "types.hpp"
#include "blinkyBlocksVM.h"
#include <list>

using namespace std;
using namespace debugger;


namespace debugger {

#define SIZE (sizeof(message_type))
#define BROADCAST true

    /*****************************************************************/

    /*GLOBAL STORED VARS*/

    /****************************************************************/

    ConcurrentQueue<message_type*> *messageQueue;
    /*global variables to controll main thread*/
  static bool isSystemPaused = true;
    static bool isDebug = false;
    static bool isSimDebug = false;
    static bool isPausedAtBreakpoint = false;
	static bool okayToBroadcastPause = true;

    static bool okayToPauseSimulation = false;

    static bool printLimitation = true;
    static bool okayToPrint = true;

    /*number of messages the Master expects to recieve*/
    int numberExpected = 0;

    bool verboseMode = false;
    bool serializationMode = false;

    /*cache ...rcvMessageList holds containters for
     * different lists of broken messages*/
    std::list<struct msgListContainer*>* rcvMessageList;

    /*BROKEN UP MESSAGE CONTAINERS TO BE STORED IN CACHE*/

    /*rcvMessageList Elems -- contains a list that holds incoming messages
     * from a certain node*/
    struct msgListContainer {
        int instruction;
        int node;
        std::list<struct msgListElem*>* msglist;
    };

    /*msgListElems -- holds the information necessary to reconstruct the
     * the message*/
    struct msgListElem{
        int priority;
        string content;
    };


    /**********************************************************************/

    /* I/0 SPECIFICATION PARSING - extract the different parts of
     * a specification format*/

    /*********************************************************************/


    /*CHARACTER IN STRING INDEX -- returns the index of a character in a string,
     *if it is not there it returns -1*/
    int characterInStringIndex(string str, char character){
        for(unsigned int i = 0; i < str.length(); i++){
            if (str[i] == character)
                return (int)i;
        }
        return -1;
    }


    /*GET TYPE -- extracts the type from the specification from input line
     *returns the type of breakpoint from the specification
     *invariant-- the type is always specified*/
    string getType(string specification){
        string build = "";
        for (unsigned int i = 0; i < specification.length(); i++){
            if(specification[i] == ':' || specification[i] == '@')
                return build;
            else
                build += specification[i];
        }
        return build;
    }


    /*GET NAME -- extracts the name from the specification
     *returns the name from the specification
     *returns "" if name is not present*/
    string getName(string specification){
        string build = "";
        //find index of colon
        int index = characterInStringIndex(specification, ':');
        // if colon not there
        if (index == -1)
            return "";
        for (unsigned int i = index +1;
             i < specification.length(); i++){
            if (specification[i] == '@')
                return build;
            else
                build += specification[i];
        }
        return build;
    }


    /*GET NODE -- extracts the node from the specification
     *returns the node from the specification
     *returns "" if node is not given*/
    string getNode(string specification){
        string build = "";
        int index = characterInStringIndex(specification, '@');
        if (index == -1)
            return "";
        for (unsigned int i = index+1; i < specification.length(); i++){
            build+=specification[i];
        }
        return build;
    }

    /*TYPE INT TO STRING -- given the encoding return
     *  the corresponding string for
     *  break point types*/
    string typeInt2String(int type){
        switch(type){
            case FACTDER:
                return "factDer";
            case FACTCON:
                return "factCon";
            case FACTRET:
                return "factRet";
            case ACTION:
                return "action";
            case SENSE:
                return "sense";
            case BLOCK:
                return "block";
        }
        return "";
    }

    /**********************************************************************/

    /*SIMULATION DEBUGGING FUNCTIONS*/

    /*********************************************************************/


    /*SETFLAGS -- switch to different debugging modes based on user input*/
   void setFlags(string specification){
        ostringstream msg;
        for (uint i = 0; i < specification.length(); i++){
            if ((uint)specification[i] == 'V'){
                verboseMode = true;
            } else if ((uint)specification[i] == 'S'){
                serializationMode = true;
            }
        }
    }


    /*HANDLE PAUSE COMMAND -- broadcast a pause message to all VMs
     *  -used when the user presses P for a runnung system*/
    void handlePauseCommand(void){

        sendMsg(-1,PAUSE,"",BROADCAST);
        okayToBroadcastPause = false;

    }


    /*DEBUG CONTROLLER -- send messages to the VMs based off the user input*/
    void debugController(int instruction, string specification){

        string type;
        string name;
        string node;


        /*keep tract of number of messages expected*/
        okayToPrint = true;
        if (instruction == CONTINUE || instruction == UNPAUSE){
            okayToBroadcastPause = true;
            okayToPauseSimulation = true;
            printLimitation = false;
            numberExpected = sendMsg(-1,CONTINUE,"X",BROADCAST);            
            unPauseSimulation();
        } else if (instruction == RUN){
            okayToBroadcastPause = true;
            okayToPauseSimulation = true;
            printLimitation = false;
            /*continue a paused system by broadcasting an CONTINUE signal*/
            numberExpected = sendMsg(-1,CONTINUE,"X",BROADCAST);
            unPauseSimulation();
        } else if (instruction == DUMP) {


            okayToPauseSimulation = false;
            /*broadcast the message to all VMs*/
            if (specification == "all"){

                numberExpected = sendMsg(-1,DUMP,specification,BROADCAST);

                printLimitation = false;
            } else {

                printLimitation = false;
                /*send to a specific VM to dump content*/
                sendMsg(atoi(specification.c_str()),DUMP,specification);
                numberExpected = 1;
            }

            /*handle the breakpoints in the lists*/
        } else if (instruction == REMOVE||instruction == BREAKPOINT) {


            okayToPauseSimulation = false;
            /*extract the destination*/
            node = getNode(specification);
            if (node == ""){

                printLimitation = true;
                /*broadcast the message if the node is not specified*/
                numberExpected = sendMsg(-1,instruction,specification,BROADCAST);

            } else {

                printLimitation = true;
                /*send break/remove to a specific node */
                sendMsg(atoi(node.c_str()),instruction,specification);
                numberExpected = 1;

            }


        } else if (instruction == PRINTLIST) {

            okayToPauseSimulation = false;
            printLimitation = false;
            /*broadcast  a PRINT LIST message*/
            numberExpected = sendMsg(-1,PRINTLIST,"",BROADCAST);

        } else if (instruction == MODE) {


            okayToPauseSimulation = false;
            printLimitation = true;
            /*set flags and tell the VMs of the update*/
            setFlags(specification);
            numberExpected = sendMsg(-1,MODE,specification,
                                     BROADCAST);
        } else if (instruction == TIME) {
            pauseSimulation(atoi(specification.c_str()));
            numberExpected = 1;
        }

    }


    /*DEBUG MASTER CONTROLLER -- the controller for the master
     *  MPI debugger-called when messages are
     *  received*/
    void debugMasterController(int instruction, string specification){

        /*print the output and then tell all other VMs to pause*/
        if (instruction == BREAKFOUND){
            printf("%s",specification.c_str());
			/*only able to broadcast pause per one run/continue*/
            if(okayToBroadcastPause) {
				sendMsg(-1,PAUSE,"",BROADCAST);
				okayToBroadcastPause = false;
            }
        } else if (instruction == PRINTCONTENT){
            /*if not verbose, only print from first VM to respond*/
            if (!verboseMode&&printLimitation&&okayToPrint){
                printf("%s",specification.c_str());
                okayToPrint  = false;
                /*print all*/
            } else if (verboseMode||!printLimitation) {
                printf("%s",specification.c_str());
            }
        } else if (instruction == TERMINATE){
            printf("PROGRAM FINISHED\n");
            exit(0);
        } else if (instruction == PAUSE){

            /*prints more information*/
            if (verboseMode){
                printf("%s",specification.c_str());
            }

        } else if (instruction == TIME){
            printf("%s",specification.c_str());
            if(okayToBroadcastPause) {
				sendMsg(-1,PAUSE,"",BROADCAST);
				okayToBroadcastPause = false;
            }
        }
    }


    /***************************************************************************/

    /*DEBUG MESSAGE SENDING - send message over a connection
     *  -messages are first broken into packets, attatched with a header
     *   when sent
     *  -the reciever stores the packets in a cache until the whole message can
     *   be reconsructed and processed*/

    /***************************************************************************/


    /*MESSAGE QUEUE INSERT -- used by simulator to update debugger message queue*/
    void messageQueueInsert(uint64_t* msg){
        messageQueue->push(msg);
    }

    /*GET MAX CHAR ARRAY SIZE--return the size of the maximum char array size*/
    inline int getMaxCharArraySize(){

        /*will always return an integer*/
        return MAXLENGTH*SIZE -
            (sizeof(int)+4*sizeof(message_type)+sizeof(size_t)
             +sizeof(int)+8);
    }


    /*PACK LIST --break message into parts while
     *it is longer than the buffer size and store
     * it in a list to be ready to be sent.
     * Attatch a header to know how many messages the
     * list contains including the header*/
    std::list<message_type*>* packList(int msgEncode, string content){

        std::list<message_type*>* msgList = new std::list<message_type*>();

        message_type* msg;

        string partition;
        int priority = 1;
        /*if too big*/
        while (content.length() + 1 > (uint)getMaxCharArraySize()){

            /*pack the broken message and put it in the list*/
            partition = content.substr(0,getMaxCharArraySize());
            content = content.substr(getMaxCharArraySize()+1);
            msg = pack(msgEncode,partition,priority);
            msgList->push_back(msg);
            priority++;
        }

        /*put the last message that is smaller than buffer*/
        ostringstream convert;
        convert << priority + 1;
        msgList->push_back(pack(msgEncode,content,priority));

        /*attatch the header with priority 0, and the number of messages
         * as the content*/
        msgList->push_back(pack(msgEncode,convert.str(),0));
        return msgList;
    }




    /*PACK--given the type encoding and content, pack the information into
     * a sendable messeage.
     * priority is the number in a message group*/
    message_type* pack(int msgEncode, string content, int priority){

        utils::byte* msg = (utils::byte*)new message_type[MAXLENGTH];
        int pos = 0;

        message_type debugFlag =  DEBUG;
        size_t contentSize = content.length() + 1;
        size_t bufSize = MAXLENGTH*SIZE;//bytes
        message_type msgSize = bufSize-SIZE;//according to message spec
        message_type timeStamp = 0;
        message_type nodeId = 0;

        /*message size in bytes*/
        utils::pack<message_type>(&msgSize,1,msg,bufSize,&pos);

        /*debug indicator*/
        utils::pack<message_type>(&debugFlag,1,msg,bufSize,&pos);

        /*timestamp*/
        utils::pack<message_type>(&timeStamp,1,msg,bufSize,&pos);

        /*VM id*/
        utils::pack<message_type>(&nodeId,1,msg,bufSize,&pos);

        /*priority of the message*/
        utils::pack<int>(&priority,1,msg,bufSize,&pos);

        /*debug command encoding*/
        utils::pack<int>(&msgEncode,1,msg,bufSize,&pos);

        /*size of content*/
        utils::pack<size_t>(&contentSize,1,msg,bufSize,&pos);

        /*content*/
        utils::pack<char>((char*)content.c_str(),content.size()+1,
                                 msg,bufSize,&pos);

        return (message_type*)msg;

    }



    /*SENDMSG--the desination specified is the user input node ID*/
    /* send a debug message to another process through the API layer*/
    /* if send to all, specify BROADCAST (see top)*/
    int sendMsg(int destination, int msgType,
              string content, bool broadcast)  {

            /*length of array*/
            size_t msgSize = MAXLENGTH*SIZE;
            /*pack the message into a broken list*/
            std::list<message_type*>* msgList = packList(msgType,content);
            message_type* msg;


            int expected;

            /*send all the messages in the list*/
            while (!msgList->empty()){

                msg = msgList->front();
                msgList->pop_front();


                if (broadcast){
                    expected = debugSendMsg(-1,msg,msgSize);

                } else {

                    /*get the process id (getVMId) and send the message*/
                    expected = debugSendMsg(destination,msg,
                                        msgSize);

                    /*error*/
                    if (expected < 0){
                        messageQueueInsert(pack(PRINTCONTENT,
                                                "Node does not exist\n",1));
                        messageQueueInsert(pack(PRINTCONTENT,"2",0));
                        return 1;
                    }
                }


            }

            delete msgList;

            return expected;

    }



    /* RECEIVE MESSAGE -- ask for messages from the simulator,
     *  if there are messages, put them in the cache.
     *  if a whole message has been completed in the cache
     *  reconctruct it and process it in the master contoller*/
    void receiveMsg(void){
		
        utils::byte *msg;
        int instruction;
        char specification[MAXLENGTH*SIZE];
        int pos = 0;
        message_type size,debugFlag,timeStamp,NodeId;
        size_t specSize;
        int priority;

        struct msgListContainer* msgContainer;
		
        while(!messageQueue->empty()){

            /*process each message until empty*/
            /*extract the message*/
            msg = (utils::byte*)messageQueue->front();

            /*==>UNPACK the message into readable form*/
            /*msgsize in bytes*/
            utils::unpack<message_type>(msg,MAXLENGTH*SIZE,
                                             &pos,&size,1);
            /*debugFlag*/
            utils::unpack<message_type>(msg,MAXLENGTH*SIZE,
                                             &pos,&debugFlag,1);
            /*timestamp*/
            utils::unpack<message_type>(msg,MAXLENGTH*SIZE,
                                             &pos,&timeStamp,1);
            /*place msg came from*/
            utils::unpack<message_type>(msg,MAXLENGTH*SIZE,
                                             &pos,&NodeId,1);
            /*priority*/
            utils::unpack<int>(msg,MAXLENGTH*SIZE,
                                       &pos,&priority,1);
            /*command encoding*/
            utils::unpack<int>(msg,MAXLENGTH*SIZE,&pos,&instruction,1);
            /*content size*/
            utils::unpack<size_t>(msg,MAXLENGTH*SIZE,&pos,&specSize,1);
            /*content*/
            utils::unpack<char>(msg,MAXLENGTH*SIZE,&pos,
                                &specification,specSize);

            string spec(specification);



             /*insert the message into a cache to be checked if the
             *broken message in pieces has been completed*/
            insertMsg(spec, priority, instruction, NodeId);

            /*check to see if a total message has been sent*/
            msgContainer = checkAndGet();

            /*messages are ready to be processed*/
            if (msgContainer!= NULL){
                instruction = msgContainer->instruction;
                spec = buildString(msgContainer);

            /*no messages completed*/
            } else {

                /* resetup*/
                memset(specification,0,MAXLENGTH*SIZE);
                messageQueue->pop();
                //memset(msg,0,MAXLENGTH*SIZE);
                delete[] msg;
                pos = 0;
                continue;
            }


            /*process a reconstructed message*/
            debugMasterController(instruction,spec);
            numberExpected--;

            /*if all expected messages have been received*/
            if (numberExpected == 0 && okayToPauseSimulation)
                pauseSimulation(-1);

            /*set up the variables and buffers for next message*/
            memset(specification,0,MAXLENGTH*SIZE);
            messageQueue->pop();
            //memset(msg,0,MAXLENGTH*SIZE);
            delete[] msg;
            pos = 0;
        }
    }


    /************************************************************************/

    /*RECONSTRUCTING/STORING PARTITIONED MESSAGES
     *  -Receiving and constructing broken apart message algorithms*/

    /************************************************************************/


    /*INSERT MESSAGE--insert the a parted message into the cache (a list of lists)*/
    void insertMsg(string content, int priority, int instruction, int node){

         std::list<struct msgListContainer*>::const_iterator it;
         struct msgListElem* elem;
         struct msgListContainer* contain;

         for (it = rcvMessageList->begin();it!=rcvMessageList->end();it++){

             contain = *it;

             /*a message that matches already came from a node, insert it with
              * that node*/
             if (contain->instruction == instruction && contain->node == node){
                 elem = new struct msgListElem;
                 elem->priority = priority;
                 elem->content = content;
                 contain->msglist->push_back(elem);
                 return;
             }
         }


         /*if no message like it exist yet create a new list in the cache*/
         contain = new struct msgListContainer;
         contain->instruction = instruction;
         contain->node = node;
         contain->msglist = new std::list<struct msgListElem*>();
         elem = new struct msgListElem;
         elem->priority = priority;
         elem->content = content;
         contain->msglist->push_back(elem);
         rcvMessageList->push_back(contain);
    }


    /*CHECK AND GET -- check to see if any lists in the cache have completed
     * i.e.  all broken partitioned messages for a command have been sent*/
    struct msgListContainer* checkAndGet(void){

        std::list<struct msgListElem*>* msgList;
        std::list<struct msgListElem*>::iterator iter;
        std::list<struct msgListContainer*>::iterator it;
        struct msgListContainer* contain;
        struct msgListElem* elem;

        /*iterate through cache*/
        for (it = rcvMessageList->begin();
             it!=rcvMessageList->end();it++){
            contain = *it;
            msgList = contain->msglist;

            /*iterate through list within cache*/
            for (iter = msgList->begin();iter!=msgList->end();iter++){
                elem = *iter;

                /*find the header and check if it is done if the size expected
                 *matches the size of the list*/
                if (elem->priority == 0 &&
                    (uint)atoi(elem->content.c_str()) == (uint)msgList->size()){
                    return contain;
                }
            }
        }

        /*no commands have been completed*/
        return NULL;
    }

    /*PRINT RECIEVED-- print the cache (used for debugging the debugger :) ) */
    void printRcv(void){

        std::list<struct msgListElem*>* msgList;
        std::list<struct msgListElem*>::iterator iter;
        std::list<struct msgListContainer*>::iterator it;
        struct msgListContainer* contain;
        struct msgListElem* elem;

        ostringstream msg;

        msg << "#######################################" << endl <<endl;

        for (it = rcvMessageList->begin();
             it!=rcvMessageList->end();it++){
            contain = *it;
            msg << "========================" << endl;
            msg << "instruction: " << contain->instruction << endl;
            msg << "node: " << contain->node << endl;
            msg << "========================" << endl;
            msgList = contain->msglist;
            for (iter = msgList->begin();iter!=msgList->end();iter++){
                elem = *iter;
                msg << "\t*****************" << endl;
                msg << "\tpriority: " << elem->priority << endl;
                msg << "\tcontent: " << elem->content << endl;
                msg << "\t*****************" << endl;
            }
            msg << "========================" << endl;
        }

        printf("%s",msg.str().c_str());
    }


    /*BUILD STRING -- rebuild a broken message that is ready to
     * be completed*/
    string buildString(struct msgListContainer* container){

        ostringstream msg;

        std::list<struct msgListElem*>::iterator it;
        struct msgListElem* elem;
        std::list<struct msgListElem*>* msgList = container->msglist;



        for (it = msgList->begin(); it!=msgList->end(); it++){
            elem = *it;
            /*ignore the header and reconstruct the full content*/
            if (elem->priority!=0)
                msg << elem->content;
            delete elem;
        }

        rcvMessageList->remove(container);
        delete container;

        return msg.str();
    }

}
