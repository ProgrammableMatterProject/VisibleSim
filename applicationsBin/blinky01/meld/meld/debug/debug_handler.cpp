/*INTERFACE TO HANDLE BREAKPOINTS, DUMPS, AND CONTINUES*/
/*
 *     David Campbell - dncswim76@gmail.com
 */

/* ===============HOW THE DEBUGGER IS IMPLEMENTED====================== */

/* MODES
   The debugger has three different modes: normal, mpi, and simulation.
   These different modes are set within from the command line when meld
   is executed in which a debugger global var is used to indicate the mode.
   These are set in meld.cpp, then main meld file.

   COMMAND LINE INITIATIVES
   In each mode a command line prompt is initiated in which the user
   can input specific commands to controll the VM.  In MPI debugging mode,
   this prompt is the 0th process setup by boost MPI, and in SIM debugging
   mode, it is a thread that is embedded within the simulator itself.
   In normal debugging mode, it is a thread that is run in the VM itself.
   In debug_prompt.cpp, this is the prompt code that will parse and send
   the input to the debugging controller to react to the input.

   MESSAGE API LAYER
   The controller will then send a message to the respective VMs running
   alongside it, else it will print to cout if not running more than one
   process.  The messages are sent through the api layer. (SIM should be
   compiled with api/bbsimpi.cpp and MPI with api/mpi.cpp)

   INSERTING BREAKPOINTS
   When a breakpoint is specified, the controller will tell the VMs to insert
   the breakpoint into their list of breakpoints.  The VMs are initially paused
   when the program begins, waiting for feedback.  When the user specifies to
   run the program, if a breakpoint is in the breakpoint list and its a match
   at key points in the program the system will be paused again.

   HITTING BREAKPOINTS
   When a breakpoint is hit, the VM lets the controller know a breakpoint was
   reached and the controller may pause all VMs (if no one reached a breakpoint)
   before it.  Since there are many VMs running loose (in simulation or MPI),
   there may be more than one break point at once.  If a break point is not
   reached, the VMs will be paused in sched/base.cpp.  If verbose mode is set
   the user can see that all VMs are paused at a certain point.

   SET BREAKPOINTS
   When a breakpoint is set, only one VM response will be printed unless verbose
   mode is set.

   DUMPS
   To dump, the debugger simply calls the db::print_database commands.

   BREAKPOINTS
   The user can also print and remove breakpoints from the list. See
   debug_list.cpp.

   SENDING MESSAGES
   It is possible for the debugger to need to send a message of an arbitrary
   size.  Therefore, since we have a limited buffer size, a message is packed
   into a list of packets that are sent to their destination.  For every message,
   there will be a header packet that had priority 0 and the total amount
   of packets in the message.  When recieving, these packets are stored in a
   cache that holds incoming packets in the correct message.  If the message
   has recieved all the packets, it will reconstruct it, and then process it.

   SERIALIZATION
   To have only one VM executed at a time, the debugger can be put into
   serialization mode.  Only the VM with the conche is allowed to execute.
   When it has finished one round of work, it will tell the master to
   pass the conche indicating who will get it next.  These messages are hidden
   from the user.  The simulation debugger cannot go into serialization mode.

*/

#include <string.h>
#include <pthread.h>
#include <iostream>
#include <sstream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "db/database.hpp"
#include "debug/debug_prompt.hpp"
#include "debug/debug_handler.hpp"
#include "utils/serialization.hpp"
#include "utils/types.hpp"
#ifdef SIMD
#include "vm/determinism.hpp"
#endif

using namespace std;
using namespace vm;
using namespace debugger;


namespace debugger {

#define SIZE (sizeof(api::message_type))
#define BROADCAST true

    /*****************************************************************/

    /*
     *  GLOBAL VARIABLES/POINTERS
     *   -to store information about the system*/

    /****************************************************************/


    /*message Queue to handle incomming messages*/
    /*the api layer should use this when handling with debuf functions*/
    std::queue<api::message_type*> *messageQueue = NULL;


    /*>>> THE FOLLOWING FOUR ARE USED FOR SYCHRONIZATION <<< */
    /*is in the pauseIt loop (see pauseIt)*/
    static bool isSystemPaused = true;
    /*used to only broadcast one pause message
     *when a breakpoint is hit*/
    static bool okayToBroadcastPause = false;
    /*the program is suspended in the do_loop function
     *see (sched/base.cpp)*/
    bool isPausedInWorkLoop = false;
    bool isPausedInDeterministicPollLoop = false;
    /*the program has reached a breakpoint*/
    static bool isPausedAtBreakpoint = false;


    /*different debugging modes*/
    static bool isDebug = false;
    static bool isSimDebug = false;
    static bool isMpiDebug = false;

    /*for serializationMode*/
    static bool hasTheConche = false;
#if defined(TARGET_mpi)
    static bool okayToSetConche = true;
#endif

    /*different mode settings for debugger*/
    bool verboseMode = false;
    bool serializationMode = false;


    /*the pointer to the list of break points*/
    static debugList factBreakList = NULL;

    /*number of messages the Master expects to recieve*/
    int numberExpected = 0;

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

    /******************************************************************/

    /*
     * DEBUG INITIALIZERS
     *  -functions to setup the debugger for use
     *   mostly implemented in interface.cpp
     */

    /******************************************************************/

    /*setup simulation debugging mode*/
    void initSimDebug(void) {
        setupFactList();
        messageQueue = new std::queue<api::message_type*>();
        rcvMessageList = new std::list<struct msgListContainer*>();
    }

    /*setup MPI debugging mode*/
    void initMpiDebug(void){
        messageQueue = new std::queue<api::message_type*>();
        setupFactList();
        rcvMessageList = new std::list<struct msgListContainer*>();
    }

    /*set up the list to store break points*/
    void setupFactList(void){
        factBreakList = newBreakpointList();
    }

    /*indicate to go into VM debugging mode*/
    void setDebuggingMode(bool setting){
        isDebug = setting;
    }

    /*indicate to go into SIM debugging mode*/
    void setSimDebuggingMode(bool setting){
        isSimDebug = setting;
    }

    /*indicate to go into MPI debugging mode*/
    void setMpiDebuggingMode(bool setting){
        isMpiDebug  = setting;
    }

    void cleanUp(void){
        if (isInSimDebuggingMode()||isInMpiDebuggingMode()){
            delete rcvMessageList;
            delete messageQueue;
        } else if (isInSimDebuggingMode()||isInMpiDebuggingMode()
                   ||isInDebuggingMode()){
            listFree(getFactList());
        }
    }


    /*********************************************************************/

    /*
     *   SYSTEM STATE FUNCTIONS
     *    -return facts about the debugging system
     *       for different modes
     */

    /*********************************************************************/

    /*returns if the VM is paused for debugging or not*/
    bool isTheSystemPaused(void){
        return isSystemPaused;
    }

    /*returns if the system is in SIMULATION debugging mode*/
    bool isInSimDebuggingMode(void){
        return isSimDebug;
    }

    /*returns if system is in VM debugging mode*/
    bool isInDebuggingMode(void){
        return isDebug;
    }

    /*returns if the system is in MPI debugging mode*/
    bool isInMpiDebuggingMode(void){
        return isMpiDebug;
    }

    /*return the pointer the list of breakpoints*/
    debugList getFactList(void){
        return factBreakList;
    }
    
    bool isDebuggerQueueEmpty(void){
		if (messageQueue == NULL) {
			return true;
		}
		return messageQueue->empty();
	}


    /**********************************************************************/

    /*
     *    I/0 SPECIFICATION PARSING
     *     -extract different parts of the
     *      specification messge
     */

    /*********************************************************************/


    /*CHARACTERINSTRINGINDEX--returns the index of a character in a string,
     * if it is not there it returns -1*/
    int characterInStringIndex(string str, char character){
        for(unsigned int i = 0; i < str.length(); i++){
            if (str[i] == character)
                return (int)i;
        }
        return -1;
    }


    /*GETTYPE--extracts the type from the specification from input line
     * returns the type of breakpoint from the specification
     * invariant-- the type is always specified*/
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


    /*GETNAME--extracts the name from the specification
     * returns the name from the specification
     * returns "" if name is not present*/
    string getName(string specification){
        string build = "";
        /*find index of colon*/
        int index = characterInStringIndex(specification, ':');
        /*if colon not there*/
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


    /*GETNODE--extracts the node from the specification
     * returns the node from the specification
     * returns "" if node is not given*/
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

    /*TYPEINT2STRING--given the encoding return the corresponding string for
     * break point types
     * returns "" if not a correct type*/
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



    /*********************************************************************/

    /*
     *   DEBUGGER HANDLING FUNCTIONS
     *    -controlls the debugger
     */

    /*********************************************************************/




    /*ACTIVATEBREAKBOINT--given the specification,
     * turn the breakPoint on by inserting
     * it into the breakpoint list*/
    void activateBreakPoint(string specification){

        ostringstream msg;

        /*to follow a format that a type must be presented first*/
        if (specification[0] == ':'|| specification[0] == '@'){
            display("Please Enter a Type\n",PRINTCONTENT);
            return;
        }

        /*parse for different specification formats*/
        string type = getType(specification);
        string name = getName(specification);
        string nodeID = getNode(specification);

        /*if this type of break point is not valid*/
        if (type!="block"&&type!="action"&&type!="factDer"&&type!="sense"&&
            type!="factCon"&&type!="factRet"){
            display("Please Enter a Valid Type-- type help for options\n"
                    ,PRINTCONTENT);
            return;
        }


        /*create mempory on heap to store break point information*/
        char* type_copy = (char*)malloc(strlen(type.c_str())+1);
        char* name_copy = (char*)malloc(strlen(name.c_str())+1);
        int node_copy;

        /*move the memory over*/
        memcpy(type_copy, (char*)type.c_str(),strlen(type.c_str())+1);
        memcpy(name_copy, (char*)name.c_str(),strlen(name.c_str())+1);

        if (nodeID != "")
            node_copy = atoi(nodeID.c_str());
        else
            node_copy = -1;

        /*insert the information in the breakpoint list*/
        insertBreak(factBreakList,type_copy,name_copy, node_copy);

        msg << "-->Breakpoint set with following conditions:" << endl;
        msg  << "\tType: " << type << endl;
        if (name!="")
            msg << "\tName: " << name << endl;
        if (nodeID!="")
            msg <<  "\tNode: " << nodeID << endl;

#if defined(TARGET_mpi)
        if (isInMpiDebuggingMode()&&api::world->rank() != 1){
            display(msg.str(),PAUSE);
            return;
        }
#endif

        display(msg.str(),PRINTCONTENT);

    }


    /*DISPLAY--divert where the output goes: either cout or over
     * message passing*/
    void display(string msg, int type){

        ostringstream MSG;

        /*if normal- pass on the normal cout*/
        if (isInDebuggingMode())
            cout << msg;

        /*simulation*/
        else if (isInSimDebuggingMode()) {
           MSG << "<=======VM#" <<
                api::getNodeID()
#ifdef SIMD
                << " at time " << vm::determinism::getCurrentLocalTime() 
#endif
                << "===================================================>"
                << endl << msg;
            sendMsg(api::getNodeID(),type,MSG.str());

        /*if is in MPI debugging mode, send to master to display/handle
         *the message*/
        } 
#if defined(TARGET_mpi)
	else {
	  if (isInMpiDebuggingMode()){
            MSG << "<=======VM#" <<
	      api::world->rank()
                << "===================================================>"
                << endl << msg;
            sendMsg(MASTER,type,MSG.str());
	  }
	}
#endif
    }



    /*RUNBREAKPOINT--initiate the system to wait until further notice
     *--> to be inserted in the code of the actual VM
     *    at specific breakpoints
     *-->acts as a filter to check if a breakpoint has been reached or not*/
    void runBreakPoint(char* type, string msg, char* name, int nodeID){

        ostringstream MSG;


        /*if is not in any debugging mode- don't care, keep going*/
        if (!isInDebuggingMode()&&!isInSimDebuggingMode()&&
            !isInMpiDebuggingMode())
            return;

        /*if the specifications are a hit, then pause the system
         *and notify user*/
        if (isInBreakPointList(factBreakList,type,name,nodeID)){
            isPausedAtBreakpoint = true;
            MSG << "Breakpoint-->";
            MSG << type << ":" << name << "@" << nodeID << endl;
            MSG <<  msg;
            display(MSG.str(),BREAKFOUND);
            pauseIt();
        }
    }


    /*PAUSE IT--pause the VM until further notice
     * if in Sim or MPI debugging mode check for messages
     * to tell it what to do*/
    void pauseIt(void){

        isSystemPaused = true;
        while(isSystemPaused) {
            /*if is in MPI mode, recieve messages*/
            /*will breakout of loop if CONTINUE message is
             * specified which is handled by debugController*/
            if (isInMpiDebuggingMode()){
                    api::debugWaitMsg();
                    receiveMsg();
            } else if (isInSimDebuggingMode()){
					if (messageQueue->empty()) {
						api::debugWaitMsg();
					}
					receiveMsg(false);
            /*for normal debugging mode*/
            } else {
                sleep(1);
            }
        }
    }

    /*SERIALIZED PAUSE -- to be impemented in sched/base.cpp
     * allows only one process to execute at a time, else
     * it will wait for the previous process to let it know
     * continue */
    void serializedPause(void){
        ostringstream spec;
        spec << api::nextProcessForDebugger();

        if (hasTheConche){

            /*tell master to let next process execute*/
            sendMsg(MASTER, CONCHE,spec.str());
            hasTheConche = false;

            /*wait till it gets the conche again*/
            while(!hasTheConche&&serializationMode){
                receiveMsg();
            }

        }
    }



    /*DUMP SYSTEM STATE--display the contents of VM
     * by calling VM database functions */
    void dumpSystemState(int nodeNumber){

        ostringstream msg;

        /*if a node is not specified by the dump command*/
        if (nodeNumber == -1)
	  vm::All->DATABASE->print_entire_db_debug(msg);
        else
            /*print out only the given node*/
	  vm::All->DATABASE->print_db_debug(msg,nodeNumber);

        display(msg.str(),PRINTCONTENT);
    }


    /*CONTINUE EXECUTION--resume a paused system*/
    void continueExecution(){
        /*setting this will break it out of a while loop
         *from pauseIt function*/
        isSystemPaused = false;
    }



    /*SETFLAGS -- parse input for different debugging mode flags.
     * the order of the flags does not matter
     * if a valid flag, turn mode on and notify user*/
    void setFlags(string specification){
        ostringstream msg;
        for (int i = 0; (uint)i < specification.length(); i++){

            if ((uint)specification[i] == 'V'){

                if (!verboseMode)
                    msg << "-verbose mode set" << endl;
                else
                    msg << "-already in verbode mode" << endl;
                verboseMode = true;


            } else if ((uint)specification[i] == 'S'){

                if (!isInSimDebuggingMode()){
                    if (!serializationMode)
                        msg << "-serialization mode set" << endl;
                    else
                        msg << "-already in serialization mode" << endl;
                    serializationMode = true;
                } else {
                    /*no serialization in simulation debugging*/
                    msg << "-cannot go into serialization mode" << endl;
                }
#if defined(TARGET_mpi)
                if (isInMpiDebuggingMode()&&api::world->rank() == 1) {
		  //let begining process execute in serialization
		  if (okayToSetConche){
		    /*only able to set conche once*/
		    hasTheConche = true;
		    okayToSetConche = false;
		  }
		}
#endif
            }
        }

#if defined(TARGET_mpi)
        if ((isInMpiDebuggingMode()&&api::world->rank()!=MASTER)
	    ||isInSimDebuggingMode())
            display(msg.str(),PRINTCONTENT);
#endif
    }

    /*DEBUGCONTROLLER -- main controller of pausing/unpausing/dumping VMs*/
    /* execute instruction based on encoding and specification
     * call from the debug_prompt -- There are two different sides to
     * this function:  There is one side that handles sending messages to
     * processes in which these process will recieve that message
     * The other side pertains to the processes that are controlled by the
     * the master process.  They will change their system state and give
     * feed back to the master process (see debugger::display())
     * When the master sends a message, it will expect to see a certain
     * amount of messages sent back*/
    void debugController(int instruction, string specification){

        string type;
        string name;
        string node;

        /*for use of numberExpected see debug_prompt.cpp, run()*/

#if defined(TARGET_mpi)
        /*if MPI debugging and the master process (process zero):
         *send a  message instead of changing the system state
         *as normally done in normal debugging*/
        if (isInMpiDebuggingMode() && api::world->rank()==MASTER){

            /*process of master debugger in MPI DEBUGGINGMODE*/
            if (instruction == CONTINUE || instruction == UNPAUSE){

                okayToBroadcastPause = true;
                /*continue a paused system by broadcasting an UNPAUSE signal*/
                sendMsg(-1,CONTINUE,"",BROADCAST);
                numberExpected = api::world->size()-1;

            } else if (instruction == RUN) {

                /*same as continue -- reserved if funcionality need to be changed*/
                okayToBroadcastPause = true;
                sendMsg(-1,RUN,"",BROADCAST);
                numberExpected = api::world->size()-1;

            } else if (instruction == MODE) {

                setFlags(specification);
                sendMsg(-1,MODE,specification,BROADCAST);
                numberExpected = api::world->size()-1;

            } else if (instruction == DUMP) {

                /*broadcast the message to all VMs*/
                if (specification == "all"){

                    sendMsg(-1,DUMP,specification,BROADCAST);
                    /*wait for all VMs to receive (not counting the debugger
                     *itself*/
                    numberExpected = (int)api::world->size()-1;

                } else {

                    /*send to a specific VM to dump content*/
                    sendMsg(atoi(specification.c_str()),DUMP,specification);
                    numberExpected = 1;
                }

            /*handle the breakpoints in the lists*/
            } else if (instruction == REMOVE||instruction == BREAKPOINT) {

                /*extract the destination*/
                node = getNode(specification);
                if (node == ""){

                    /*broadcast the message if the node is not specified*/
                    sendMsg(-1,instruction,specification,BROADCAST);
                    numberExpected = (int)api::world->size()-1;

                } else {

                    /*send break/remove to a specific node */
                    sendMsg(atoi(node.c_str()),instruction,specification);
                    numberExpected = 1;

                }


            } else if (instruction == PRINTLIST) {

                /*broadcast  a print message*/
                sendMsg(-1,PRINTLIST,"",BROADCAST);
                numberExpected = (int)api::world->size()-1;

            }


        /*this section pertains to the slave VMs that respond to the
         *master debugging process-- also run in normal execution of
         *a single VM (no MPI debugging mode, but normal debugging mode)
         * also run by VMs if in Sim debugging mode*/
        } 
	else 
#endif
	  {

            switch(instruction){

                case DUMP:
                    if (specification == "all"){
                        if (isInSimDebuggingMode()) {
                            dumpSystemState(api::getNodeID());
                        } else {
                            dumpSystemState(-1);
                        }
                    } else {
                        dumpSystemState(atoi(specification.c_str()));
                    }
                    break;
                case PAUSE:

                    /*sychronization conditions -- if already paused
                     *don't do anything, the master already knows
                     *they are paused*/
                    if (isPausedInWorkLoop)
                        break;
                    if (isPausedAtBreakpoint){
                        break;
                    }
                    if (isPausedInDeterministicPollLoop) {
						break;
					}
                    /*if it never left a pause loop (continue would have been called
                     * and pause message repaused it without leaving loop)*/
                    if (isSystemPaused){
                        display("CURRENTLY PAUSED\n",PAUSE);
                        break;
                    }

                    /*let the master know that child is not being executed*/
                    if (serializationMode&&!hasTheConche){
                        display("PAUSED DUE TO SERIALIZATION\n",PAUSE);
                        break;
                    }

                    /*pause it otherwise*/
                    isSystemPaused = true;
                    break;
                case UNPAUSE:
                case CONTINUE:

                    /*unleash all bounds to system*/
                    isPausedAtBreakpoint = false;
                    isPausedInWorkLoop = false;
                    isPausedInDeterministicPollLoop = false;
                    continueExecution();
                    break;

                case REMOVE:

                    /*remove breakpoint from list*/
                    type = getType(specification);
                    name = getName(specification);
                    node = getNode(specification);
                    if (removeBreakPoint(getFactList(),(char*)type.c_str(),
                                         (char *)name.c_str(),
                                         atoi(node.c_str())) < 0){
                        display("Breakpoint is not in List\n",PRINTCONTENT);
                    } else {
                        display("Breakpoint removed\n",PRINTCONTENT);
                    }
                    break;

                case BREAKPOINT:

                    activateBreakPoint(specification);
                    break;

                case TERMINATE:

                    /*if quit command was specified*/
                    if (isInMpiDebuggingMode())
                        api::end();
                    cleanUp();
                    exit(0);
                    break;

                case PRINTLIST:
                {
                    /*print the debugging breakpoint list*/
                    std::ostringstream printListMsg;
                    printList(printListMsg,getFactList());
                    display(printListMsg.str(),PRINTCONTENT);
                }
                    break;
                case RUN:

                    /*similar to continue--set aside if
                     *want to run a certain way*/
                    isPausedAtBreakpoint = false;
                    isPausedInWorkLoop = false;
                    isPausedInDeterministicPollLoop = false;
                    continueExecution();
                    break;

                case MODE:

                    /*turn on different modes*/
                    setFlags(specification);
                    break;

#if defined(TARGET_mpi)
                case CONCHE:

                    /*if match for next conche, retrieve it*/
                    if (atoi(specification.c_str()) == api::world->rank()){
                        hasTheConche = true;
                    }
                    break;
#endif

                case ENDSER:
                    serializationMode = false;
                    break;

            }
        }
    }


    /*DEBUGMASTERCONTROLLER-the controller for
     * the master MPI debugger-called when messages are
     * received*/
    void debugMasterController(int instruction, string specification){

        /*print the output and then tell all other VMs to pause*/
        if (instruction == BREAKFOUND){

            printf("%s",specification.c_str());
            /*allows for only one pause massage to be sent
             *per CONTINUE instruction*/
            if (okayToBroadcastPause){
                sendMsg(-1,PAUSE,"",BROADCAST);
                okayToBroadcastPause = false;
            }

        } else if (instruction == PRINTCONTENT){

            printf("%s",specification.c_str());

        } else if (instruction == TERMINATE){

            printf("PROGRAM FINISHED\n");
            if (serializationMode)
                sendMsg(-1,ENDSER,"",BROADCAST);
            api::end();
            cleanUp();
            exit(0);

        } else if (instruction == PAUSE){

            /*prints more information*/
            if (verboseMode){
                printf("%s",specification.c_str());
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


    /*GET MAX CHAR ARRAY SIZE--return the size of the maximum char array size*/
    inline int getMaxCharArraySize(){

        /*will always return an integer*/
        return api::MAXLENGTH*SIZE -
            (sizeof(int)+4*sizeof(api::message_type)+sizeof(size_t)
             +sizeof(int)+8);
    }


    /*PACK LIST --break message into parts while
     *it is longer than the buffer size and store
     * it in a list to be ready to be sent.
     * Attatch a header to know how many messages the
     * list contains including the header*/
    std::list<api::message_type*>* packList(int msgEncode, string content){

        std::list<api::message_type*>* msgList = new std::list<api::message_type*>();

        api::message_type* msg;

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
    api::message_type* pack(int msgEncode, string content, int priority){


        utils::byte* msg = (utils::byte*)new api::message_type[api::MAXLENGTH];
        int pos = 0;


        api::message_type debugFlag =  DEBUG;
        size_t contentSize = content.length() + 1;
        size_t bufSize = api::MAXLENGTH*SIZE;//bytes
        api::message_type msgSize = bufSize-SIZE;//according to message spec
#ifdef SIMD
		api::message_type timeStamp = vm::determinism::getCurrentLocalTime();
#else 
		api::message_type timeStamp = 0;
#endif
        api::message_type nodeId = api::getNodeID();

        /*message size in bytes*/
        utils::pack<api::message_type>(&msgSize,1,msg,bufSize,&pos);

        /*debug indicator*/
        utils::pack<api::message_type>(&debugFlag,1,msg,bufSize,&pos);

        /*timestamp*/
        utils::pack<api::message_type>(&timeStamp,1,msg,bufSize,&pos);

        /*VM id*/
        utils::pack<api::message_type>(&nodeId,1,msg,bufSize,&pos);

        /*priority of the message*/
        utils::pack<int>(&priority,1,msg,bufSize,&pos);

        /*debug command encoding*/
        utils::pack<int>(&msgEncode,1,msg,bufSize,&pos);

        /*size of content*/
        utils::pack<size_t>(&contentSize,1,msg,bufSize,&pos);

        /*content*/
        utils::pack<char>((char*)content.c_str(),content.size()+1,
                                 msg,bufSize,&pos);

        return (api::message_type*)msg;

    }



    /*SENDMSG--the desination specified is the user input node ID*/
    /* send a debug message to another process through the API layer*/
    /* if send to all, specify BROADCAST (see top)*/
    void sendMsg(int destination, int msgType,
              string content, bool broadcast)  {

         /*length of array*/
            size_t msgSize = api::MAXLENGTH;
            /*pack the message into a broken list*/
            std::list<api::message_type*>* msgList = packList(msgType,content);
            api::message_type* msg;

            /*send all the messages in the list*/
            while (!msgList->empty()){

                msg = msgList->front();
                msgList->pop_front();



                if (broadcast){

                    /*send to all*/
                    api::debugBroadcastMsg(msg,msgSize);

                } else {

                    /*send to the master debugging process*/
                    if (destination == MASTER){
                        api::debugSendMsg(MASTER,msg,
                                          msgSize);
                        continue;
                    }

                    /*send the message through api layer*/
                    api::debugSendMsg(destination,msg,
                                      msgSize);
                }

            }

            delete msgList;

    }


    /*RECIEVEMSG--populate the queue of messages
     * from the mpi layer and handle them
     * until there are no more messages*/
    void receiveMsg(bool poll){

        utils::byte *msg;
        int instruction;
        char specification[api::MAXLENGTH*SIZE];
        int pos = 0;
        api::message_type size,debugFlag,timeStamp,NodeId;
        size_t specSize;
        int priority;

        struct msgListContainer* msgContainer;

		if (poll) {
			/*load the message queue with messages*/
			api::debugGetMsgs();
		}

        while(!messageQueue->empty()){
            /*process each message until empty*/
            /*extract the message*/
            msg = (utils::byte*)messageQueue->front();

            /*==>UNPACK the message into readable form*/
            /*msgsize in bytes*/
            utils::unpack<api::message_type>(msg,api::MAXLENGTH*SIZE,
                                             &pos,&size,1);
            /*debugFlag*/
            utils::unpack<api::message_type>(msg,api::MAXLENGTH*SIZE,
                                             &pos,&debugFlag,1);
            /*timestamp*/
            utils::unpack<api::message_type>(msg,api::MAXLENGTH*SIZE,
                                             &pos,&timeStamp,1);
            /*place msg came from*/
            utils::unpack<api::message_type>(msg,api::MAXLENGTH*SIZE,
                                             &pos,&NodeId,1);
            /*priority*/
            utils::unpack<int>(msg,api::MAXLENGTH*SIZE,
                                       &pos,&priority,1);
            /*command encoding*/
            utils::unpack<int>(msg,api::MAXLENGTH*SIZE,&pos,&instruction,1);
            /*content size*/
            utils::unpack<size_t>(msg,api::MAXLENGTH*SIZE,&pos,&specSize,1);
            /*content*/
            utils::unpack<char>(msg,api::MAXLENGTH*SIZE,&pos,
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
                memset(specification,0,api::MAXLENGTH*SIZE);
                messageQueue->pop();
                //memset(msg,0,api::MAXLENGTH*SIZE);
                delete[] msg;
                pos = 0;
                continue;
            }

#ifdef TARGET_mpi
            /*if the controlling process is recieving a message*/
            if (isInMpiDebuggingMode()&&api::world->rank()==MASTER){

                /*broadcast new conche owner specified by old conche owner*/
                if (instruction == CONCHE){

                    sendMsg(-1,CONCHE,spec,BROADCAST);

                /*do normal message handling*/
                } else {
                    debugMasterController(instruction,spec);
                    numberExpected--;
                }

            /*if a slave process (any vm) is receiving the message*/
            } else 
#endif
	      {

                debugController(instruction,spec);
            }

            /*set up the variables and buffers for next message*/
            memset(specification,0,api::MAXLENGTH*SIZE);
            messageQueue->pop();
            //memset(msg,0,api::MAXLENGTH*SIZE);
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



}//namespace debugger














