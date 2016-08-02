
/* Interface for debugging- spawns a  prompt that will controll the main thread if it hits a specifies break point*/

#include <pthread.h>
#include <iostream>
#include <string>
#include <stdio.h>
#include <stdlib.h>
#include "debug/debug_handler.hpp"
#include "debug/debug_prompt.hpp"

using namespace std;

namespace debugger {

  /*to store the last input in the debugger*/
  int lastInstruction = 0;
  string lastBuild = "";


  /*spawn the debbugging prompt as a separate thread to
    controll the main one*/
  void debug(void) {

    //start the list of break points to be used
    setupFactList();

    pthread_t tid;
    pthread_create(&tid,NULL,run, NULL);

  }


  //continuously attend command line prompt for debugger
  //when the system is not paused
  void *run(void*x) {
    ignoreUnusedParamWarning(x);

    string inpt;

    debugList factBreaks = getFactList();

    bool expectingMessage;

    while(true){
      if (isTheSystemPaused()&&numberExpected==0){
        cout << ">";
        getline(cin,inpt);
        //react to the input
        expectingMessage = parseline(inpt,factBreaks);
      }

      if (isInMpiDebuggingMode()&&expectingMessage){
          api::debugWaitMsg();
          receiveMsg();
      }

    }
    return NULL;
  }



  /*parses the command line and run the debugger*/
  bool parseline(string line, debugList& factBreaks){

    string build = "";
    int wordCount = 1;

    int command = NOTHING;

    /*empty input*/
    if (line == ""){
      //enterlast stored command
      debugController(lastInstruction, lastBuild);
      return true;
    }

    /*loop through input line*/
    for (unsigned int i = 0; i < line.length(); i++){


      /*parse line for words*/
      if (line[i]!=' '){
        build += line[i];
      } else {
        //exract the command
        if (wordCount == 1){
          command = handle_command(build,factBreaks);
          if (command==NOTHING){
              return false;
          }
        wordCount++;
        build = "";
        }
      }
    }


    /*no whitespace at all-single word commands*/
    if (wordCount == 1){
      command = handle_command(build,factBreaks);

      if (command==NOTHING){
              return false;
          }

      if (command != BREAKPOINT && command!=DUMP
          && command != REMOVE && command != MODE){
        debugController(command, build);
        lastInstruction = command;
        lastBuild = build;
        return true;
      }
    }

    /*if not enough info - these  types must have a specification*/
    if ((command == BREAKPOINT||command == DUMP||command == REMOVE||
         command == MODE)&&wordCount == 1){
      cout << "Please specify- type help for options" << endl;
      return false;
    }

    /*handle breakpointsand dumps*/
    if (wordCount == 2){
        if (command == BREAKPOINT||command == DUMP||command == REMOVE||
            command == MODE){
        debugController(command,build);
        } else
        debugController(command,"");
      lastInstruction = command;
      lastBuild = build;
      return true;
    }
    return false;

  }

  /*recognizes and sets different modes for the debugger*/
  int handle_command(string command, debugList& factList){

      (void) factList;
    int retVal;

    if (command == "break"){
        retVal = BREAKPOINT;
    } else if (command == "help"||command == "h") {
        help();
        retVal = NOTHING;
    } else if (command == "run"|| command == "r") {
        retVal = RUN;
    } else if (command == "dump"||command == "d") {
        retVal = DUMP;
    } else if (command == "print" || command == "p"){
        retVal = PRINTLIST;
    } else if (command == "remove" || command == "rm"){
        retVal = REMOVE;
    } else if (command == "continue"||command == "c"){
        retVal = CONTINUE;
    } else if (command == "mode"||command == "m"){
        retVal = MODE;
    } else if (command == "quit"||command == "q"){
        sendMsg(-1,TERMINATE,"",true);
        api::end();
        delete messageQueue;
        exit(0);
    } else {
        cout << "unknown command: type 'help' for options " << endl;
        retVal = NOTHING;
    }
    return retVal;
  }


  /*prints the help screen*/
  void help(){
    cout << endl;
    cout << "*******************************************************************" << endl;
    cout << endl;
    cout << "DEBUGGER HELP" << endl;
    cout << "\t-break <Specification>- set break point at specified place" << endl;
    cout << "\t-rm <Specification> - remove a breakpoint at specified place" << endl;
    cout << "\t\t-Specification Format:" << endl;
    cout << "\t\t  <type>:<name>@<node> OR" << endl;
    cout << "\t\t  <type>:<name>        OR" << endl;
    cout << "\t\t  <type>@<node>" << endl;
    cout << "\t\t    -type - [factRet|factDer|factCon|action|sense]" << endl;
    cout << "\t\t\t-a type MUST be specified" << endl;
    cout << "\t\t    -name - the name of certain type ex. the name of a fact" << endl;
    cout << "\t\t    -node - the number of the node" << endl;
    cout << "\t-dump or d <nodeID> <all> - dump the state of the system" << endl;

    cout << "\t-mode <parameter> - change the debugger mode" << endl;
    cout << "\t\t - <parameter> - V - verbose mode" << endl;
    cout << "\t\t                 S - serialized mode" << endl;
    cout << "\t-print - print the breakpoint list" << endl;
    cout << "\t-continue or c - continue execution" << endl;
    cout << "\t-run or r - start the program" << endl;
    cout << "\t-quit - exit debugger" << endl;
    cout << endl;
    cout << "\t-Press Enter to use last Input" << endl;
    cout << endl;
    cout << "*******************************************************************" << endl;
  }
}
