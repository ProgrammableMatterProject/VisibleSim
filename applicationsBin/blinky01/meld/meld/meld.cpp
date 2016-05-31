#include <iostream>
#include <vector>

#include "process/machine.hpp"
#include "utils/utils.hpp"
#include "utils/fs.hpp"
#include "vm/state.hpp"
#include "debug/debug_handler.hpp"
#include "debug/debug_prompt.hpp"
#include "interface.hpp"
#include "compileInfo.hpp"

using namespace utils;
using namespace process;
using namespace std;
using namespace sched;

static char *program = NULL;
static char *data_file = NULL;
static char *progname = NULL;
static char checkedApiTarget = 0; // if set to 1, can exit successfully with zero args

namespace xutils {
  static char* compileInfo = NULL;

  char*
  addCompileInfo(char const* info)
  {
    static int ciLen = 0;
    int iLen = strlen(info);
    
    compileInfo = (char *)realloc(compileInfo, ciLen + 2 + iLen);
    if (ciLen > 0) compileInfo[ciLen++] = ' ';
    strcpy(compileInfo+ciLen, info);
    ciLen += iLen;
    return compileInfo;
  }
}

static void
help(void)
{
  cerr << "meld: execute meld program for " << (xutils::compileInfo?xutils::compileInfo:"??") << endl;
	cerr << "meld -f <program file> -c <scheduler> [options] -- arg1 arg2 ... argN" << endl;
	cerr << "\t-f <name>\tmeld program" << endl;
	cerr << "\t-a\tprint on stdout the target api" << endl;
	cerr << "\t-a <name>\tmake sure <name> is proper target" << endl;
   cerr << "\t-r <data file>\tdata file for meld program" << endl;
	help_schedulers();
	cerr << "\t-t \t\ttime execution" << endl;
	cerr << "\t-m \t\tmemory statistics" << endl;
	cerr << "\t-i <file>\tdump time statistics" << endl;
	cerr << "\t-s \t\tshows database" << endl;
   cerr << "\t-d \t\tdump database (debug option)" << endl;
   cerr << "\t-h \t\tshow this screen" << endl;
   cerr << "\t-D MPI|SIM \t\tgo into debugging mode" << endl;


   exit(EXIT_SUCCESS);
}

static vm::machine_arguments
read_arguments(int argc, char **argv)
{
	vm::machine_arguments program_arguments;

   progname = *argv++;
   --argc;

   while (argc > 0 && (argv[0][0] == '-')) {
      switch(argv[0][1]) {
		 case 'f': {
            if (program != NULL || argc < 2)
               help();

            program = argv[1];

            argc--;
            argv++;
         }
         break;
      case 'a': {
	if (argc > 1) {
	  // check that supplied arg matches apiTarget
	  if (strcmp(argv[1], api::apiTarget)) {
	    cerr << "Mismatch of request " << argv[1] << " and compiled for " << api::apiTarget << endl;
	    exit(EXIT_FAILURE);
	  }  else {
	    // we are good, you can continue
	    argc--;
	    argv++;
	    checkedApiTarget = 1;
	  }
	} else {
	    cout << api::apiTarget << endl;
	    exit(EXIT_SUCCESS);
	  }
      }
	break;

         case 'r': {
            if(data_file != NULL || argc < 2)
               help();

            data_file = argv[1];

            argc--;
            argv++;
         }
         break;
         case 'c': {
            if (sched_type != SCHED_UNKNOWN)
               help();

            parse_sched(argv[1]);
            argc--;
            argv++;
         }
         break;
         case 's':
            show_database = true;
            break;
         case 'd':
            dump_database = true;
            break;
         case 't':
            time_execution = true;
            break;
         case 'm':
            memory_statistics = true;
            break;
         case 'i':
            if(argc < 2)
               help();

            statistics::set_stat_file(string(argv[1]));
            argc--;
            argv++;
            break;
         case 'h':
            help();
            break;
         case 'D':
	   if (string(argv[1]) == "VM"){
	     cout << "DEBUGGING MODE -- type 'help' for options" << endl;
	     debugger::setDebuggingMode(true);
	   } else if (string(argv[1]) == "MPI"){
           debugger::setMpiDebuggingMode(true);
	   } else if (string(argv[1]) == "SIM"){
	     debugger::setSimDebuggingMode(true);
	   } else {
	     cout << "Unknow debug option" << endl;
	     exit(EXIT_FAILURE);
	   }
	   break;


			case '-':

				for(--argc, ++argv ; argc > 0; --argc, ++argv)
					program_arguments.push_back(string(*argv));
			break;
         default:
            help();
      }

      /* advance */
      argc--; argv++;
   }

	return program_arguments;
}

int
main(int argc, char **argv)
{
   vm::machine_arguments margs(read_arguments(argc, argv));

   if(sched_type == SCHED_UNKNOWN) {
      sched_type = SCHED_SERIAL;
      num_threads = 1;
   }

   if ((program == NULL) && (sched_type != SCHED_UNKNOWN)) {
    if (checkedApiTarget) return EXIT_SUCCESS;

     cerr << "Error: please provide a program to run" << endl;
     return EXIT_FAILURE;
   }

	if(!file_exists(program)) {
		cerr << "Error: file " << program << " does not exist or is not readable" << endl;
		return EXIT_FAILURE;
	}

   try {
      run_program(argc, argv, program, margs, data_file);
   } catch(vm::load_file_error& err) {
      cerr << "File error: " << err.what() << endl;
      exit(EXIT_FAILURE);
   } catch(machine_error& err) {
      cerr << "VM error: " << err.what() << endl;
      exit(EXIT_FAILURE);
   } catch(db::database_error& err) {
      cerr << "Database error: " << err.what() << endl;
      exit(EXIT_FAILURE);
   }

   return EXIT_SUCCESS;
}


// Local Variables:
// mode: C++
// indent-tabs-mode: nil
// End:
