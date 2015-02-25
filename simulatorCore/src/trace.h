/*
 * trace.h
 *
 *  Created on: June 22, 2013
 *      Author: Andre
 */

#ifndef TRACE_H_
#define TRACE_H_

#include <fstream>

#define LOGFILE

#ifdef LOGFILE
	extern std::ofstream log_file;
	#define OUTPUT log_file
	#define ERRPUT log_file
#else
	#define OUTPUT cout
	#define ERRPUT cerr
#endif

#endif
