/**
 * @file   utils.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Fri Aug 10 18:07:23 2018
 * 
 * @brief  Utils functions implementation, with utils.h as a standalone all functions need to be inline and any tweaking to the utils forces the recompilation of the whole project
 * 
 * 
 */


#include "utils.h"

#include <fstream>
#include <ctime>

using namespace BaseSimulator;
using namespace utils;
using namespace std;

int utils::m_mod(int l, int mod) {
    VS_ASSERT_MSG(mod != 0, "Modulus cannot be 0!");
    return l < 0 and l % mod != 0 ? mod - (-l % mod) : l % mod;
}

bool utils::assert_handler(bool cond, const char *file, const int line,
                    const char *func, const char* msg) {
    if (msg) std::cerr << msg << endl;
    
    std::cerr << "assert triggered in fonction " << func
              << " at " << file << ":" << line << std::endl;
    awaitKeyPressed();

    exit(1);
    
    return true;
}

void utils::awaitKeyPressed() {
    std::cout << "Press any key to continue..." << std::endl; std::cin.ignore();
}

const std::string
utils::generateTimestampedFilename(const std::string& prefix, const std::string& ext) {
    std::ostringstream out;

    time_t now = time(0);
    tm *ltm = localtime(&now);
    
    out << prefix << "_" << ltm->tm_hour << "_"
        << ltm->tm_min << "_" << ltm->tm_sec << "." << ext;
    
    return out.str();
}

const std::string
utils::generateTimestampedDirName(const std::string& dirBasename) {
    std::ostringstream out;

    time_t now = time(0);
    tm *ltm = localtime(&now);
    
    out << dirBasename << "_" << ltm->tm_hour << "_"
        << ltm->tm_min << "_" << ltm->tm_sec;
    
    return out.str();
}

bool utils::file_exists(const std::string fileName) {
    std::ifstream infile(fileName);
    return infile.good();
}

void utils::swap(int* a, int* b) {
	int temp = *a;
	*a = *b;
	*b = temp;
}
