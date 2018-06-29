
#ifndef UTILS_H__
#define UTILS_H__

#include <iostream>
#include <fstream>
#include <ctime>
#include <sstream>

#include "tDefs.h"

namespace BaseSimulator {

//!< utils Utilities namespace for providing globally needed constants, types, and helper methods
namespace utils {

#ifndef M_PI
#define M_PI	3.1415926535897932384626433832795 //!< 31-digit pi constant
#endif

#define IS_ODD(x) ((x>0?x:-x) % 2)     //!< returns 1 if x is odd, 0 otherwise
#define IS_EVEN(x) (!IS_ODD(x)) //!< returns 1 if x is even, 0 otherwise

inline static void awaitKeyPressed() {
    std::cout << "Press ENTER to continue..." << std::endl; std::cin.ignore();
}

//!< @brief Return true if a <= x <= b, false otherwise
inline static bool isInRange(int x, int a, int b) { return (a <= x && x <= b); };

const float M_SQRT2_2 = sqrt(2.0) / 2.0; //!< $\frac{\sqrt{2}}{2}$
const double M_SQRT3_2 = sqrt(3.0) / 2.0; //!< $\frac{\sqrt{3}}{2}$
const double EPS = 1E-5;                  //!< Epsilon

//!< @brief Generates a formatted filename string
//!< @param prefix e.g, config
//!< @param ext e.g, xml
//!< @return a string with format <prefix>_hh_mm_ss.<ext>
inline static const std::string
generateTimestampedFilename(const std::string& prefix, const std::string& ext) {
    std::ostringstream out;

    time_t now = time(0);
    tm *ltm = localtime(&now);
    
    out << prefix << "_" << ltm->tm_hour << "_"
        << ltm->tm_min << "_" << ltm->tm_sec << "." << ext;
    
    return out.str();
}

//!< @brief Generates a formatted directory name string
//!< @param dirBasename e.g, dir
//!< @return a string with format <dirBasename>_hh_mm_ss
inline static const std::string
generateTimestampedDirName(const std::string& dirBasename) {
    std::ostringstream out;

    time_t now = time(0);
    tm *ltm = localtime(&now);
    
    out << dirBasename << "_" << ltm->tm_hour << "_"
        << ltm->tm_min << "_" << ltm->tm_sec;
    
    return out.str();
}

//!< Return true if file at path fileName exists and can be read, false otherwise
inline static bool file_exists(const std::string fileName) {
    std::ifstream infile(fileName);
    return infile.good();
}

//!< Swaps the value of two pointers a and b
inline static void swap(int* a, int* b) {
	int temp = *a;
	*a = *b;
	*b = temp;
}

template< typename ContainerT, typename PredicateT >
     void erase_if( ContainerT& items, const PredicateT& predicate ) {
     for( auto it = items.begin(); it != items.end(); ) {
          if( predicate(*it) ) it = items.erase(it);
          else ++it;
     }
};
 
} // namespace utils

} // namespace BaseSimulator

#endif
