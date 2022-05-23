#ifndef UTILS_H__
#define UTILS_H__

#include <iostream>
#include <vector>
#include <algorithm>
#include <iomanip>
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

/**
 * @brief Custom modulus fonction, python style e.g. -2 % 6 = 4
 * @param l left operand
 * @param mod right operand, modulus value
 * @return python style modulus operation result
 */
        int m_mod(int l, int mod);

/**
 * Pause the current thread until a key is pressed
 */
        void awaitKeyPressed();

/**
 * @brief Wrapper function for Scheduler::toggle_pause
 * Pauses the scheduler and processing of events, or resumes it if scheduler state
 *  was PAUSED
 * @warning only works in Realtime mode scheduler ('r').
 * @note can be triggered from the GUI using the <SPACE> key
 */
        void toggleSchedulerPause();

/**
 * For a given triggered assert, display its location and freeze current simulation until user provides an input
 * @return always true
 */
        bool assert_handler(bool cond, const char *file,
                            const int line, const char* func,
                            const char *msg = NULL);

/**
 * For a given triggered assert, displays the stack trace of the current thread
 * @return always true
 */
        bool assert_stack_print();

#ifndef WIN32 // not available in windows context
        // This function produces a stack backtrace with demangled function & method names.
std::string Backtrace(int skip);
#endif

/**
 * Custom assertion macro used for debugging, it shows a message
 *  on stderr, before pausing the simulation until a key is pressed
 *  (for simulation environment analysis in the GUI), then triggers
 *  a standard C assert
 * @param cond assertion condition
 */
#define VS_ASSERT(cond) \
    ((void)(!(cond) && assert_handler(cond, __FILE__, __LINE__, __PRETTY_FUNCTION__)))

#define VS_ASSERT_MSG(cond, msg) \
    ((void)(!(cond) && assert_handler(cond, __FILE__, __LINE__, __PRETTY_FUNCTION__, #msg)))

//!< @brief Return true if a <= x <= b, false otherwise
        inline bool isInRange(int x, int a, int b) { return (a <= x && x <= b); }

        const float M_SQRT2_2 = sqrt(2.0) / 2.0; //!< $\frac{\sqrt{2}}{2}$
        const double M_SQRT3_2 = sqrt(3.0) / 2.0; //!< $\frac{\sqrt{3}}{2}$
        const double EPS = 1E-5;                  //!< Epsilon

//!< @brief Generates a formatted filename string
//!< @param prefix e.g, config
//!< @param ext e.g, xml
//!< @param includeDate if enabled format = <prefix>_ddmmyyyy__hh_mm_ss.<ext>
//!< @return a string with format <prefix>_hh_mm_ss.<ext>
        const std::string
        generateTimestampedFilename(const std::string& prefix, const std::string& ext,
                                    bool includeDate = false);

//!< @brief Generates a formatted directory name string
//!< @param dirBasename e.g, dir
//!< @return a string with format <dirBasename>_hh_mm_ss
        const std::string
        generateTimestampedDirName(const std::string& dirBasename);

//!< Return true if file at path fileName exists and can be read, false otherwise
        bool file_exists(const std::string fileName);

//!< Swaps the value of two pointers a and b
        void swap(int* a, int* b);

//!< https://stackoverflow.com/questions/19483663/vector-intersection-in-c
        template<typename T>
        std::vector<T> intersection(std::vector<T> &v1, std::vector<T> &v2) {
            std::vector<T> v3;

            std::sort(v1.begin(), v1.end());
            std::sort(v2.begin(), v2.end());

            std::set_intersection(v1.begin(),v1.end(),v2.begin(),v2.end(),std::back_inserter(v3));

            return v3;
        }

        template< typename ContainerT, typename PredicateT >
        void erase_if( ContainerT& items, const PredicateT& predicate ) {
            for( auto it = items.begin(); it != items.end(); ) {
                if( predicate(*it) ) it = items.erase(it);
                else ++it;
            }
        }

        template< typename T >
        std::string int_to_hex_str( T i, int width)
        {
            std::stringstream stream;
            stream << "0x" << std::setfill('0') << std::setw(width) << std::hex << i;
            return stream.str();
        }

/**
 * @brief trims a file name from a path
 * @param path
 * @return filename of the file at path
 */
        char *myBasename(char const *path);
        std::string myBasename(const std::string& path);

        inline void toLowercase(std::string& str) {
            transform(str.begin(), str.end(), str.begin(), ::tolower);
        }

        inline void toUppercase(std::string& str) {
            transform(str.begin(), str.end(), str.begin(), ::toupper);
        }

    } // namespace utils

} // namespace BaseSimulator

#endif
