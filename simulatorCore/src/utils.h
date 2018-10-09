
#ifndef UTILS_H__
#define UTILS_H__

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
    cout << "Press ENTER to continue..." << endl; cin.ignore();
}

//!< @brief Return true if a <= x <= b, false otherwise
inline static bool isInRange(int x, int a, int b) { return (a <= x && x <= b); };

const float M_SQRT2_2 = sqrt(2.0) / 2.0; //!< $\frac{\sqrt{2}}{2}$
const double M_SQRT3_2 = sqrt(3.0) / 2.0; //!< $\frac{\sqrt{3}}{2}$
const double EPS = 1E-5;                  //!< Epsilon

//!< Return true if file at path fileName exists and can be read, false otherwise
inline static bool file_exists(const string fileName) {
    std::ifstream infile(fileName);
    return infile.good();
}

//!< An exception for marking functions as not implemented
struct NotImplementedException : std::exception {
    const char* what() const noexcept { return "not yet implemented!\n"; }
};

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

class Cell3DPosition;
namespace Catoms3D {

// Utility functions that return cell in direction d of a given Catoms3D Position
typedef Cell3DPosition (*conProj)(const Cell3DPosition&);
static const conProj CONPROJECTOR[12] {
    [] (const Cell3DPosition &p) { return Cell3DPosition(p[0]+1, p[1], p[2]); },
           
       [] (const Cell3DPosition &p) { return Cell3DPosition(p[0], p[1]+1, p[2]); },
               
       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0], p[1], p[2]+1):Cell3DPosition(p[0]+1, p[1]+1, p[2]+1); },

       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0]-1, p[1], p[2]+1):Cell3DPosition(p[0], p[1]+1, p[2]+1); },

       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0]-1, p[1]-1, p[2]+1):Cell3DPosition(p[0], p[1], p[2]+1); },

       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0], p[1]-1, p[2]+1):Cell3DPosition(p[0]+1, p[1], p[2]+1); },

       [] (const Cell3DPosition &p) { return Cell3DPosition(p[0]-1, p[1], p[2]); },

       [] (const Cell3DPosition &p) { return Cell3DPosition(p[0], p[1]-1, p[2]); },
           
       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0]-1, p[1]-1, p[2]-1):Cell3DPosition(p[0], p[1], p[2]-1); },
           
       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0], p[1]-1, p[2]-1):Cell3DPosition(p[0]+1, p[1], p[2]-1); },
           
       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0], p[1], p[2]-1):Cell3DPosition(p[0]+1, p[1]+1, p[2]-1); },

       [] (const Cell3DPosition &p) { return IS_EVEN(p[2])?
               Cell3DPosition(p[0]-1, p[1], p[2]-1):Cell3DPosition(p[0], p[1]+1, p[2]-1); },
  };

} // namespace Catoms3D

#endif
