
#ifndef UTILS_H__
#define UTILS_H__

namespace BaseSimulator {

/** \namespace Utils Utilities namespace for providing globally needed constants and helper methods   
 */
namespace utils {

#ifndef M_PI
#define M_PI	3.1415926535897932384626433832795
#endif

#define IS_ODD(x) ((x) % 2)
#define IS_EVEN(x) (!IS_ODD(x))

inline static bool isInRange(int x, int a, int b) { return (a <= x && x <= b); };

const float M_SQRT2_2 = sqrt(2.0) / 2.0; //!< $\frac{\sqrt{2}}{2}$
const double M_SQRT3_2 = sqrt(3.0) / 2.0; //!< $\frac{\sqrt{3}}{2}$
const double EPS = 1E-5;

inline static bool file_exists(const string fileName) { std::ifstream infile(fileName); return infile.good(); }

}

}

#endif
