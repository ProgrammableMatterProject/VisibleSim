// call addCompileInfo with string describing the options being compiled into the program.
#ifndef _COMPILE_INFO_
#define COMPILE_INFO

namespace xutils {

#ifndef BBHW
char* addCompileInfo(char const* info);
#else
# define addCompileInfo(x)
#endif

}

#ifndef BBHW
# define declareCompileInfo(x, y) char* _ ## x = xutils::addCompileInfo(#x  ":" y)
#else
# define declareCompileInfo(x, y) 
#endif


#endif


// Local Variables:
// mode: C++
// indent-tabs-mode: nil
// End:
