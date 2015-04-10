#ifndef _MYASSERT_H_
#define  _MYASSERT_H_
#ifdef assert
#undef assert
#endif

# define assert(e)	((e) ? (void)0 : __myassert(__FILE__, __LINE__, #e))
void __myassert(char* file, int line, char* exp);
#endif
