#ifndef TYPE_H_
#define TYPE_H_

#include <typeinfo>
#include <string>

#define TYPE_INT typeid(int)
#define TYPE_UINT typeid(uint)
#define TYPE_UNSIGNED typeid(unsigned)
#define TYPE_FLOAT  typeid(float)
#define TYPE_DOUBLE typeid(double)

#define TYPE_CHAR typeid(char)
#define TYPE_STRING typeid(string)

#define TYPE_INT_PTR typeid(int*)
#define TYPE_DOUBLE_PTR typeid(double*)
#define TYPE_CHAR_PTR typeid(char*)

#endif
