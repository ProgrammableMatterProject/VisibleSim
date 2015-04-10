#ifndef __HW_DEBUG_H__
#define __HW_DEBUG_H__

#include <stdio.h>

//  DEBUG 
//
void initHWDebug(void);

extern FILE debug;

int debugPutChar(char c, FILE * fb);
int debugGetChar(FILE * fb);

#endif
