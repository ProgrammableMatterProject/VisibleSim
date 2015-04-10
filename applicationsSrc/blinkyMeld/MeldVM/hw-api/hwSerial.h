#ifndef __HW_SERIAL_H__
#define __HW_SERIAL_H__

#ifndef IGNORE_IN_PASS1_OFF_COMPILE_BB
#include "../system/serial.h"
#endif

void initHWPorts(void);
void pPutChar(char c, PRef p);
int pGetChar(PRef p);

#endif
