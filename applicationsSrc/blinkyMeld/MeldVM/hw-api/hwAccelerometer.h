#ifndef __HW_ACCELEROMETER_H__
#define __HW_ACCELEROMETER_H__

#ifndef IGNORE_IN_PASS1_OFF_COMPILE_BB
#include "../system/defs.h"
#endif

int newHWAccelData(void);
void updateHWAccel(void);
void initHWAccel(void);

// helper function to setup accelerometer registers
void setAccelRegister(byte one, byte two);

#endif
