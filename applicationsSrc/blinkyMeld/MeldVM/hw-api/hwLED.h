#ifndef __HW_LED_H__
#define __HW_LED_H__

#ifndef IGNORE_IN_PASS1_OFF_COMPILE_BB
#include "../system/defs.h"
#include "../system/led.h"
#endif

void setHWLED(byte r, byte g, byte b, Intensity i);
void initHWLED(void);

#endif
