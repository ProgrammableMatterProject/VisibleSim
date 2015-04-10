#ifndef __HW_TIME_H__
#define __HW_TIME_H__

#ifndef IGNORE_IN_PASS1_OFF_COMPILE_BB
#include  "../system/hardwaretime.h"
#endif

Time getHWTime(void);
Time getHWTimeUS(void);
void initHWTime(void);
#endif

