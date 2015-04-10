#ifndef __HWAUDIO_H__
#define __HWAUDIO_H__

#include <stdint.h>

#define SAMPLES 64

void chirpHW(unsigned int, unsigned int);
void setDacHW(unsigned int, unsigned int);

void initHWAudio(void);

#endif
