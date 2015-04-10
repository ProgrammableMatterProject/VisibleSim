#include "audio.bbh"
#include "../hw-api/hwAudio.h"

void initAudio(void)
{
  initHWAudio();
}

void chirp(unsigned int freq, unsigned int duration)
{
  chirpHW(freq, duration);
}

void setDac(unsigned int ch0, unsigned int ch1)
{
  setDacHW(ch0, ch1);
}
