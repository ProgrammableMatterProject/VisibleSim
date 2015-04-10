// SYSTEM INCLUDES
#include "microphone.bbh"

// HARDWARE INCLUDES
#include "../hw-api/hwMicrophone.h"

threadvar MicData _mic;

MicData getMicData()
{
    return _mic;
}

/*
void updateMic()
{
	updateHWMic();
}
*/
/*
int newMicData()
{
	return newHWMicData();
}
*/

