// SYSTEM INCLUDES
#include "accelerometer.bbh"
#include "handler.bbh"

// HARDWARE INCLUDES
#include "../hw-api/hwAccelerometer.h"

threadvar AccelData _acc;

AccelData getAccelData()
{
	return _acc;
}

int newAccelData()
{
	return newHWAccelData();
}

void updateAccel()
{
	//byte oldstatus = _acc.status & ACC_O_MASK;

	// this changes the _acc datastructure with new data, if available
	updateHWAccel();

}
