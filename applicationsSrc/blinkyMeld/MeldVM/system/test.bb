#include "handler.h"
#include "led.h"

void myMain(void)
{
	setColor(3);
}

void userRegistration(void)
{
	registerHandler(SYSTEM_MAIN, (GenericHandler)&myMain);
}
