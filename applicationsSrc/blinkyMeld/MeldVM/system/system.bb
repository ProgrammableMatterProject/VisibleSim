#include "system.bbh"
//#include "unistd.h"

#include "led.bbh"

int blockProgram(void)
{

    initBlock();

    
    // low-level robot initialization routine
    
    // handler registration / load control structure
    userRegistration();
    
    // Call user-based system_init function, if registered
    // A good idea to separate from UserRegistration?
    callHandler(SYSTEM_INIT);    
    
    while(1)
    {
        // manage low level stuff    
        // manage stuff

        // step through triggered handler functions
	//printf("%d\r\n",count++);
	    
        // Run user system_main function, if registered
	callHandler(SYSTEM_MAIN);

// CAN COMMENT THIS OUT IN SIMULATOR (or just delete it in general, it's mostly pointless)		
//        _delay_us(10);
    }
    return 0;
}
