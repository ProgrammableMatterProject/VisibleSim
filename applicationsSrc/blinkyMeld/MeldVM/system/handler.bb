#include "handler.bbh"

// System struct for managing callbacks.
// Hardcoded a bit - NUM_HANDLERS must be less than sizeof(handlermask_t)
threadtype typedef struct _systemhandler { HandleMask HandlerMask; GenericHandler HandlerTable[NUM_HANDLERS]; } SystemHandler;

threadvar SystemHandler system;


void initHandlers(void){
  int i;
  system.HandlerMask=0;
  for( i=0; i<NUM_HANDLERS; i++){
    system.HandlerTable[i]=NULL;
  }
}

//  ExecuteHandlers
//
//  if any mask is set, will scan through and execute handlers in ascending numerical order.
//  it is technically possible to trigger handlers during earlier handlers, or even the same handler repeatedly.
//  triggering an earlier handler will cause it to execute during the next iteration.
void executeHandlers(void)
{
	if(system.HandlerMask)
	{
		Event i = 0;
		uint32_t j = 0x00000001;
	
		while(i < NUM_HANDLERS)
		{
			if(system.HandlerMask & j)
			{
				// clear mask bit1
				system.HandlerMask &= ~j;
				
				// execute callback
				(*(system.HandlerTable[i]))();
			}

			i++;
			j <<= 1;
		}
	}
}


//TriggerHandler
//	will trigger a callback for a valid event and valid handler
//  execution will occur during the CURRENT or NEXT handler scan
// 	returns 1 on success, 0 on failure
int triggerHandler(Event e)
{
	if(e < NUM_HANDLERS)
	{
		if(system.HandlerTable[e] == NULL)
		{
			return 0;
		}
		
		system.HandlerMask |= (uint32_t)1 << e;
		return 1;
	}
	
	return 0;	
}


// RegisterHandler
// will register a callback for a valid event with currently NULL callback.
// must unregister a valid handler before re-registering.
// returns 1 on success, 0 on failure.
int registerHandler(Event e, GenericHandler callback)
{
	if(e < NUM_HANDLERS)
	{
		if(system.HandlerTable[e] == NULL)
		{
			system.HandlerTable[e] = callback;
			return 1;
		}
		
		return 0;
	}
	
	return 0;
}


// UnregisterHandler
// will unregister a valid callback for a valid event, setting it to NULL.
// must unregister a valid handler before re-registering.
// returns 1 on success, 0 on failure.
int unregisterHandler(Event e)
{
	if(e < NUM_HANDLERS)
	{
		if(system.HandlerTable[e] == NULL)
		{
			return 0;
		}
		
		system.HandlerTable[e] = NULL;
		return 1;
	}
	
	return 0;
}

void callHandler(Event e)
{
	if(e < NUM_HANDLERS)
	{
		if(system.HandlerTable[e] != NULL)
		{
			(system.HandlerTable[e])();
		}
	}
}
