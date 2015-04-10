#include "hardwaretime.bbh"
#include "../hw-api/hwTime.h"
#include "assert.h"

threadvar Timeout* thisTimeout;		// global var referencing current timeout struct for callbacks
threadvar Timeout* timeoutList;	// semi-private data, do not modify outside of this file
threadvar Timer* timerList;		// semi-private data, do not modify outside of this file

#ifdef BBSIM
extern void yieldTil(Time x);
#endif

void delayMS(int ms) 
{
  Time until = getTime() + ms;
	
  while(getTime() < until) {
#ifdef BBSIM
    yieldTil(until);
#endif
  }
}

Time getTime()
{
	return getHWTime();
}

void checkTimeout()
{
	if(timeoutList != NULL)
	{
		Time now = getTime();
		
		do
		{
			// check list, remove timer and call function
			if(now >= timeoutList->calltime)
			{
				// set reference variable, remove timeout from list
				thisTimeout = timeoutList;
				timeoutList = timeoutList->next;

				// if timeout was not pre-emptively disabled, disable it, execute callback();
				if(thisTimeout->state != INACTIVE)
				  {
				    // disable callback until reactivated/reinserted into list.
				    thisTimeout->state = INACTIVE;
				   
				    (thisTimeout->callback)();	
				  }
			}
			else
			{
				// stop searching list
				break;
			}
			
		} while (timeoutList != NULL);
	}
}

int registerTimeout(Timeout * t)
{
	t->next = NULL;

	if(timeoutList == NULL)
	{
		timeoutList = t;
	}
	else
	{
		Timeout * prev = NULL;
		Timeout * cur;
		
		cur = timeoutList;
		
		while((cur->calltime < t->calltime) && (cur->next != NULL))
		{
			prev = cur;
			cur = cur->next;
		}		
		
		if(cur->calltime >= t->calltime)
		{
			if(prev == NULL)
			{
				timeoutList = t;
			}
			else
			{
				prev->next = t;
			}
			
			t->next = cur;
		}
		else
		{
			cur->next = t;
			t->next = NULL;
		}
	}
	
	t->state = ACTIVE;
	
	return 1;	
}

int deregisterTimeout(Timeout * t)
{
	if(timeoutList == NULL)
	{		
		return 0;
	}
	else
	{
		Timeout * prev = NULL;
		Timeout * cur;

		cur = timeoutList;
		

		while((cur != NULL) && (t != cur)) //(cur->callback != t->callback) && (cur->calltime != t->calltime))
		{
			prev = cur;
			cur = cur->next;
		}

		
		if(cur == NULL)
		{
			return 0;
		}
		else
		{
			if(prev == NULL)
			{
				timeoutList = cur->next;
			}
			else
			{
				prev->next = cur->next;
			}
			
			t->state = INACTIVE;
			
			return 1;
		}
	}
}

int deregisterTimeoutByHandler(GenericHandler h)
{
	if(timeoutList == NULL)
	{		
		return 0;
	}
	else
	{
		Timeout * prev = NULL;
		Timeout * cur;

		cur = timeoutList;
		
		while((cur != NULL) && (cur->callback != h))
		{
			prev = cur;
			cur = cur->next;
		}
		
		if(cur == NULL)
		{
			return 0;
		}
		else
		{
			if(prev == NULL)
			{
				timeoutList = cur->next;
			}
			else
			{
				prev->next = cur->next;
			}
			
			cur->state = INACTIVE;
			
			return 1;
		}
	}

}

void checkTimer()
{
	Timer * tt = timerList;
	
	while(tt != NULL)
	{
		if(tt->state == ACTIVE)
		{
			if((tt->t).state == INACTIVE)
			{
				(tt->t).calltime = getTime() + tt->period;
				registerTimeout(&(tt->t));
				
			}
		}
	
		tt = tt->next;
	}
}

int registerTimer(Timer * tt)
{
  tt->next = NULL;

	if(timerList == NULL)
	{
		timerList = tt;
		
	}
	else
	{
		Timer * cur = timerList;
		
		
		while(cur->next != NULL)
		{
		  assert(tt != cur);
			cur = cur->next;
		}
		assert(tt != cur);
		
		cur->next = tt;
	}
	
	tt->state = ACTIVE;	
	
	return 1;
}

int deregisterTimer(Timer * tt)
{
	if(timerList == NULL)
	{
		return 0;
	}
	else
	{
		Timer * prev = NULL;
		Timer * cur = timerList;
	
		while(cur != NULL && cur != tt)
		{
			prev = cur;
			cur = cur->next;
		}
		
		if(cur == NULL)
		{
			return 0;
		}
		else
		{
			if(prev == NULL)
			{
				timerList = cur->next;
			}
			else
			{
				prev->next = cur->next;
			}
			cur->next = NULL;
			cur->state = INACTIVE;
			
			return 1;
		}
	}
}

// attempts to deregister the timer and its timeout from both queues.
// returns the sum of the component deregistrations.
int clearTimer(Timer * tt)
{

  int ret = deregisterTimer(tt);


  if(tt != NULL)
    {
      ret += deregisterTimeout(&(tt->t));
    }

  return ret;
}

void initTime()
{
	timeoutList = NULL;
	thisTimeout = NULL;
	timerList = NULL;
	
	initHWTime();
}	
