#ifndef __HW_BLOCK_TICK_H__
#define __HW_BLOCK_TICK_H__

//  This does TWO THINGS:
//  sets the frequency at which blocktick runs (currently ~40khz)
//	enables interrupts (*very* important to many of the system functions in HW)

void initBlockTick(void);	


#endif
