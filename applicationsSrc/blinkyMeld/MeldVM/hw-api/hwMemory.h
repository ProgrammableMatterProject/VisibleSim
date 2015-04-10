#ifndef __HW_MEMORY_H__
#define __HW_MEMORY_H__

#ifndef IGNORE_IN_PASS1_OFF_COMPILE_BB
#include "../system/defs.h"
#endif

//  Store a data structure in EEPROM. 
//    arguments are in the form:
//    EEPROM_DEST, SRAM_SOURCE, SIZEOF(SRAM_SOURCE)
void store(void * dest, void * src, int len);

//  Load a data structure from EEPROM. 
//    arguments are in the form:
//    SRAM_DEST, EEPROM_SOURCE, SIZEOF(EEPROM_SOURCE)
void restore(void * dest, void * src, int len);

/* TODO: This is WRONG. It should be Uid, but the build system is being stupid and now is not a good time to spend forever trying to fix it. */
uint16_t getGUID(void);
 
#endif
