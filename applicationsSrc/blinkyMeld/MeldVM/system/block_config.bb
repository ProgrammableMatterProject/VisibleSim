// block_config.c
//
// Contains block configuration data

#ifndef _BLOCK_CONFIG_C_
#define _BLOCK_CONFIG_C_

#include "block_config.bbh"

threadextern blockConf EEMEM nv_conf;
threadextern blockConf conf;

// sets local copy of UID
void setUID(uint16_t newID)
{
    conf.UID = newID;
}

// sets local copy of UID and stores in EEPROM
void setAndStoreUID(uint16_t newID)
{
  // Reverse first and second bytes of newID
  uint16_t tmp = 0;
  uint8_t *a = (uint8_t*) &newID;
  tmp = *(a) << 8 | *(a+1);
  
  // Assign ID to the block
  conf.UID = tmp;
  
  // Store into EEPROM
  store(&nv_conf, &conf, sizeof(blockConf));
}

#endif
