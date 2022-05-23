/**
 * @file   replayTags.h
 * @author Pierre Thalamy <pthalamy@pierre-ZenBook-UX433FA>
 * @date   Tue Jun  9 11:56:25 2020
 *
 * @brief All tags and byte code flags to be used by both the replay exporter and players
 *
 *
 */
#ifndef REPLAYTAGS_H_
#define REPLAYTAGS_H_


namespace ReplayTags {

typedef std::uint8_t u1;
typedef std::uint16_t u2;
typedef std::uint32_t u4;
typedef std::uint64_t u8;

typedef std::int8_t s1; // [s]igned 1
typedef std::int16_t s2;
typedef std::int32_t s4;
typedef std::int64_t s8;

typedef struct KeyframeBlock {
    int id;
    short x,y,z;
    u2 rotation;
    u1 r,g,b;
    u2 displayedValue;
} KeyFrameBlock;

typedef struct Keyframe {
    //KeyFrameBlock * block;
    int id;
    u8 time;
    u8 position;
    u8 eventPosition;
    int blockCount = 0;
} KeyFrame;




/* GENERAL FLAGS */

/**
 * Used as the first 32 bits of a replay data file to identify
 *  its content. Spells "VSim" in ASCII.
 * @size 4 bytes
 */
// const u4 VS_MAGIC = 0x5653696d;
const u4 VS_MAGIC = 0x6d695356;

/* MODULE TYPES */
/**
 * Identifies the type of the simulated modular robot
 * @size 1 byte
 */
const u1 MODULE_TYPE_BB = (u1)0;
const u1 MODULE_TYPE_C2D = (u1)1;
const u1 MODULE_TYPE_C3D = (u1)2;
const u1 MODULE_TYPE_DATOM = (u1)3;
const u1 MODULE_TYPE_HEXANODE = (u1)4;
const u1 MODULE_TYPE_NODE2D = (u1)5;
const u1 MODULE_TYPE_OKTEEN = (u1)6;
const u1 MODULE_TYPE_SLIDINGCUBE = (u1)7;
const u1 MODULE_TYPE_SMARTBLOCKS = (u1)8;
const u1 MODULE_TYPE_VCELLS = (u1)9;

/* EVENT TYPES */
const u1 EVENT_COLOR_UPDATE = (u1)0; // Message Length : 3*1 = 3 Bytes
const u1 EVENT_DISPLAY_UPDATE = (u1)1; // Message Length : 2 Bytes
const u1 EVENT_POSITION_UPDATE = (u1)2; // Message Length : 3*2 + 1 = 7 Bytes
const u1 EVENT_ADD_MODULE = (u1)3; // Message Length :
const u1 EVENT_REMOVE_MODULE = (u1)4; // Message Length :
const u1 EVENT_MOTION = (u1)5; // Message Length : 8 + 3*2 = 14
const u1 EVENT_CONSOLE_TRACE = (u1)6; // Message Length :
const u1 EVENT_MOTION_CATOMS3D = (u1)7; // Message Length :

/* REPLAY MODES */
const u1 REPLAY_MODE_PAUSE = (u1)0;
const u1 REPLAY_MODE_PLAY = (u1)1;

}

#endif
