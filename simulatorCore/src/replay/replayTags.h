/**
 * @file   replayTags.h
 * @author Pierre Thalamy <pthalamy@pierre-ZenBook-UX433FA>
 * @date   Tue Jun  9 11:56:25 2020
 *
 * @brief All tags and byte code flags to be used by both the replay exporter and players
 *
 *
 */

namespace ReplayTags {

typedef std::byte u1;
typedef std::uint16_t u2;
typedef std::uint32_t u4;
typedef std::uint64_t u8;

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
}
