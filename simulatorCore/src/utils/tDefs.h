/**
 * @file tDef.h
 * @brief Header for base type definitions used across all source files.
 * @date 04/08/2016
 * @author Pierre Thalamy
 */

#ifndef TDEF_H_
#define TDEF_H_

#include <cinttypes>
#include <random>
#include <functional>

typedef uint32_t bID;           //!< Defines the type of a module identifier
#define BID_MAX UINT32_MAX       //!< Maximum module identifier value

typedef uint64_t Time; //!< Time (unit: us)
#define TIME_MAX UINT64_MAX //!< Maximum possible value of time given its type
typedef double Distance; //!< Distance (unit: meter)

enum ModuleType {BB, RB, SB, C2D, C3D, MR}; /** Defines a type that can be used to refer to a particular type of Module:
                                             * In order: BlinkyBlock, RobotBlock, Catom2D, Catom3D, MultiRobot.
                                             * Used with generic block codes to determine the type of simulator to instantiate. */

#endif  // TDEF_H_
