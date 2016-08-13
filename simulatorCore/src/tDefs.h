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

typedef uint64_t bID;           //!< Defines the type of a module identifier
#define BID_MAX UINT64_MAX       //!< Maximum module identifier value

typedef uint64_t Time; //!< Time (unit: us)
typedef double Distance; //!< Distance (unit: meter)

#endif  // TDEF_H_
