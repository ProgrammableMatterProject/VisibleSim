#ifndef __PARSER_H__
#define __PARSER_H__

#include "stdint.h"
#include "string.h"

/******************************************************************************
@Description: LMParser is used to generate a .bb file (In this case, a Meld byte code file for the targetVM = oldbb's one), from a .m file (Byte code file from Flavio Cruz's cl-meld compiler, I sometimes refer to it as source MeldVM).

@author: Pierre Thalamy
@email: pierre.thalamy@gmail.com
@date: 07/30/14
@note: Feel free to contact me if you have any question.
*******************************************************************************/

typedef unsigned char byte;

/* Used to check if opened file is Meld byte code file */
/* First 8 bytes of any Meld byte code file */
const uint32_t MAGIC1 = 0x646c656d;
const uint32_t MAGIC2 = 0x6c696620;

/* Macros from source MeldVM */
/* Only version 0.11 is currently supported by the parser */
#define VERSION_AT_LEAST(MAJ, MIN) \
(majorVersion > (MAJ) || (majorVersion == (MAJ) && minorVersion >= (MIN)))
/* Maximum length of a predicate name string */
#define PRED_NAME_SIZE_MAX 32
/* Maximum length of aggregate information */
/* PTHY: I do not really know the use of this since it looks always empty */
#define PRED_AGG_INFO_MAX 32
/* Maximum number of included predicates for a rule */
#define RULE_MAX_INCL_PREDS 32

/* Macros for target MeldVM */
/* Size of a predicate descriptor, without argument descriptor */
#define PREDICATE_DESCRIPTOR_SIZE 6
/* Size of a rule descriptor, without included predicate IDs */
#define RULE_DESCRIPTOR_SIZE 4

/* Structures used for storing information a rule and predicates, 
 * which makes printing easier */

typedef struct _Predicate {
  uint32_t codeSize; 		/* Size of byte code */
  byte argOffset;		/* Offset for global arg array */
  char *pName;			/* Name string */
  byte *pBytecode;		/* Pointer to byte code */

  uint32_t bytecodeOffset;	/* Offset to byte code in header */
  byte properties;		/* Linear, persistent, agg ... */
  byte agg;			/* Type of agg if applicable */
  byte level;			/* Stratification round */
  byte nFields;			/* Number of arguments */

  byte desc_size;		/* Size of predicate descriptor */
} Predicate;

typedef struct _Rule {
  uint32_t codeSize; 		/* Size of byte code */
  byte *pBytecode;		/* Pointer to bytecode */
  char *pName;			/* Rule string */

  uint32_t bytecodeOffset;	/* Offset to byte code */
  uint32_t numInclPreds;	/* Number of included predicates */
  byte inclPredIDs[RULE_MAX_INCL_PREDS];	/* ID of each included predicate */
  byte persistence;		/* Is the rule persistent? */

  byte desc_size;		/* Size of rule descriptor */
} Rule;


/* Source predicate properties */
#define PRED_AGG 0x01		/* Predicate is an aggregate */
#define PRED_ROUTE 0x02		/* Predicate is used for communication */
#define PRED_REVERSE_ROUTE 0x04	/* PTHY: Don't know the use of this one */
#define PRED_LINEAR 0x08	/* Linear predicate */
#define PRED_ACTION 0x10	/* Action predicate -- ignore it for targetVM */
#define PRED_REUSED 0x20	/* Unused by targetVM */
#define PRED_CYCLE 0x40		/* Unused by targetVM */
/* If not linear, predicate is persistent */

/* Source aggregate types */
/* PTHY: It seems useless for targetVM */
#define PRED_AGG_LOCAL 0x01
#define PRED_AGG_REMOTE 0x02
#define PRED_AGG_REMOTE_AND_SELF 0x04
#define PRED_AGG_IMMEDIATE 0x08
#define PRED_AGG_UNSAFE 0x00

/* Field types for source VM */
enum field_type {
   FIELD_INT = 0x0,
   FIELD_FLOAT = 0x1,
   FIELD_NODE = 0x2,
   FIELD_LIST = 0x3,
   FIELD_STRUCT = 0x4,
   FIELD_BOOL = 0x5,
   FIELD_ANY = 0x6, 
   FIELD_STRING = 0x9,
   FIELD_INTLIST = 0xa,
   FIELD_FLOATLIST = 0xb,
   FIELD_NODELIST = 0xc
};

/* Target predicate properties */
/* !-- TYPE = PREDICATE --! */
#define TYPE_AGG 0x01		/* Predicate is an aggregate */
#define TYPE_PERSISTENT 0x02	/* Predicate is persistent */
#define TYPE_LINEAR 0x03	/* Predicate is linear */
#define TYPE_DELETE 0x08	/* Unused */
#define TYPE_SCHEDULE 0x10	/* Unused */
#define TYPE_ROUTING 0x20	/* Unused */

/* Aggregate types common to both source and target VMs */
#define AGG_FIRST 1		/* First received? */
#define AGG_MAX_INT 2		/* Maximum int */
#define AGG_MIN_INT 3		/* Minimum int */
#define AGG_SUM_INT 4		/* Sum of all int's */
#define AGG_MAX_FLOAT 5		/* Max float */
#define AGG_MIN_FLOAT 6		/* Min float */
#define AGG_SUM_FLOAT 7		/* Sum of all floats */
#define AGG_SUM_LIST_FLOAT 11	/* PTHY: Don't really know what that is */

/* Compatibility with the targetVM may be limited for all these types since
 * they do not exist in the source byte code.
 * If it turns out that they need to be used, some additional 
 * conversion / implementation work may be needed */
#define AGG_SET_UNION_INT 8    
#define AGG_SET_UNION_FLOAT 9
#define AGG_SUM_LIST_INT 10

/* Field types for targetVM */
#define TFIELD_INT 0x0		/* 4 byte integer */
#define TFIELD_FLOAT 0x1	/* 8 byte double */
#define TFIELD_ADDR 0x2		/* 2 byte integer: NodeID */
#define TFIELD_OTHER 0x2	/* ? */
#define TFIELD_LIST_INT 0x3	/* List of integers */
#define TFIELD_LIST_FLOAT 0x4	/* List of doubles */
#define TFIELD_LIST_ADDR 0x5	/* List of NodeID's */

#define TFIELD_SET_INT 0x6	/* Unused */
#define TFIELD_SET_FLOAT 0x7	/* Unused */
#define TFIELD_TYPE 0x8		/* Unused */
#define TFIELD_STRING 0x9	/* Unused */

#define TFIELD_BOOL 0xa		/* Boolean, implemented but not tested */

#endif	/* ifdef __PARSER_H__ */
