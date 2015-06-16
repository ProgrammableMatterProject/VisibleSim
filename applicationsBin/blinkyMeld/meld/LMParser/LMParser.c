#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "LMParser.h"

/******************************************************************************

@note: Check out LMParser.h for more information.

*******************************************************************************/

/* Uncomment to print content of input file */
//#define DEBUG_PARSER

/* Function prototypes */
byte readType (FILE *pFile);
byte readTypeID (FILE *pFile, byte typeArray[]);
void skipNodeReferences (FILE *pFile);
size_t sizeOfPredicateDescriptors(Predicate predicates[], byte numPreds);
size_t sizeOfRuleDescriptors(Rule rules[], byte numRules);

int 
main (int argc, char* argv[])
{
  /* Verify number of arguments */
  if (argc != 2) {
    perror ("Invalid number of arguments\n"
	    "Usage: ./LMParser <path-to-.m-file>\n");
    exit(1);
  }

  char *inNameBuf = argv[1];
  printf("\nLMParser: Parsing file %s\n", inNameBuf);

  /* Deduce output file's name from name of input file */
  char outNameBuf[strlen (inNameBuf) + 2]; 
  int i,j;
  for (i = 0;  i < (strlen (inNameBuf) - 1); ++i) {
    outNameBuf[i] = inNameBuf[i];
  }
  outNameBuf[i++] = 'b';
  outNameBuf[i++] = 'b';
  outNameBuf[i] = '\0';

  /* Open input file in read-only mode */
  FILE *pMeldProg;
  pMeldProg = fopen (inNameBuf, "r");

  /* Open output file in write-only mode */
  FILE *pBBFile;
  pBBFile = fopen (outNameBuf, "w");

  /* Check that file opening succeeded and start parsing */
  if (pMeldProg == NULL) {
     perror ("Error opening file");
     exit(1);
  } else
    {
      /* ************* FILE AND VERSION CHECK ************* */

      /* Read magic and check that program is Meld file */
      uint32_t magic1, magic2;
      fread (&magic1, 4, 1, pMeldProg);
      fread (&magic2, 4, 1, pMeldProg);	  

#ifdef DEBUG_PARSER
      printf ("\nmagics: %#x %#x\n", magic1, magic2);
#endif

      if (magic1 != MAGIC1 || magic2 != MAGIC2) {
	perror ("Not a Meld byte code file!");
	exit (-1);
      }
#ifdef DEBUG_PARSER
      else printf ("magics OK\n");
#endif

      /* Check file version: 0.12 is the only supported version for now */
      uint32_t majorVersion, minorVersion;
      fread (&majorVersion, 4, 1, pMeldProg);
      fread (&minorVersion, 4, 1, pMeldProg);
      if ( !VERSION_AT_LEAST(0, 12) || VERSION_AT_LEAST(0, 13) ) {
	perror ("Unsupported byte code version");
	exit(-2);
      }
#ifdef DEBUG_PARSER
      else printf ("Version OK\n");
#endif

      /* ************* NUMBER OF PREDICATES ************* */

      /* Read number of predicates in the program */
      /* It includes both the program's predicate, and some mandatory predicates
       * added by the compiler */
      byte numPredicates;
      fread (&numPredicates, 1, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of predicates: %d\n", numPredicates);
#endif
      /* Initialize Predicate structure array */
      Predicate predicates[numPredicates];

      /* ************* NUMBER OF NODES ************* */

      /* Read number of nodes */
      uint32_t numNodes;
      fread (&numNodes, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of nodes: %d\n", numNodes);
#endif

      /* The number of nodes for a BB program has to be 0 */
      if (numNodes > 0) {
	perror ("Invalid number of nodes -- Should be 0\n");
	exit(-3);
      }

      /* ************* TYPES ************* */

      /* Read number of types 
       * Types (int, node, string, float, etc...) are stored in a list in the 
       * source byte code. All other types in the byte code will simply be 
       * references to the type at index i of this list
       */
      byte numTypes;
      fread (&numTypes, 1, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of types: %d\n", numTypes);
#endif

      /* Read types */
      byte types[numTypes];
#ifdef DEBUG_PARSER
      printf ("Types: ");
#endif
      for (i = 0; i < numTypes; ++i) 
	types[i] = readType (pMeldProg);
#ifdef DEBUG_PARSER
      printf ("\n");
#endif

      /* ************* IMPORTED / EXPORTED PREDICATES ************* */

      /* Read number of imported predicates */
      /* PTHY: Unused by targetVM for now since I do not know what they represent
       * and have never seen them used.
       */
      uint32_t numImportedPreds;
      fread (&numImportedPreds, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of imported predicates: %d\n", numImportedPreds);
#endif

      /* Read number of exported predicates */
      /* PTHY: Same as for imported predicates */
      uint32_t numExportedPreds;
      fread (&numExportedPreds, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of exported predicates: %d\n", numExportedPreds);
#endif

      /* ************* MELD PROGRAM ARGUMENTS ************* */

      /* Read number of args needed by program 
       * Unused by targetVM, should be 0
       */
      byte numProgArgs;
      fread (&numProgArgs, 1, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of program arguments: %d\n", numProgArgs);
#endif

      /* ************* RULE STRINGS ************* */

      /* Read and store rule information */
      uint32_t numRules;	/* Number of rules in the program */
      fread (&numRules, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of rules: %i\n", numRules);
#endif

      /* Initialize array containing all rules */
      Rule rules[numRules];

      for (i = 0; i < numRules; ++i) {
#ifdef DEBUG_PARSER
	printf ("  Rule %d: ", i);
#endif	    
	/* Read rule length */
	uint32_t ruleLength;	/* Length of rule name string */
	fread (&ruleLength, 4, 1, pMeldProg);
	    
	/* Read rule string */
	rules[i].pName = malloc (ruleLength + 1);
	/* Sometimes cl-meld adds newLine characters to the rule string
	 * we have to get rid of it otherwise the output file will have syntax
	 * issues. Read characters one by one and eliminate newlines.
	 */
	char c; 
	byte charPos = 0;
	for (j = 0; j < ruleLength; ++j) {
	  fread (&c, 1, 1, pMeldProg);
	  if (c == '\n')
	    continue;
	  else
	    rules[i].pName[charPos++] = c;
	}
	rules[i].pName[charPos] = '\0';
#ifdef DEBUG_PARSER
	printf ("%s\n", rules[i].pName);
#endif
      }
#ifdef DEBUG_PARSER
      printf ("\n");
#endif

      /* ************* CONSTANTS ************* */

      /* Read string constants */
      /* PTHY: Not implemented yet since constants were not compiling 
       * at the time I did this work
       */
      uint32_t numStrings;
      fread (&numStrings, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of string constants: %d\n", numStrings);
#endif

      for (i = 0; i < numStrings; ++i) {
#ifdef DEBUG_PARSER
	printf ("  String %d: ", i);
#endif

	uint32_t stringLength;
	fread (&stringLength, 4, 1, pMeldProg);
				
	char constStr[stringLength + 1];
	fread (&constStr, 1, stringLength, pMeldProg);
	constStr[stringLength] = '\0';

#ifdef DEBUG_PARSER
	printf ("%s\n", constStr);
#endif
      }

      /* Read constants */
      uint32_t numConstants;
      fread (&numConstants, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of constants: %d\n", numConstants);
#endif
 
      /* Read constants type */
      byte constTypes[numConstants];
      for (i = 0; i < numConstants; ++i) {
	constTypes[i] = readTypeID(pMeldProg, types);
      }

      /* Avoid unused variable warning */
      (void) constTypes;

      /* Read and store code */
      uint32_t constCodeSize;
      fread (&constCodeSize, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Constant code size: %d\n", constCodeSize);
#endif

      byte constCode[constCodeSize];
      fread (&constCode, 1, constCodeSize, pMeldProg);
#ifdef DEBUG_PARSER      
      printf("code: %x\n", constCode[0]);
#endif
      skipNodeReferences (pMeldProg);

      /* ************* FUNCTIONS ************* */

      /* PTHY: Functions are currently not implemented into the VM */

      /* Read function code */
      uint32_t numFunctions;
      fread (&numFunctions, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of functions: %d\n", numFunctions);
#endif

      for (i = 0; i < numFunctions; ++i) {
	uint32_t functionSize;
	fread (&functionSize, 4, 1, pMeldProg);

	byte functionCode[functionSize];
	fread (&functionSize, 1, functionSize, pMeldProg);
	/* Avoid getting unused variable warning */
	(void)functionCode;

	skipNodeReferences (pMeldProg);
      }

      /* Read external functions definitions */
      uint32_t numExternalFunctions;
      fread (&numExternalFunctions, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of external functions: %d\n", numExternalFunctions);
#endif
      
      for (i = 0; i < numExternalFunctions; ++i) {
#ifdef DEBUG_PARSER
	printf ("  Extern %d:\n", i);
#endif
	
	uint32_t externID; 	/* ID of the function */
	fread (&externID, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
	printf ("    ID: %d\n", externID);
#endif

	char externName[256];	/* Name string */
	fread (&externName, 1, sizeof(externName), pMeldProg);
#ifdef DEBUG_PARSER
	printf ("    Name: %s\n", externName);
#endif
	
	char skipFilename[1024]; /* Function file 's name */
	fread (&skipFilename, 1, sizeof(skipFilename), pMeldProg);
#ifdef DEBUG_PARSER
	printf ("    Filename: %s\n", skipFilename);
#endif

	uint64_t skipPtr;	/* Pointer to function? */
	fread (&skipPtr, sizeof(uint64_t), 1, pMeldProg);
	
	uint32_t numFuncArgs;	/* Number of args for function */
	fread (&numFuncArgs, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
	printf ("    Number of args: %d\n", numFuncArgs);
#endif
	
	if (numFuncArgs) {
#ifdef DEBUG_PARSER
	  printf ("    Types: ");
#endif
	  byte funcArgTypes[numFuncArgs]; /* Type of each argument */
	  
	  for (j = 0; j < numFuncArgs; j++) {
	    funcArgTypes[j] = readTypeID (pMeldProg, types);
	  }
	 
	  /* Avoid getting unused variable warning */
	  (void)funcArgTypes;
	} 
      }

      /* ************* PREDICATE INFORMATION ************* */

      /* Number of arguments in the program, used to keep track of the current
       * index of the allArguments array */
      size_t totalArguments = 0; 
      /* All argument types of the program will be stored here linearly,
       * and each predicate will have an offset to this array to access its own 
       * argument types.
       */
      byte allArguments[256];
      
#ifdef DEBUG_PARSER
      printf ("\n PREDICATE DESCRIPTORS \n");
#endif

      for (i = 0; i < numPredicates; ++i) { 
#ifdef DEBUG_PARSER
	printf("  Predicate %d:\n", i);
#endif
	/* Read size of byte code for this predicate */
	uint32_t codeSize;
	fread (&codeSize, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
	printf("    code size: %d\n", codeSize);
#endif
	predicates[i].codeSize = codeSize;

	/* Read predicate properties */
	byte prop;
	fread (&prop, 1, 1, pMeldProg);

#ifdef DEBUG_PARSER
	printf ("    Properties: ");
#endif	
	/* Format it to target property byte format */
	/* A conversion of the property byte is happening here! */
	byte targetProp = 0x0;

	if (prop & PRED_AGG) {
#ifdef DEBUG_PARSER
	  printf ("AGG ");
#endif
	  targetProp |= 0x01;
	}
	if (prop & PRED_LINEAR) {
#ifdef DEBUG_PARSER
	  printf ("LINEAR ");
#endif
	  targetProp |= 0x04;
	}
	else {
#ifdef DEBUG_PARSER
	  printf ("PERSISTENT ");
#endif
	  targetProp |= 0x02;
	}
	if (prop & PRED_ROUTE) {
#ifdef DEBUG_PARSER
	  printf ("ROUTE ");
#endif
	  targetProp |= 0x20;
	}
	if (prop & PRED_REVERSE_ROUTE) {
#ifdef DEBUG_PARSER
	  printf ("REVERSE-ROUTE ");
#endif
	  targetProp |= 0x20;
	}
	if (prop & PRED_ACTION) {
#ifdef DEBUG_PARSER
	  printf ("ACTION ");
#endif
     targetProp |= 0x10;
	}
	if (prop & PRED_REUSED) {
#ifdef DEBUG_PARSER
	  printf ("REUSED ");
#endif
	  /* Not specified in target VM */
	}
	if (prop & PRED_CYCLE) {
#ifdef DEBUG_PARSER
	  printf ("CYCLE ");
#endif
	  /* Not specified in target VM */
	}
#ifdef DEBUG_PARSER
	printf ("\n");
#endif
	predicates[i].properties = targetProp;
	
	/* Aggregate information if any */
	byte agg;
	fread (&agg, 1, 1, pMeldProg);
	predicates[i].agg = agg;

	byte aggField = agg & 0xf; 
	byte type = ((0xf0 & agg) >> 4);

	if (prop & PRED_AGG) {
#ifdef DEBUG_PARSER
	  printf ("    Aggregate: \n");
	  printf ("     field: %d\n", aggField);
	  printf ("     type: ");

	  switch(type) {
	  case AGG_FIRST: printf ("first\n"); break;
	  case AGG_MAX_INT: printf ("max_int\n"); break;
	  case AGG_MIN_INT: printf ("min_int\n"); break;
	  case AGG_SUM_INT: printf ("sum_int\n"); break;
	  case AGG_MAX_FLOAT: printf ("max_float\n"); break;
	  case AGG_MIN_FLOAT: printf ("min_float\n"); break;
	  case AGG_SUM_FLOAT: printf ("sum_float\n"); break;
	  case AGG_SUM_LIST_FLOAT: printf ("sum_list_float\n"); break;
	  }
#else
(void)aggField;
(void)type;
#endif
	}

	/* Stratification level */
	byte stratLevel;
	fread (&stratLevel, 1, 1, pMeldProg);
	predicates[i].level = stratLevel;
#ifdef DEBUG_PARSER
	printf ("    Stratification level: %d\n", stratLevel);
#endif

	/* skip index */
	byte fieldIndex;
	fread(&fieldIndex, 1, 1, pMeldProg);

	/* Number of fields (# of arguments) */
	byte numFields;
	fread (&numFields, 1, 1, pMeldProg);
	predicates[i].nFields = numFields;
#ifdef DEBUG_PARSER
	printf ("    Number of fields: %d\n", numFields);
#endif


	/* Argument types */
#ifdef DEBUG_PARSER
	printf ("    Field types: ");
#endif
	/* Set offset to argument types in allArguments array */
	predicates[i].argOffset = totalArguments;
	
	int k;
	for (k = 0; k < numFields; ++k) {
	  allArguments[totalArguments++] = readTypeID (pMeldProg, types);
	}
#ifdef DEBUG_PARSER
	printf ("\n");
#endif

	/* Predicate name string */
	predicates[i].pName = malloc (PRED_NAME_SIZE_MAX + 1);
	
	fread (predicates[i].pName, 1, PRED_NAME_SIZE_MAX, pMeldProg);

	/* PTHY: Using %s returns a seg fault for some reason,
	   print one character at a time instead */
#ifdef DEBUG_PARSER
	printf ("    Name: ");
	char *sc = predicates[i].pName;
	for (j = 0; (j < PRED_NAME_SIZE_MAX) || (*sc == '\0'); ++j) {
	  printf("%c", *sc);
	  ++sc;
	}
	printf ("\n");
#endif

	/* Aggregate info - Seems always empty, ignored it */
	char bufVec[PRED_AGG_INFO_MAX];
	fread (&bufVec, 1, PRED_AGG_INFO_MAX, pMeldProg);
	char *buf = bufVec;
	
#ifdef DEBUG_PARSER
	printf ("    Aggregate info (if any): ");
	if (prop & PRED_AGG) {
	  if (buf[0] == PRED_AGG_LOCAL) {
	    buf++;
	    printf ("local_agg\n");
	  } else if (buf[0] == PRED_AGG_REMOTE) {
	    buf++;
	    printf ("neighborhood_arg\n");
	  } else if(buf[0] == PRED_AGG_REMOTE_AND_SELF) {
	    buf++;
	    printf ("neighborhood_and_self_agg\n");
	  } else if(buf[0] == PRED_AGG_IMMEDIATE) {
	    buf++;
	    printf ("immediate_agg\n");
	  } else if(buf[0] & PRED_AGG_UNSAFE) {
	    buf++;
	    printf ("unsafe_agg\n");
	  }
	  else printf ("unknown\n");
	} else printf ("  none\n");
#else
(void)buf;
#endif

#ifdef DEBUG_PARSER
	printf ("\n");
#endif
	/* Set TOTAL descriptor size (Argument descriptor included) */
	predicates[i].desc_size = PREDICATE_DESCRIPTOR_SIZE + numFields;	
      }

	/* ************* PRIORITY INFO ************* */
  
	/* Read global priority info */
	/* This is ignored by the parser */
#ifdef DEBUG_PARSER
      printf ("\nPRIORITY INFO\n");
#endif
      byte globalInfo;
      fread (&globalInfo, 1, 1, pMeldProg);
      
      switch (globalInfo) {
      case 0x01: perror ("Priority by predicate - not supported anymore.\n"); break;
      case 0x02: {
#ifdef DEBUG_PARSER
	printf ("Normal priority\n");
#endif
	byte type = 0x0;
	byte ascDesc;

	fread (&type, 1, 1, pMeldProg);
#ifdef DEBUG_PARSER
	printf ("Type: field float\n");
#endif

	fread (&ascDesc, 1, 1, pMeldProg);
#ifdef DEBUG_PARSER
	if (ascDesc & 0x01) printf ("Order: asc\n");
#endif
#ifdef DEBUG_PARSER
	else printf ("Order: desc\n");
#endif

	double initialPriorityValue;
	fread (&initialPriorityValue, sizeof(double), 1, pMeldProg);
#ifdef DEBUG_PARSER
	printf ("Initial priority value: %f\n", initialPriorityValue);
#endif
      }	break;
      case 0x03: perror ("File wrongly appears as data file\n"); break;
      }

      /* ************* PREDICATE BYTE CODE ************* */

      /* Read predicate bytecode */
#ifdef DEBUG_PARSER
      printf ("\nEXTRACTING PREDICATE BYTE CODE...\n");
#endif
      for (i = 0; i < numPredicates; ++i) {
	uint32_t bytecodeSize = predicates[i].codeSize;
	if (bytecodeSize > 0) {  
	  predicates[i].pBytecode = malloc (bytecodeSize);
	  fread (predicates[i].pBytecode, 1, bytecodeSize, pMeldProg);
	  skipNodeReferences (pMeldProg);
	} else {
	  //predicates[i].pBytecode = NULL;
	  predicates[i].codeSize = 1;
	  predicates[i].pBytecode = malloc (bytecodeSize);
	  predicates[i].pBytecode[0] = 0;
	}
      }

      /* ************* RULE INFORMATION ************* */

      /* Read rule bytecode */
#ifdef DEBUG_PARSER
      printf ("\nEXTRACTING RULE BYTECODE\n");
#endif

      uint32_t numRulesCode;
      fread (&numRulesCode, 4, 1, pMeldProg);
#ifdef DEBUG_PARSER
      printf ("Number of rule codes: %d\n", numRulesCode);
#endif

      for (i = 0; i < numRulesCode; ++i) {
	uint32_t ruleCodeSize;
	fread (&ruleCodeSize, 4, 1, pMeldProg);
	rules[i].codeSize = ruleCodeSize;

	rules[i].pBytecode = malloc (ruleCodeSize);
	fread (rules[i].pBytecode, 1, ruleCodeSize, pMeldProg);

	skipNodeReferences(pMeldProg);

	/* Read persistence */
	//fread (&rules[i].persistence, 1, 1, pMeldProg);
	rules[i].persistence = 0;

	/* Max number of preds by rule for parser set at 32 for now
	 * Check LMParser.h / rule struct to upgrade it if needed */
	fread (&rules[i].numInclPreds, 4, 1, pMeldProg);

	/* Get ID of each included predicate */
	for (j = 0; j < rules[i].numInclPreds; ++j) {
	  fread (&rules[i].inclPredIDs[j], 1, 1, pMeldProg);
	}

	/* Set descriptor size */
        rules[i].desc_size = RULE_DESCRIPTOR_SIZE + rules[i].numInclPreds;	
      }

      printf("Parsing done - Printing to output file %s\n", outNameBuf);

      /* ************* PRINT BYTE CODE HEADER ************* */

      /* Print byte code header to output file */

      printf ("-> Printing byte code header...\n");

      fprintf (pBBFile, "const unsigned char meld_prog[] = {");

      /* Print number of predicates */
      fprintf (pBBFile, "\n/* NUMBER OF PREDICATES */\n");
      fprintf (pBBFile, "%#x, ", numPredicates);
      
      /* Print number of rules */
      fprintf (pBBFile, "\n/* NUMBER OF RULES */\n");
      fprintf (pBBFile, "%#x, ", numRules);

      /* ************* PRINT OFFSETS TO DESCRIPTORS ************* */

      /* Calculate and print offset to predicate descriptor for every predicate */
      uint32_t descriptorStart = 2 + numPredicates * sizeof(unsigned short)	
	+ numRules * sizeof(unsigned short);
      uint32_t currentOffset = descriptorStart;
      fprintf (pBBFile, "\n/* OFFSETS TO PREDICATE DESCRIPTORS */");
      
      for (i = 0; i < numPredicates; ++i) {
	fprintf (pBBFile, "\n");
          
	/* Print offset */
	fprintf (pBBFile, "%#x, ", currentOffset & 0x00ff );
	fprintf (pBBFile, "%#x, ", (currentOffset & 0xff00) >> 8);

	// Increment current offset
	currentOffset += predicates[i].desc_size;
      }

      /* Calculate and print offset to rule descriptor for every rule */
      fprintf (pBBFile, "\n/* OFFSETS TO RULE DESCRIPTORS */");
      
      for (i = 0; i < numRules; ++i) {
	fprintf (pBBFile, "\n");
          
	/* Print offset */
	fprintf (pBBFile, "%#x, ", currentOffset & 0x00ff );
	fprintf (pBBFile, "%#x, ", (currentOffset & 0xff00) >> 8);

	// Increment current offset
	currentOffset += rules[i].desc_size;
      }

      /* Calculate byte code offsets */
      
      /* Start with offset to each predicate's bytecode */
      size_t bcOffset = currentOffset;
  
#ifdef DEBUG_PARSER
      printf ("Predicate byte code offsets: ");
#endif
      for (i = 0; i < numPredicates; ++i) {
#ifdef DEBUG_PARSER
	printf ("%zu ,", bcOffset);
#endif
	predicates[i].bytecodeOffset = bcOffset;
	bcOffset += predicates[i].codeSize;
      }
#ifdef DEBUG_PARSER
      printf ("\n");
#endif

      /* Then set rule offsets */
#ifdef DEBUG_PARSER
      printf ("Rule byte code offsets: ");
#endif
      for (i = 0; i < numRules; ++i) {
#ifdef DEBUG_PARSER
	printf ("%zu ,", bcOffset);
#endif
	rules[i].bytecodeOffset = bcOffset;
	bcOffset += rules[i].codeSize;
      }

      /* ************* PRINT PREDICATE DESCRIPTORS ************* */      

      fprintf (pBBFile, "\n/* PREDICATE DESCRIPTORS */");
      /* Print predicate descriptors */
      for (i = 0; i < numPredicates; ++i) {
	fprintf (pBBFile, "\n");
	
	/* Force printing 2 bytes of the offset */
	fprintf (pBBFile, "%#x, ", predicates[i].bytecodeOffset & 0x00ff );
	fprintf (pBBFile, "%#x, ", (predicates[i].bytecodeOffset & 0xff00) >> 8);

	/* Type properties */
	fprintf (pBBFile, "%#x, ", predicates[i].properties);

	/* Aggregate type */
	fprintf (pBBFile, "%#x, ", predicates[i].agg);

	/* Stratification rount */
	fprintf (pBBFile, "%#x, ", predicates[i].level);

	/* Number of arguments */
	fprintf (pBBFile, "%#x, ", predicates[i].nFields);

	/* Print argument descriptor */
	for (j = 0; j < predicates[i].nFields; ++j)
	  fprintf (pBBFile, "%#x, ", allArguments[predicates[i].argOffset + j]);
      }

      /* ************* PRINT RULE DESCRIPTORS ************* */

      fprintf (pBBFile, "\n/* RULE DESCRIPTORS */");

      for (i = 0; i < numRules; ++i) {
	fprintf (pBBFile, "\n");

	/* Force printing 2 bytes of the offset */
	fprintf (pBBFile, "%#x, ", rules[i].bytecodeOffset & 0x00ff );
	fprintf (pBBFile, "%#x, ", (rules[i].bytecodeOffset & 0xff00) >> 8);

	/* Persistence */
	fprintf (pBBFile, "%#x, ", rules[i].persistence);

	/* Number of included predicates */
	fprintf (pBBFile, "%#x, ", rules[i].numInclPreds);

	/* ID of each included predicate */
	for (j = 0; j < rules[i].numInclPreds; ++j)
	  fprintf (pBBFile, "%#x, ", rules[i].inclPredIDs[j]);
      }

      /* ************* PRINT PREDICATE BYTE CODE ************* */

      /* Print predicate bytecode */

      printf ("-> Printing predicate byte code...\n");

      fprintf (pBBFile, "\n/* PREDICATE BYTECODE */");
      for (i = 0; i < numPredicates; ++i) {
	fprintf (pBBFile, "\n/* Predicate %d: */", i);
	if (predicates[i].codeSize > 0) {
	  byte *pc =  predicates[i].pBytecode;
	  for (j = 0; j < predicates[i].codeSize; j++) {
	    fprintf (pBBFile, "%#x, ", *pc);
	    ++pc;
	  }
	}/* else {
	  fprintf(pBBFile, "%#x, ", 0);
	  //++pc;
	  }*/
      }

      /* ************* PRINT RULE BYTE CODE ************* */

      /* Print rule bytecode */

      printf ("-> Printing predicate byte code...\n");

      fprintf (pBBFile, "\n/* RULE BYTECODE */");
      for (i = 0; i < numRules; ++i) {
	fprintf (pBBFile, "\n/* Rule %d: */", i);
	byte *pc =  rules[i].pBytecode;
	for (j = 0; j < rules[i].codeSize; j++) {
	  fprintf (pBBFile, "%#x, ", *pc);
	  ++pc;
	}
      }

      // Close byte code array
      fprintf (pBBFile, "};\n");

      /* ************* PRINT PREDICATE NAMES ************* */

      /* Print predicate name array */

      printf ("-> Printing predicate names...\n");

      fprintf (pBBFile, "\nchar *tuple_names[] = {");
      for (i = 0; i < numPredicates; ++i) {
	char *sc = predicates[i].pName;
	fprintf (pBBFile, "\"");
	for (j = 0; (j < PRED_NAME_SIZE_MAX) && (*sc != 0x0); ++j) {
	  fprintf (pBBFile, "%c", *sc);
	  ++sc;
	}
	fprintf (pBBFile, "\"");
	fprintf (pBBFile, ", ");
      }
      fprintf (pBBFile, "};\n\n");

      /* ************* PRINT RULE NAMES ************* */

      /* Print rule string array */     

      printf ("-> Printing rule names...\n");
 
      fprintf (pBBFile, "char *rule_names[] = {");
      for (i = 0; i < numRules; ++i) {
	fprintf (pBBFile, "\"%s\", ", rules[i].pName);
      }
      fprintf (pBBFile, "};\n\n");

      /* ************* PRINT EXTERNAL FUNCTIONS ************* */

      /* Print remaining elements */

      printf ("-> Printing external functions (empty for now)...\n");

      fprintf (pBBFile, "#include \"extern_functions.bbh\"\n");
      fprintf (pBBFile, "Register (*extern_functs[])() = {};\n");
      fprintf (pBBFile, "\nint extern_functs_args[] = {};\n");

      fclose (pMeldProg);
      fclose (pBBFile);

      printf ("LMParser: Done.\n\n");
      return 0;  
  }
  return -4;
}

/* Reads a field type byte from source byte code file and return its equivalent
   for the targetVM
 */
byte
readType (FILE *pFile)
{
  byte fieldType;
  fread (&fieldType, 1, 1, pFile);

  switch (fieldType) {
  case FIELD_BOOL:    
#ifdef DEBUG_PARSER
    printf ("BOOL ");   
#endif
    return TFIELD_BOOL;
  case FIELD_INT:     
#ifdef DEBUG_PARSER
    printf ("INT ");    
#endif
    return TFIELD_INT;
  case FIELD_FLOAT:   
#ifdef DEBUG_PARSER
    printf ("FLOAT ");  
#endif
    return TFIELD_FLOAT;
  case FIELD_NODE:    
#ifdef DEBUG_PARSER
    printf ("NODE ");   
#endif
    return TFIELD_ADDR;
  case FIELD_STRING:  
#ifdef DEBUG_PARSER
    printf ("STRING "); 
#endif
    return TFIELD_STRING;
  case FIELD_LIST:
    {
      byte listType;
      fread (&listType, 1, 1, pFile);
      
      switch (listType) {
      case FIELD_INT:
#ifdef DEBUG_PARSER
	printf ("INTLIST ");
#endif
	return TFIELD_LIST_INT;
      case FIELD_FLOAT:
#ifdef DEBUG_PARSER
	printf ("FLOATLIST ");
#endif
	return TFIELD_LIST_FLOAT;
      case FIELD_NODE:
#ifdef DEBUG_PARSER
	printf ("NODELIST ");
#endif
	return TFIELD_LIST_ADDR;
      default:
	perror ("UNKNOWN LIST type!");
	exit (1);
      }
    }	
  case FIELD_STRUCT:    
    perror ("STRUCT type not supported yet!");
    exit (1);
  default:	
    perror ("UNKNOWN type!");
    exit (1);
  }
  return 0xff;
}

/* Returns the type byte stored at index pos of typeArray */
byte
readTypeID (FILE *pFile, byte typeArray[])
{
  byte pos;
  fread (&pos, 1, 1, pFile);
  return typeArray[pos];
}

/* There should never be any node reference as in BB programs there should
 * be always one single node, the block itself. 
 * This function is used to skip the node references
 */
void
skipNodeReferences (FILE *pFile)
{
  uint32_t sizeNodes;
  fread (&sizeNodes, 1, sizeof(uint32_t), pFile);
  fseek (pFile, sizeNodes * sizeof(uint32_t), SEEK_CUR);
}

/* Returns the total size of all rule descriptors */
size_t
sizeOfRuleDescriptors(Rule rules[], byte numRules)
{
  size_t i;
  size_t size = 0;
  for (i = 0; i < numRules; i++)
    size += rules[i].desc_size;
  
  return size;
}

/* Returns the total size of all predicate descriptors */
size_t
sizeOfPredicateDescriptors(Predicate predicates[], byte numPreds)
{
  size_t i;
  size_t size = 0;
  for (i = 0; i < numPreds; i++)
    size += predicates[i].desc_size;
  
  return size;
}
