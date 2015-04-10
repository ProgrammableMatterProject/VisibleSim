/*
 * blinkyMeldBlockCode.h
 *
 *	Created on: 24 january 2015
 *			Author: Lucas Trônel
 */
	
Time MeldVM::myGetTime() {
#ifdef BBSIM
	struct timeb t;
	ftime(&t);
	return t.millitm + 1000 * (t.time % (1 <<	20));
#else
	 return getTime();
#endif
}

/* Print the content of the newTuples queue */
void MeldVM::print_newTuples(void) {
#ifdef BBSIM
	pthread_mutex_lock(&(printMutex));
#endif
	fprintf(stderr, "\x1b[34m--%d--\tContent of queue newTuples: \n", blockId);
	tuple_entry *tupleEntry;
	for (tupleEntry = newTuples->head; 
			 tupleEntry != NULL; 
			 tupleEntry = tupleEntry->next) {
		tuple_print(tupleEntry->tuple, stderr);
		fprintf(stderr, " -- isNew = %d\n", tupleEntry->records.count);
	}
	fprintf(stderr, "\x1b[0m");
#ifdef BBSIM
	pthread_mutex_unlock(&(printMutex));
#endif
}

/* Prints the content of the newStartTuples queue */
void MeldVM::print_newStratTuples() {
#ifdef BBSIM
	pthread_mutex_lock(&(printMutex));
#endif
	fprintf(stderr, "\x1b[34m--%d--\tContent of queue newStratTuples: \n",
		blockId);
	if (newStratTuples) {
		tuple_pentry *tupleEntry;
		for (tupleEntry = newStratTuples->queue; 
	 tupleEntry != NULL; 
	 tupleEntry = tupleEntry->next) {
			tuple_print(tupleEntry->tuple, stderr);
			fprintf(stderr, " -- isNew = %d\n", tupleEntry->records.count);
		}
	}
	fprintf(stderr, "\x1b[0m");
#ifdef BBSIM
	pthread_mutex_unlock(&(printMutex));
#endif
}

/* Gets ID of neighbor on face 'face' */
NodeID MeldVM::get_neighbor_ID(int face) {
	if (face == UP)
		return up();
	else if (face == DOWN)
		return down();
	else if (face == WEST)
		return west();
	else if (face == EAST)
		return east();
	else if (face == NORTH)
		return north();
	else if (face == SOUTH)
		return south();
	else {
		assert(0);
		return -1;
	}
}

/* Enqueue a tuple for execution */
void MeldVM::enqueueNewTuple(tuple_t tuple, record_type isNew) {
	assert (TUPLE_TYPE(tuple) < NUM_TYPES);

	if (TYPE_IS_STRATIFIED(TUPLE_TYPE(tuple))) {
		/* pthread_mutex_lock(&(printMutex)); */
		/* fprintf(stderr, "\x1b[1;35m--%d--\tStrat enqueuing tuple ", getBlockId()); */
		/* tuple_print (tuple, stderr); */
		/* fprintf(stderr, "\x1b[0m\n"); */
		/* pthread_mutex_unlock(&(printMutex)); */
		p_enqueue(newStratTuples, 
				TYPE_STRATIFICATION_ROUND(TUPLE_TYPE(tuple)), tuple, 0, isNew);
	}
	else {
		/* pthread_mutex_lock(&(printMutex)); */
		/* fprintf(stderr, "\x1b[1;35m--%d--\tBase enqueuing tuple ", getBlockId()); */
		/* tuple_print (tuple, stderr); */
		/* fprintf(stderr, "\x1b[0m\n"); */
		/* pthread_mutex_unlock(&(printMutex)); */
		queue_enqueue(newTuples, tuple, isNew);
	}
}

/* Enqueue a neighbor or vacant tuple */
void MeldVM::enqueue_face(NodeID neighbor, meld_int face, int isNew) {
	tuple_t tuple = NULL;

	if (neighbor <= 0) {
		 if(TYPE_VACANT == -1) /* no such predicate in the program */
				return;
		 tuple = tuple_alloc(TYPE_VACANT);
		 SET_TUPLE_FIELD(tuple, 0, &face);
	}
	else {
		 if(TYPE_NEIGHBOR == -1) /* no such predicate in the program */
				return;

		 tuple = tuple_alloc(TYPE_NEIGHBOR);
		 SET_TUPLE_FIELD(tuple, 0, &neighbor);
		 SET_TUPLE_FIELD(tuple, 1, &face);
	}
	enqueueNewTuple(tuple, (record_type) isNew);
}

/* Enqueue a neighborCount tuple */
void MeldVM::enqueue_count(meld_int count, int isNew) {
	 if(TYPE_NEIGHBORCOUNT == -1) /* no such predicate in the program */
			return;

	tuple_t tuple = tuple_alloc(TYPE_NEIGHBORCOUNT);
	SET_TUPLE_FIELD(tuple, 0, &count);
	enqueueNewTuple(tuple, (record_type) isNew);
}

/* Enqueue a tap tuple and also prints database if DEBUG is ON */
void MeldVM::enqueue_tap() {
	 if(TYPE_TAP == -1) /* no such predicate in the program */
			return;

	tuple_t tuple = tuple_alloc(TYPE_TAP);

	enqueueNewTuple(tuple, (record_type) 1);

	/* #if DEBUG */
	facts_dump();
	/* #endif */
}

/* Enqueue init tuple, triggers derivation of RULE 0, which derives axioms */
void MeldVM::enqueue_init(){
	if(TYPE_INIT == -1)
		return;

	tuple_t tuple = tuple_alloc(TYPE_INIT);
	enqueueNewTuple(tuple, (record_type) 1);
}

/* Saves the ID of useful types */
void MeldVM::init_all_consts() {
	init_consts();
	tuple_type i;

	for (i = 0; i < NUM_TYPES; i++) {
		if (strcmp(TYPE_NAME(i), "tap") == 0)
			TYPE_TAP = i;
		else if (strcmp(TYPE_NAME(i), "neighbor") == 0)
			TYPE_NEIGHBOR = i;
		else if ( (strcmp(TYPE_NAME(i), "neighborCount" ) == 0) ||
				(strcmp(TYPE_NAME(i), "neighborcount" ) == 0) )
			TYPE_NEIGHBORCOUNT = i;
		else if (strcmp(TYPE_NAME(i), "vacant") == 0)
			TYPE_VACANT = i;
	}	
}

/* The VM's main function */
void MeldVM::MeldVM(){
	vm_init();

	//block initialization
#if DEBUG
	printf("meld program started\n");
#endif

	//setColor(0);
	setLED(128,0,128,32);

	//print_program_info();

	/* Enqueue init to derive the program's axioms */
	enqueue_init();

	// introduce intial set of axioms
	NodeID neighbors[6];
	int numNeighbors = getNeighborCount();

	enqueue_count(numNeighbors, 1);
	
	int i;
	for (i = 0; i < NUM_PORTS; i++) {
		neighbors[i] = get_neighbor_ID(i);

		enqueue_face(neighbors[i], i, 1);
	}

	// loop forever, processing new facts and updating axioms
	while(1) {
		// loop for new facts to process

		if(!queue_is_empty(newTuples)) {
			int isNew = 0;
			tuple_t tuple = queue_dequeue(newTuples, &isNew);
			tuple_handle(tuple, isNew, reg);
		} else if (!p_empty(delayedTuples) 
				 && p_peek(delayedTuples)->priority <= myGetTime()) {
			tuple_pentry *entry = p_dequeue(delayedTuples);

			tuple_send(entry->tuple, entry->rt, 0, entry->records.count);
			free(entry);
		} else if (!(p_empty(newStratTuples))) {
			tuple_pentry *entry = p_dequeue(newStratTuples);
			tuple_handle(entry->tuple, entry->records.count, reg);
			
			free(entry);
		} else {
			/* If all tuples have been processed
			 * update rule state and process them if they are ready */
			for (i = 0; i < NUM_RULES; ++i) {

	if (updateRuleState(i)) {

		/* Set state byte used by DEBUG */
		byte processState = PROCESS_RULE | (i << 4);
		
		/* Don't process persistent rules (which is useless) 
		 * as they all have only a RETURN instruction.
		 */
		if (!RULE_ISPERSISTENT(i)) {
#ifdef DEBUG_RULES
			printf ("\n\x1b[35m--%d--\tRule %d READY!\x1b[0m\n", getBlockId(), i);
#endif
			/* Trigger execution */
		process_bytecode (NULL, RULE_START(i), 1, NOT_LINEAR, reg, processState);
		}
	}
	/* else: Rule not ready yet, will re-check at next main loop run */
			}

			// if we've processed everything, sleep for the sake of letting other blocks run in the simulator
			delayMS(30);
		}

		updateAccel();

		// update axioms based upon any changes
		int newNumNeighbors = getNeighborCount();
		if (newNumNeighbors != numNeighbors) {
			enqueue_count(numNeighbors, -1);
			numNeighbors = newNumNeighbors;
			enqueue_count(numNeighbors, 1);			
		}

		for (i = 0; i < NUM_PORTS; i++) {
			NodeID neighbor = get_neighbor_ID(i);

			if (neighbor == neighbors[i])
	continue;

#ifdef DEBUG_NEIGHBORHOOD
			printf ("--%d--\tNew neighbor %d on face %d!\n", blockId, neighbor, i);
#endif

			enqueue_face(neighbors[i], i, -1);

			/* Delete received tuples from database
			 * This may need to be reviewed, 
			 * I am not sure what LM is supposed to do with received tuples
			 */
			while(!queue_is_empty(&(receivedTuples[i]))) {
				 tuple_t tuple = queue_dequeue(&receivedTuples[i], NULL);
				 printf("--%d--\tDelete received ", blockId);
				 tuple_print(tuple, stdout);
				 printf("\n");
				 enqueueNewTuple(tuple, (record_type) -1);
			}

			neighbors[i] = neighbor;
			enqueue_face(neighbors[i], i, 1);
		}
	}
}

void MeldVM::userRegistration() {
	registerHandler(SYSTEM_MAIN, (GenericHandler)&meldMain);
	registerHandler(EVENT_ACCEL_TAP, (GenericHandler)&enqueue_tap);
}

#ifdef _LP64
void MeldVM::endian_swap(Register* x) {
	*x = (*x>>56) | 
		((*x<<40) & 0x00FF000000000000) |
		((*x<<24) & 0x0000FF0000000000) |
		((*x<<8)	& 0x000000FF00000000) |
		((*x>>8)	& 0x00000000FF000000) |
		((*x>>24) & 0x0000000000FF0000) |
		((*x>>40) & 0x000000000000FF00) |
		(*x<<56);
}
#endif

/* Receive a tuple and enqueue it to both receivedTuples and newTuples */
void MeldVM::receive_tuple(int isNew) {
	tuple_t rcvdTuple = (tuple_t)thisChunk->data;
	tuple_t tuple;
	tuple_type type = TUPLE_TYPE(rcvdTuple);
	size_t tuple_size = TYPE_SIZE(type);
	
#ifdef DEBUG_SEND
#ifdef BBSIM
	pthread_mutex_lock(&(printMutex));
#endif
	printf ("\x1b[33m--%d--\t Tuple received from %d: %s -- isNew = %d\x1b[0m\n",
		getBlockId(), get_neighbor_ID(faceNum(thisChunk)), 
		tuple_names[TUPLE_TYPE(rcvdTuple)], isNew);
/* tuple_print (rcvdTuple, stdout); */
#ifdef BBSIM
	pthread_mutex_unlock(&(printMutex));
#endif
#endif
	
	if(!TYPE_IS_LINEAR(type) && !TYPE_IS_ACTION(type)) {
		tuple_queue *queue = receivedTuples + faceNum(thisChunk);
		if(isNew > 0) {
			tuple = malloc(tuple_size);
			memcpy(tuple, rcvdTuple, tuple_size);
			queue_enqueue(queue, tuple, (record_type)isNew);
		} else {
			// delete tuple from queue because it must invalidate some other tuple
			tuple_entry **current;
			for (current = &queue->head;
				*current != NULL;
				current = &(*current)->next) {
			 if(memcmp((*current)->tuple, rcvdTuple, tuple_size) == 0) {
				 FREE_TUPLE(queue_dequeue_pos(queue, current));
				 break;
			 }
			}
		}
	}

	tuple = malloc(tuple_size);
	memcpy(tuple, rcvdTuple, tuple_size);
	enqueueNewTuple(tuple, (record_type)isNew);
}

/* Received tuple is a retraction fact */
void MeldVM::receive_tuple_delete() {
	receive_tuple(-1);
}

/* Received tuple is a normal fact */
void MeldVM::receive_tuple_add() {
	receive_tuple(1);
}

/* Does not looks like it actually do something */
void MeldVM::free_chunk() {
	free(thisChunk);
}

/* Sends a tuple to Block of ID rt, with or without delay */
void MeldVM::tuple_send(tuple_t tuple, NodeID rt, meld_int delay, int isNew) {
	assert (TUPLE_TYPE(tuple) < NUM_TYPES);

	if (delay > 0) {
		p_enqueue(delayedTuples, myGetTime() + delay, tuple, rt, (record_type) isNew);
		return;
	}

	NodeID target = rt;

#ifdef DEBUG_SEND
#ifdef BBSIM
	pthread_mutex_lock(&(printMutex));
#endif
	printf ("\x1b[33m--%d--\t Sending tuple: %s to %d -- isNew = %d\x1b[0m\n", 
		getBlockId(), tuple_names[TUPLE_TYPE(tuple)], target, isNew);
	/* tuple_print (tuple, stdout); */
#ifdef BBSIM
	pthread_mutex_unlock(&(printMutex));
#endif
#endif
	if (target == blockId) {
		enqueueNewTuple(tuple, (record_type) isNew);
	}
	else {
		int face = -1;

		if (target == up())
			face = UP;
		else if (target == down())
			face = DOWN;
		else if (target == west())
			face = WEST;
		else if (target == east())
			face = EAST;
		else if (target == north())
			face = NORTH;
		else if (target == south())
			face = SOUTH;

		if (face != -1) {			
			Chunk *c=calloc(sizeof(Chunk), 1);
			MsgHandler receiver;

			assert(c != NULL);

			if (isNew > 0) {
				receiver = (MsgHandler)receive_tuple_add;
			}
			else {
				receiver = (MsgHandler)receive_tuple_delete;
			}

			assert(TYPE_SIZE(TUPLE_TYPE(tuple)) <= 17);

			if (sendMessageToPort(c, face, tuple, TYPE_SIZE(TUPLE_TYPE(tuple)), (MsgHandler)receiver, (GenericHandler)&free_chunk) == 0) {
				// Send failed :(
				free(c);
				fprintf(stderr, "--%d--\tSEND FAILED EVEN THOUGH BLOCK IS PRESENT! TO %d\n", blockId, (int)target);
			}
		}
		else {
			/* This may happen when you delete a block in the simulator */
			fprintf(stderr, "--%d--\tUNABLE TO ROUTE MESSAGE! To %d\n", (int)blockId, (int)target);
			//exit(EXIT_FAILURE);
		}
		/* TODO-REAL: needs to free on real blinky blocks??? */
	}
}

/* Check if rule of ID rid is ready to be derived */
/* Returns 1 if true, 0 otherwise */
byte MeldVM::updateRuleState(byte rid) {
	int i;
	/* A rule is ready if all included predicates are present in the database */
	for (i = 0; i < RULE_NUM_INCLPREDS(rid); ++i) {
		if (TUPLES[RULE_INCLPRED_ID(rid, i)].length == 0)
			return INACTIVE_RULE;
	}

	/* Rule is ready, enqueue it or process it rightaway */
	return ACTIVE_RULE;
}

/* Simply calls tuple_do_handle located in core.c to handle tuple	*/
void MeldVM::tuple_handle(tuple_t tuple, int isNew, Register *registers) {
	tuple_type type = TUPLE_TYPE(tuple);
	assert (type < NUM_TYPES);

	/* if (isNew > 0 */
	/*		 && !inNeighborCountUpdate)	*/
	/*	 retractionOccured = 1; */

	tuple_do_handle(type, tuple, isNew, registers);
}

/* Used to call setColor functions from core.c */
void MeldVM::setColorWrapper(byte color) {
	setColor(color % NUM_COLORS);
}

/* Call itself ? */
void MeldVM::setLEDWrapper(byte r, byte g, byte b, byte intensity) {
	setLEDWrapper(r, g, b, intensity);
}

/* Used to get blockId from core.c */
NodeID MeldVM::getBlockId(void) { 
	return blockId; 
}

/* VM initialization routine */
void MeldVM::vm_init() {
#ifdef BBSIM
	// We only want to do this once per executable, so in simulator make sure it happens only once and everyone else waits for it to complete.
	if (alreadyExecuted(0)) return;
#endif

	fprintf(stderr, "In VM_init\n");
	init_all_consts();
	init_fields();
#if DEBUG
	print_program_info();
#endif

#ifdef BBSIM
	// indicate that the vm is inited.
	alreadyExecuted(1);
#endif
}

/* Called upon block init (block.bb) 
 * to ensure that data structures are allocated before
 * VM start in case other blocks send us tuples - Would seg fault otherwise */
void MeldVM::vm_alloc() {
	/* Get node ID */
	blockId = getGUID();

	// init stuff
	tuples = calloc(NUM_TYPES, sizeof(tuple_queue));
	newTuples = calloc(1, sizeof(tuple_queue));
	newStratTuples = calloc(1, sizeof(tuple_pqueue));
	delayedTuples = calloc(1, sizeof(tuple_pqueue));

	assert(tuples!=NULL);
	assert(newTuples!=NULL);
	assert(newStratTuples!=NULL);
	assert(delayedTuples!=NULL);

	/* Reset received tuples queue */
	memset(receivedTuples, 0, sizeof(tuple_queue) * NUM_PORTS);

#ifdef BBSIM
	pthread_mutex_init(&(printMutex), NULL);
#endif
}

#ifndef BBSIM
void MeldVM::__myassert(char* file, int line, char* exp) {
#ifdef LOG_DEBUG
//{
	char str[50];
	sprintf(str, "assert %s:%d %s", file, line, exp);
	printDebug(str);
//}
#endif
	while (1) {
		setColor(RED); 
		delayMS(50); 
		setColor(BLUE); 
		delayMS(50);
	}
}
#endif