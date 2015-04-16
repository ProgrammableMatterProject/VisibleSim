#include "meldInterpretVM.h"
#include "meldInterpretVMCore.h"
#include "meldInterpretEvents.h"
#include "meldInterpretMessages.h"
#include "meldInterpretScheduler.h"
#include "events.h"
#include "blinkyBlocksBlock.h"

namespace MeldInterpret{

map<int, MeldInterpretVM*> MeldInterpretVM::vmMap;
const unsigned char* MeldInterpretVM::meld_prog;
bool MeldInterpretVM::configured = false;
bool MeldInterpretVM::debugging = false;


	MeldInterpretVM::MeldInterpretVM(BlinkyBlocks::BlinkyBlocksBlock *b){
		vm_init();
		host = b;
		blockId = (NodeID)b->blockId;
		hasWork = true;
		polling = false;
		currentLocalDate = myGetTime();
		//block initialization
		//setColor(0);
		setLED(128,0,128,32);

		/* Enqueue init to derive the program's axioms */
		enqueue_init();

		// introduce initial set of axioms
		numNeighbors = getNeighborCount();
		enqueue_count(numNeighbors, 1);

		for (int i = 0; i < NUM_PORTS; i++) {
			neighbors[i] = get_neighbor_ID(i);

			enqueue_face(neighbors[i], i, 1);
		}
		vmMap.insert(std::pair<int, MeldInterpretVM*>(blockId,this));
	}

	MeldInterpretVM::~MeldInterpretVM(){
	}

	Time MeldInterpretVM::myGetTime(){
	   return (Time)host->getTime();
	}


	/* Gets ID of neighbor on face 'face' */
	inline NodeID MeldInterpretVM::get_neighbor_ID(int face){
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
	void MeldInterpretVM::enqueueNewTuple(tuple_t tuple, record_type isNew){
	  assert (TUPLE_TYPE(tuple) < NUM_TYPES);

	  if (TYPE_IS_STRATIFIED(TUPLE_TYPE(tuple))) {
	    p_enqueue(newStratTuples, TYPE_STRATIFICATION_ROUND(TUPLE_TYPE(tuple)), tuple, 0, isNew);
	  }
	  else {
	    queue_enqueue(newTuples, tuple, isNew);
	  }
	}

	/* Enqueue a neighbor or vacant tuple */
	void MeldInterpretVM::enqueue_face(NodeID neighbor, meld_int face, int isNew){
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
	void MeldInterpretVM::enqueue_count(meld_int count, int isNew){
	   if(TYPE_NEIGHBORCOUNT == -1) /* no such predicate in the program */
	      return;

	  tuple_t tuple = tuple_alloc(TYPE_NEIGHBORCOUNT);

	  SET_TUPLE_FIELD(tuple, 0, &count);

	  enqueueNewTuple(tuple, (record_type) isNew);
	}

	/* Enqueue a tap tuple */
	void MeldInterpretVM::enqueue_tap(void) {
	   if(TYPE_TAP == -1) /* no such predicate in the program */
	      return;

	  tuple_t tuple = tuple_alloc(TYPE_TAP);

	  enqueueNewTuple(tuple, (record_type) 1);

	}

	/* Enqueue init tuple, triggers derivation of RULE 0, which derives axioms */
	void MeldInterpretVM::enqueue_init(void) {
	  if(TYPE_INIT == -1)
	    return;

	  tuple_t tuple = tuple_alloc(TYPE_INIT);
	  enqueueNewTuple(tuple, (record_type) 1);
	}

	/* Saves the ID of useful types */
	void MeldInterpretVM::init_all_consts(void) {
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
	void MeldInterpretVM::processOneRule(void) {
	  // processing new facts and updating axioms
	    // loop for new facts to process
		while(1){
			//If there are new tuples
		    if(!queue_is_empty(newTuples)) {
		      int isNew = 0;
		      tuple_t tuple = queue_dequeue(newTuples, &isNew);
		      tuple_handle(tuple, isNew, reg);
			//Else if there are new delayed tuple it's priority is inferior at the time
		    } else if (!p_empty(delayedTuples) && p_peek(delayedTuples)->priority <= myGetTime()) {
		      tuple_pentry *entry = p_dequeue(delayedTuples);

		      tuple_send(entry->tuple, entry->rt, 0, entry->records.count);
		      free(entry);
			//Else if there are new stratified tuple
		    } else if (!(p_empty(newStratTuples))) {
		      tuple_pentry *entry = p_dequeue(newStratTuples);
		      tuple_handle(entry->tuple, entry->records.count, reg);

		      free(entry);
			//Else if there are no tuple to process
		    } else {
				/* If all tuples have been processed
				* update rule state and process them if they are ready */
				waiting = 0;
				for (int i = 0; i < NUM_RULES; ++i) {
					//If a rule has all its predicate (considered ACTIVE)
					if (updateRuleState(i)) {

						/* Set state byte used by DEBUG */
						byte processState = PROCESS_RULE | (i << 4);

						/* Don't process persistent rules (which is useless)
						* as they all have only a RETURN instruction.
						*/
						if (!RULE_ISPERSISTENT(i)) {

							/* Trigger execution */
							process_bytecode (NULL, RULE_START(i), 1, NOT_LINEAR, reg, processState);

							/* After one rule is executed we set the VM on waiting until next call of scheduler*/
							waiting = 1;
							i = NUM_RULES;
						}
					}
					/* else: Rule not ready yet, set status to not waiting until new fact appear */
				}

				// if we've processed everything, sleep for the sake of letting other blocks run in the simulator
				// Not necessary with VisibleSim integration ?
				//delayMS(30);
		    }

		    //updateAccel();

		    // update axioms based upon any changes
		    int newNumNeighbors = getNeighborCount();
		    if (newNumNeighbors != numNeighbors) {
		      enqueue_count(numNeighbors, -1);
		      numNeighbors = newNumNeighbors;
			waiting = 1;
		      enqueue_count(numNeighbors, 1);
		    }

		    for (int i = 0; i < NUM_PORTS; i++) {
			      NodeID neighbor = get_neighbor_ID(i);

			      if (neighbor == neighbors[i])
				continue;

				waiting = 1;
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

	bool MeldInterpretVM::isWaiting(){
		return waiting > 0;
	}


	/*void MeldInterpretVM::userRegistration(void) {
	  registerHandler(SYSTEM_MAIN, (GenericHandler)&meldMain);
	  registerHandler(EVENT_ACCEL_TAP, (GenericHandler)&enqueue_tap);
	}*/

	/* Receive a tuple and enqueue it to both receivedTuples and newTuples */
	void MeldInterpretVM::receive_tuple(int isNew, tuple_t tpl, byte face) {
	  tuple_t rcvdTuple = (tuple_t)tpl;
	  tuple_t tuple;
	  tuple_type type = TUPLE_TYPE(rcvdTuple);
	  size_t tuple_size = TYPE_SIZE(type);

	  if(!TYPE_IS_LINEAR(type) && !TYPE_IS_ACTION(type)) {
	     tuple_queue *queue = receivedTuples + face;
	     if(isNew > 0) {
		tuple = malloc(tuple_size);
		memcpy(tuple, rcvdTuple, tuple_size);
		queue_enqueue(queue, tuple, (record_type)isNew);
	     } else {
		// delete tuple from queue because it must invalidate some other tuple
		tuple_entry **current;
		for (current = &queue->head; *current != NULL; current = &(*current)->next) {
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

	/* Sends a tuple to Block of ID rt, with or without delay */
	void MeldInterpretVM::tuple_send(tuple_t tuple, NodeID rt, meld_int delay, int isNew) {
	  assert (TUPLE_TYPE(tuple) < NUM_TYPES);

	  if (delay > 0) {
	    p_enqueue(delayedTuples, myGetTime() + delay, tuple, rt, (record_type) isNew);
	    return;
	  }

	  NodeID target = rt;

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

            assert(TYPE_SIZE(TUPLE_TYPE(tuple)) <= 17);
            MessagePtr ptr;

	      if (isNew > 0) {
                  ptr = MessagePtr(new AddTupleMessage(tuple));
	      }
	      else {
                  ptr = MessagePtr(new RemoveTupleMessage(tuple));
	      }

            P2PNetworkInterface *p2p = host->getP2PNetworkInterfaceByDestBlockId(neighbors[face]);
            /*Prepare message*/
            ptr->sourceInterface = p2p;
            ptr->destinationInterface  = p2p->connectedInterface;
            MeldInterpret::getScheduler()->schedule(new VMSendMessageEvent(MeldInterpret::getScheduler()->now(), host, ptr, p2p));
	    }
	    else {
	      /* This may happen when you delete a block in the simulator */
	      fprintf(stderr, "--%d--\tUNABLE TO ROUTE MESSAGE! To %d\n", (int)blockId, (int)target);
	      //exit(EXIT_FAILURE);
	    }
	  }
	}

	/* Check if rule of ID rid is ready to be derived */
	/* Returns 1 if true, 0 otherwise */
	byte MeldInterpretVM::updateRuleState(byte rid) {
	  int i;
	  /* A rule is ready if all included predicates are present in the database */
	  for (i = 0; i < RULE_NUM_INCLPREDS(rid); ++i) {
	    if (TUPLES[RULE_INCLPRED_ID(rid, i)].length == 0)
	      return INACTIVE_RULE;
	  }

	  /* Rule is ready, enqueue it or process it rightaway */
	  return ACTIVE_RULE;
	}

	/* Simply calls tuple_do_handle located in core.c to handle tuple  */
	void MeldInterpretVM::tuple_handle(tuple_t tuple, int isNew, Register *registers) {
	  tuple_type type = TUPLE_TYPE(tuple);
	  assert (type < NUM_TYPES);

	  tuple_do_handle(type, tuple, isNew, registers);
	}

	/* Used to get blockId from core.c */
	NodeID MeldInterpretVM::getBlockId (void) { return blockId; }

	/* VM initialization routine */
	void MeldInterpretVM::vm_init(void) {

	  fprintf(stderr, "In VM_init\n");

	  init_all_consts();
	  init_fields();

	}

	/* Called upon block init (block.bb)
	 * to ensure that data structures are allocated before
	 * VM start in case other blocks send us tuples - Would seg fault otherwise */
	void MeldInterpretVM::vm_alloc(void) {

	  // init stuff
	  tuples = (tuple_queue*)calloc(NUM_TYPES, sizeof(tuple_queue));
	  newTuples = (tuple_queue*)calloc(1, sizeof(tuple_queue));
	  newStratTuples = (tuple_pqueue*)calloc(1, sizeof(tuple_pqueue));
	  delayedTuples = (tuple_pqueue*)calloc(1, sizeof(tuple_pqueue));

	  assert(tuples!=NULL);
	  assert(newTuples!=NULL);
	  assert(newStratTuples!=NULL);
	  assert(delayedTuples!=NULL);

	  /* Reset received tuples queue */
	  memset(receivedTuples, 0, sizeof(tuple_queue) * NUM_PORTS);

	}

    byte MeldInterpretVM::getNeighborCount() {
        uint8_t count, i;

        for(count = 0, i = 0; i < NUM_PORTS; ++i) {
            if(neighbors[i] != VACANT) {
                count++;
            }
        }
        return count;
    }

// simple functions to access geographic neighbors
    Uid MeldInterpretVM::down(void) {
        return neighbors[DOWN];
    }
    Uid MeldInterpretVM::up(void) {
        return neighbors[UP];
    }
    Uid MeldInterpretVM::north(void) {
        return neighbors[NORTH];
    }
    Uid MeldInterpretVM::south(void) {
        return neighbors[SOUTH];
    }
    Uid MeldInterpretVM::east(void) {
        return neighbors[EAST];
    }
    Uid MeldInterpretVM::west(void) {
        return neighbors[WEST];
    }

    NodeID MeldInterpretVM::getGUID(){
      return blockId;
    }

      inline void MeldInterpretVM::setColor(Color color){
            setLED(color[0]*255, color[1]*255, color[2]*255, color[3]*255);
      }
      void MeldInterpretVM::setLED(byte r, byte g, byte b, byte intensity){
            BaseSimulator::getScheduler()->schedule(new SetColorEvent(BaseSimulator::getScheduler()->now(), host , (float)r/255, (float)g/255, (float)b/255, (float)intensity/255));
      }

      bool MeldInterpretVM::dateHasBeenReachedByAll(uint64_t date) {
		static uint64_t minReallyReached = 0;
		uint64_t min, min2;
		int alive = 0, hasNoWork = 0;

		if (date < minReallyReached) {
			return true;
		}

		map<int, MeldInterpretVM*>::iterator it;
		for(it = vmMap.begin(); it != vmMap.end(); it++) {
			MeldInterpretVM *vm = it->second;
			BuildingBlock *buildb = vm->host;
			if (buildb->getState() < BuildingBlock::ALIVE) {
				continue;
			}
			alive++;
			if (!vm->hasWork || vm->polling) {
				hasNoWork++;
				if (alive == 1) {
					min2 = vm->currentLocalDate;
				} else if (vm->currentLocalDate < min2) {
					min2 = vm->currentLocalDate;
				}
			} else {
				if ((alive - 1) == hasNoWork) {
					min = vm->currentLocalDate;
				} else if (vm->currentLocalDate < min) {
					min = vm->currentLocalDate;
				}
				if (min < min2) {
					min2 = min;
				}
			}
		}
		if (alive==hasNoWork) {
			return true;
		}
		minReallyReached = min2;
		return (date < min);
	}


      bool MeldInterpretVM::equilibrium() {
		map<int, MeldInterpretVM*>::iterator it;
		for(it = vmMap.begin(); it != vmMap.end(); it++) {
			MeldInterpretVM *vm = it->second;
			BuildingBlock *buildb = vm->host;
			if (buildb->getState() < BuildingBlock::ALIVE) {
				continue;
			}
			if (vm->hasWork) {
				return false;
			}
		}
		return true;
	}

      void MeldInterpretVM::setConfiguration(string path, bool d){
            debugging = d;
            if(!configured){
                  readProgram(path);
            }
            configured = true;
      }

      void MeldInterpretVM::readProgram(string path){
            ifstream in(path.c_str(), ios::in | ios::binary | ios::ate);
            stringstream sstr;
            sstr << in.rdbuf();
            meld_prog = (const unsigned char*) sstr.str().c_str();
      }

}
