
include conf.mk

OS = $(shell uname -s)

INCLUDE_DIRS = -I.
LIBRARY_DIRS =

#to remove depricated char* warnings
NOSTRINGWARN = -Wno-write-strings

ARCH = -march=x86-64
#ARCH = -march=armv6
ifeq ($(RELEASE), true)
	DEBUG =
	OPTIMIZATIONS = -O3 -DNDEBUG
else
	DEBUG = -g
	PROFILING = -pg
	OPTIMIZATIONS = -O0
endif

WARNINGS = -Wall -Wextra #-Werror
C0X = -std=c++0x


ifeq ($(INTERFACE),true)
	LIBRARIES = -lwebsocketpp -ljson_spirit
	CFLAGS += -DUSE_UI=1
endif

CFLAGS = $(ARCH) $(PROFILING) $(OPTIMIZATIONS) $(WARNINGS) $(DEBUG) $(INCLUDE_DIRS) $(C0X) $(NOSTRINGWARN) -DUSERFRIENDLY=1
LIBRARIES = -pthread -lpthread -lm  -lboost_thread -lboost_system \
				-lboost_date_time -lboost_regex	\
				-ldl $(UILIBRARIES)

GCC_MINOR    := $(shell $(CXX) -v 2>&1 | grep " version " | cut -d' ' -f3  | cut -d'.' -f2)

ifeq ($(GCC_MINOR),2)
	CFLAGS += -DTEMPLATE_OPTIMIZERS=1
endif
ifeq ($(GCC_MINOR),4)
	CFLAGS += $(C0X)
endif

CXXFLAGS = $(CFLAGS)
LDFLAGS = $(PROFILING) $(LIBRARY_DIRS) $(LIBRARIES)
COMPILE = $(CXX) $(CXXFLAGS) 

SRCS = 	utils/utils.cpp \
	utils/types.cpp \
	utils/fs.cpp \
	vm/program.cpp \
	vm/predicate.cpp \
	vm/types.cpp \
	vm/instr.cpp \
	vm/state.cpp \
	vm/tuple.cpp \
	vm/exec.cpp \
	vm/external.cpp \
	vm/rule.cpp \
	vm/rule_matcher.cpp \
	vm/stat.cpp \
	db/node.cpp \
	db/tuple.cpp \
	db/agg_configuration.cpp \
	db/tuple_aggregate.cpp \
	db/database.cpp \
	db/trie.cpp \
	process/machine.cpp \
	mem/thread.cpp \
	mem/center.cpp \
	mem/stat.cpp \
	sched/base.cpp \
	sched/common.cpp \
	sched/serial.cpp \
	external/math.cpp \
	external/lists.cpp \
	external/utils.cpp \
	external/strings.cpp \
	external/others.cpp \
	external/core.cpp \
	stat/stat.cpp \
	stat/slice.cpp \
	stat/slice_set.cpp \
	interface.cpp \
	runtime/common.cpp \
	debug/debug_prompt.cpp \
	./meld.cpp \
	debug/debug_handler.cpp \
	debug/debug_list.cpp \
	#sched/thread/threaded.cpp \
	#sched/thread/assert.cpp \

export

all:	meld-bbsim #meld-mpi

meld-%:	FORCE
	@echo "Making meld-$* with Makefile.$*"
	@mkdir -p target/$*
	@$(MAKE) --no-print-directory -f Makefile.$* TARGET=$*

print:

clean:
	/bin/rm -rf target

FORCE:


