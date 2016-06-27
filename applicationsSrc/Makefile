SUBDIRS = simpleColorRB simpleCatoms3D simpleColorC2D simpleColorBB
#SUBDIRS = noneCatoms2D bbCycle robot01 sphereCatoms3D sbReconf smartBlocks1

# SUBDIRS =  blinkyMeld blinky01 bbCycle sbReconf coloration coloration_v2 leader_election smartBlocks1 detection map02 map03 map04 mElection01 reconfCatoms2D ABCcenterC2D csgCatoms3D sphereCatoms3D msrSync musicPlayer noneCatoms2D robot01 robot02 simpleCatoms3D simpleColorC2D simpleColorRB

.PHONY: subdirs $(SUBDIRS) test

subdirs: $(SUBDIRS)

# autoinstall not needed anymore
$(SUBDIRS):
	@mkdir -p ../applicationsBin/$@
	$(MAKE) -C $@ APPDIR=../../applicationsBin/$@ GLOBAL_INCLUDES="$(GLOBAL_INCLUDES)" GLOBAL_LIBS="$(GLOBAL_LIBS)" GLOBAL_CCFLAGS="$(GLOBAL_CCFLAGS)"

#subdirs:
#	@for dir in $(SUBDIRS); do \
#	mkdir -p ../applicationsBin/$$dir; \
#	$(MAKE) -C $$dir autoinstall APPDIR=../../applicationsBin/$$dir; \
#	done

test:	subdirs
	@for dir in $(SUBDIRS); do \
		echo "(cd ../applicationsBin/$$dir; ./$$dir)"; \
		(cd ../applicationsBin/$$dir; ./$$dir); \
	done

clean:
	rm -f *~ *.o
	@for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir clean; \
	done
