SUBDIRS = noneCatoms2D bbCycle robot01 smartBlocks1 sphereCatoms3D

# SUBDIRS =  blinkyMeld blinky01 bbCycle sbReconf coloration coloration_v2 leader_election smartBlocks1 detection robot02 map02 map03 map04 mElection01 reconfCatoms2D ABCcenterC2D csgCatoms3D sphereCatoms3D msrSync musicPlayer noneCatoms2D robot01 robot02 simpleCatoms3D simpleColorC2D simpleColorRB

.PHONY: subdirs $(SUBDIRS)

subdirs: $(SUBDIRS)

$(SUBDIRS):
	@mkdir -p ../applicationsBin/$@
	$(MAKE) -C $@ autoinstall APPDIR=../../applicationsBin/$@ GLOBAL_INCLUDES="$(GLOBAL_INCLUDES)" GLOBAL_LIBS="$(GLOBAL_LIBS)" GLOBAL_CCFLAGS="$(GLOBAL_CCFLAGS)"

#subdirs:
#	@for dir in $(SUBDIRS); do \
#	mkdir -p ../applicationsBin/$$dir; \
#	$(MAKE) -C $$dir autoinstall APPDIR=../../applicationsBin/$$dir; \
#	done

clean:
	rm -f *~ *.o
	@for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir clean; \
	done
