SUBDIRS =  blinkyMeld blinky01 bbCycle sbReconf coloration coloration_v2 leader_election smartBlocks1 detection robot02 map02 map03 map04 mElection01 reconfCatoms2D csgCatoms3D ABCcenterC2D ABCcenterC3D csgCatoms3D sphereCatoms3D sphereBlinky 

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
