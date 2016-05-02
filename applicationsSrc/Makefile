#SUBDIRS = flavio01 blinky01 blinky02 smartBlocks1 leader_election coloration\
#          coloration_v2

SUBDIRS = simpleColorC2D simpleColorRB
#SUBDIRS = csgCatoms3D sbReconf

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
