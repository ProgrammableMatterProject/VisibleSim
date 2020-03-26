# GENERIC BlockCodes
SUBDIRS = c2d_demo c2srCatoms2D C3DRotate catoms3DRotations csgCatoms3D datomsRotate distanceRB locomotion lightWalkCatoms3D meld reconfCatoms3D reconfCatoms3DMultilayer robot01 sbReconf scaffolding_cube scaffolding_pyramid_async scaffolding_pyramid_sync simpleColorBB simpleColorC2D simpleColorOkteen simpleColorRB simpleColorSB sinkSource smartBlocks1 sphereCatoms3D targetColoration

.PHONY: subdirs $(SUBDIRS) test

subdirs: $(SUBDIRS)

# autoinstall not needed anymore
$(SUBDIRS):
	@mkdir -p ../applicationsBin/$@
	@echo "Checking $@ block code"
	@$(MAKE) -C $@ APPDIR=../../applicationsBin/$@ GLOBAL_INCLUDES="$(GLOBAL_INCLUDES)" GLOBAL_LIBS="$(GLOBAL_LIBS)" GLOBAL_CCFLAGS="$(GLOBAL_CCFLAGS)"

#subdirs:
#	@for dir in $(SUBDIRS); do \
#	mkdir -p ../applicationsBin/$$dir; \
#	$(MAKE) -C $$dir autoinstall APPDIR=../../applicationsBin/$$dir; \
#	done

test:
	@echo "\nBlockCodes Regression Testing:"
	@for dir in $(SUBDIRS); do \
		$(MAKE) -C $$dir test;\
	done

clean:
	rm -f *~ *.o
	@for dir in $(SUBDIRS); do \
	$(MAKE) -C $$dir APPDIR=../../applicationsBin/$$dir clean; \
	done

realclean:
	@for dir in $(SUBDIRS); do \
	rm -f ../../applicationsBin/$$dir/simulation.log; \
	done
