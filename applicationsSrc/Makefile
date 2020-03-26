# GENERIC BlockCodes
SUBDIRS = c2srCatoms2D  datomsRotate leader_election map03   meshCatoms3D_mvmt  reconfCatoms3DMultilayer   scaffolding_pyramid_sync  simpleColorSB  targetColoration bbCycle C3DRotate datomsTestbed lightWalkCatoms3D  map04   msrSync  robot02 simpleCatoms3D  simpleMotion blinky01   catoms3DRotations  distanceRB  lineC2D  meld musicPlayer   sbReconf simpleColorBB simpleMotionOkteen blinky02   cbLE edyLE  locomotion meltSortGrow node_demo scaffolding_csg  simpleColorC2D  simpleReconfC2D broadcast  coating  forcesPredictionIPPT  Makefile meshCatoms3D noneCatoms2D  scaffolding_cube simpleColorOkteen sinkSource c2d_demo   csgCatoms3D   hexanode_demo map02 meshCatoms3D_assembly  reconfCatoms3D scaffolding_pyramid_async  simpleColorRB   smartBlocks1


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
