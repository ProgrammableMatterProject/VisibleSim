# GENERIC BlockCodes
<<<<<<< HEAD
SUBDIRS = #C3DRotate #simpleColorC2D simpleColorRB simpleColorSB distanceRB simpleCatoms3D simpleColorBB
#SUBDIRS = simpleCatoms3D simpleColorSB simpleColorC2D simpleMotion
SUBDIRS = datomsRotate datomsTestbed
# simpleColorOkteen simpleMotionOkteen simpleCatoms3D

# MELD BlockCodes
#SUBDIRS += meld MRMeld

# TESTED Legacy codeBlocks
#SUBDIRS += noneCatoms2D robot01 sphereCatoms3D sbReconf smartBlocks1 bbCycle bbCycle-bak msrSync reconfCatoms2D csgCatoms3D coloration
#SUBDIRS += noneCatoms2D robot01 sphereCatoms3D sbReconf smartBlocks1 bbCycle bbCycle-bak msrSync c2srCatoms2D csgCatoms3D coloration
# ALL
# SUBDIRS =  blinkyMeld blinky01 bbCycle sbReconf coloration coloration_v2 leader_election smartBlocks1 detection map02 map03 map04 mElection01 reconfCatoms2D ABCcenterC2D csgCatoms3D sphereCatoms3D msrSync musicPlayer noneCatoms2D robot01 robot02 simpleCatoms3D simpleColorC2D simpleColorRB
SUBDIRS += #meshCatoms3D_assembly meshCatoms3D #csgCatoms3D meltSortGrow targetColoration simpleMotionOkteen reconfCatoms3D
# SUBDIRS = reconfCatoms3D reconfCatoms3DMultilayer
SUBDIRS += csgCatoms3D catoms3DRotate
=======
SUBDIRS = C3DRotate hexanode_demo meld reconfCatoms3DMultilayer scaffolding_pyramid_async simpleColorC2D csgCatoms3D lightWalkCatoms3D musicPlayer robot02 scaffolding_pyramid_sync simpleColorOkteen simpleOkteenMotion c2d_demo datomsRotate maxFlowSC node_demo sbReconf simpleColorSC sinkSource c2srCatoms2D distanceSC reconfCatoms3D scaffolding_cube simpleColorBB simpleColorSB molding coating scaffolding_csg datomsTestbed
>>>>>>> 730896439aebaf1a83364403d2d7b40eb7d1ddd1

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
