SUBDIRS = e1_flooding
# C3DRotate hexanodes_demo meld reconfCatoms3DMultilayer scaffolding_pyramid_async simpleColorC2D csgCatoms3D lightWalkCatoms3D musicPlayer robot02 scaffolding_pyramid_sync c2d_demo datomsRotate maxFlowSC nodes2D_demo sbReconf simpleColorSC sinkSource c2srCatoms2D distanceSC reconfCatoms3D scaffolding_cube simpleColorBB simpleColorSB molding datomsTestbed gameOfLife rainbow tetris

.PHONY: subdirs $(SUBDIRS) test

subdirs: $(SUBDIRS)

# autoinstall not needed anymore
$(SUBDIRS):
	@mkdir -p ../applicationsBin/$@
	@echo "Checking $@ block code"
	@$(MAKE) -C $@ APPDIR=../../applicationsBin/$@ GLOBAL_INCLUDES="$(GLOBAL_INCLUDES)" GLOBAL_LIBS="$(GLOBAL_LIBS)" GLOBAL_CCFLAGS="$(GLOBAL_CCFLAGS)"

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
