SUBDIRS = rainbow changingDistance gameOfLife stressTestCatoms stressTestSB

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
