# make -f test.make FILE=<srcname> [TARGET=<mpi|bbsim|serial|mcore>] [NODES=<number>] [update]
#
# FILE = the meld program to test
# TARGET = the api used to do the test
# NODES = if relevant, the number of nodes for the target
# update = to update the reference output
#
# FILE=<srcname> must be specified, all others are optional.  if not specified, then TARGET=mpi NODES=1
# will make sure <srcname> is compiled and run the test and compare to the reference output
#
# <srcname> = a meld file in the progs directory
# 

# figure out the source file & compiled file
srcpath=$(FILE)
srcname=$(notdir $(srcpath))
basename=$(basename $(srcname))
mfile=code/$(basename)

# default target to test.  user can specify alternatives
TARGET=mpi

# if relevant, number of nodes to test on
NODES=1

top:	exec-$(TARGET)

exec-bbsim:	$(mfile).m files/$(basename).test
	@./mtest.sh "mpiexec -n $(NODES) ../meld-$(TARGET) -a bbsim -d -f $(mfile).m -c sl" $(basename)
	this needs to be fixed

exec-mpi:	$(mfile).m files/$(basename).test
	@./mtest.sh "mpiexec -n $(NODES) ../meld-$(TARGET) -a mpi -d -f $(mfile).m -c sl" $(basename)

$(mfile).m:	$(srcpath)
	./mcl.sh $(srcpath) $(mfile)
