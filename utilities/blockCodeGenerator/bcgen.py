import os, sys, getopt

def makeDir(path):
    try:
        os.mkdir(path)
    except OSError:
        print ("Creation of the directory %s failed" % path)
        if os.path.isdir(path):
            print ("Directory exists: %s" % path)
            sys.exit(1)
    else:
        print ("Successfully created the directory %s " % path)

def createSourceFile(tagsFile, outFile):
    with open(tagsFile, "rt") as fin:
        with open(outFile, "wt") as fout:
            for line in fin:
                fout.write(line
                           .replace('<<moduleName>>', module)
                           .replace('<<moduleNameLc>>', moduleNameLc) # lowercase
                           .replace('<<appName>>', appName)
                           .replace('<<appNameLc>>', appNameLc)) # lowercase
    print ("Successfully created file %s " % outFile)

def print_usage():
    print ("\nusage:\tpython bcgen.py --app <APPNAME> --module <MODULENAME>")
    print ("\tpython bcgen.py -a <APPNAME> -m <MODULENAME>")
    print ("\nModule options: Catoms3D, Catoms2D, CatomsR, Datoms, BlinkyBlocks, SmartBlocks, RobotBlocks, Okteens, MultiRobots")

# Main variables
appName = None
module = None
moduleOptions = ["Catoms3D", "Catoms2D", "CatomsR", "Datoms", "BlinkyBlocks",
                 "SmartBlocks", "RobotBlocks", "Okteens", "MultiRobots"]

# Parse arguments
unixOptions = "a:m:hv"
gnuOptions = ["app=", "module=", "verbose", "help"]
verbose = False

# read commandline arguments, first
fullCmdArguments = sys.argv

# - further arguments
argumentList = fullCmdArguments[1:]

try:
    arguments, values = getopt.getopt(argumentList, unixOptions, gnuOptions)
except getopt.error as err:
    # output error, and return with an error code
    print (str(err))
    sys.exit(2)

# evaluate given options
for currentArgument, currentValue in arguments:
    if currentArgument in ("-h", "--help"):
        print_usage()
        sys.exit(0)
    elif currentArgument in ("-v", "--verbose"):
        verbose = True
        print ("enabling verbose mode")
    elif currentArgument in ("-a", "--app"):
        appName = currentValue
        # correct for uppercase
        appName = appName[0].upper() + appName[1:]

        print (("Supplied AppName: (%s)") % (appName))
    elif currentArgument in ("-m", "--module"):
        module = currentValue
        if module in moduleOptions:
            print (("Supplied ModuleName: (%s)") % (module))
        else:
            print (("error: invalid module option: (%s)") % (module))
            module = None

# Ensure arguments are correctly initialized
if appName is None or module is None:
    print ("error: Please provide --app and --module arguments")
    print_usage()
    sys.exit(2)

# Set tag variables
appNameLc = appName[0].lower() + appName[1:]
moduleNameLc = module[0].lower() + module[1:]

# Set working paths
path = os.getcwd()
vsrootPath = os.path.dirname(os.path.dirname(path))
vsAppSrcPath = os.path.join(vsrootPath, "applicationsSrc")
vsAppBinPath = os.path.join(vsrootPath, "applicationsBin")

srcPath = os.path.join(vsAppSrcPath, appNameLc)
binPath = os.path.join(vsAppBinPath, appNameLc)

appCpp = os.path.join(srcPath, appNameLc + ".cpp")
appBlockCodeCpp = os.path.join(srcPath, appNameLc + "BlockCode.cpp")
appBlockCodeHpp = os.path.join(srcPath, appNameLc + "BlockCode.hpp")
appMakefile = os.path.join(srcPath, "Makefile")

# Create target directories
makeDir(srcPath)
makeDir(binPath)

# Create app sources
createSourceFile("sample/sample.cpp", appCpp)
createSourceFile("sample/sampleBlockCode.cpp", appBlockCodeCpp)
createSourceFile("sample/sampleBlockCode.hpp", appBlockCodeHpp)

# Create Makefile
createSourceFile("sample/Makefile", appMakefile)
