#!/usr/bin/python

import os, sys, argparse

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

# Main variables
appName = None
module = None
moduleOptions = ["Catoms3D", "Catoms2D", "Datoms", "BlinkyBlocks",
                 "SmartBlocks", "SlidingCubes", "Nodes2D", "Hexanodes"]

# Parse arguments
text = 'This program creates a sample application in the applicationsSrc directory for a given module type. It takes an application name and a module type.'
parser = argparse.ArgumentParser(description = text)
parser.add_argument("-V", "--version", help="show program version", action="store_true")
parser.add_argument("-v", "--verbose", help="increase output verbosity", action="store_true")
parser.add_argument("-a", "--app", type=str, required=True,
                    help="the name of the desired application")
parser.add_argument("-m", "--module", type=str, required=True, choices=moduleOptions,
                    help="the module type")
args = parser.parse_args()

if args.verbose:
    verbose = True

appName = args.app[0].upper() + args.app[1:] # correct for uppercase
print (("Supplied AppName: (%s)") % (appName))

module = args.module
if module in moduleOptions:
    print (("Supplied ModuleName: (%s)") % (module))

print('')

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

print ("\nApplication generated successfully. Do not forget to add your application to the Makefile in the applicationsSrc/ directory before running `make`.")
