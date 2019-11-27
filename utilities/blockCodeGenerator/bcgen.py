import os
import sys

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

appName = "TestC3D"
appNameLc = appName[0].lower() + appName[1:]
module = "Catoms3D"
moduleNameLc = module[0].lower() + module[1:]
# modulesDict = {
#     "Catoms3D": "catoms3D",
#     "Catoms2D": "catoms2D",
#     "CatomsR": "catomsR",
#     "Datoms": "datoms",
#     "BlinkyBlocks": "blinkyBlocks",
#     "SmartBlocks": "smartBlocks",
#     "RobotBlocks": "robotBlocks",
#     "Okteens": "okteens",
#     "MultiRobots": "multiRobots",
# }

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
