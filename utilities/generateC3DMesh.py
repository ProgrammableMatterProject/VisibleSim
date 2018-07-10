import sys

def isEven(n):
    return n % 2 == 0

def isInGrid(x, y, z, gridSize):
    return ((not (x < 0 or y < 0 or z < 0)) and
            (x < gridSize and y < gridSize and z < gridSize))

def isOnXBranch(x, y, z, meshScale):
    return (y % meshScale == ((z / meshScale) * (meshScale / 2) + (1 if not isEven(meshScale) and isEven(z) and z != 0 else 0)) % meshScale and
            z % meshScale == 0) 


def isOnYBranch(x, y, z, meshScale):
    return (x % meshScale == ((z / meshScale) * (meshScale / 2) + (1 if not isEven(meshScale) and isEven(z) and z != 0 else 0)) % meshScale and
            z % meshScale == 0)

def isOnZBranch(x, y, z, meshScale):
    return ( x % meshScale == y % meshScale and
             (z % meshScale == x % meshScale + y % meshScale or z % meshScale == x % meshScale + y % meshScale + 1)
             )
             # (z % meshScale == x % meshScale + y % meshScale + (1 if isEven(z) and z != 0 else 0) ) )

def isOnRevZBranch(x, y, z, meshScale):
    return (x % meshScale == y % meshScale
            and z % meshScale == (meshScale - x % meshScale))

def isOnMinus45DegZBranch(x, y, z, meshScale):
    return (x % meshScale + z % meshScale == meshScale
            and y % meshScale == 0)

def isOnPlus45DegZBranch(x, y, z, meshScale):
    return (y % meshScale + z % meshScale == meshScale
            and x % meshScale == 0)
    
def main(argv):
    # try: 
        gridSize = int(argv[1])
        assert gridSize > 0
        boxSize = int(round(gridSize * 1))
        meshScale = int(argv[2])
        assert meshScale > 0
        numBranches = int(argv[3])
        assert numBranches > 0 and numBranches < 7

        isFree = [[[True for i in xrange(gridSize)] for j in xrange(gridSize)] for k in xrange(gridSize)]
        
        filename = "config.xml"
        
        file = open(filename, "w")
        file.write('<?xml version="1.0" standalone="no" ?>\n\
<world gridSize="%d, %d, %d">\n\
    <camera target="50,50,10" directionSpherical="-20,30,100"\n\
        angle="45" near="0.1" far="2000.0" />\n\
    <blockList color="128,128,128" blocksize="10,10,10">\n' %
                   (boxSize, boxSize, boxSize))

        XMLBlock = "\t\t<block position=\"%d,%d,%d\"/>\n"
        for x in xrange(gridSize):
            for y in xrange(gridSize):
                for z in xrange(gridSize):
                    # 3 Regular Axes
                    if ( (isOnXBranch(x, y, z, meshScale) and numBranches > 0
                        or isOnYBranch(x, y, z, meshScale) and numBranches > 1
                        or isOnZBranch(x, y, z, meshScale) and numBranches > 2
                        or isOnRevZBranch(x, y, z, meshScale) and numBranches > 3
                        or isOnPlus45DegZBranch(x, y, z, meshScale) and numBranches > 4
                        or isOnMinus45DegZBranch(x, y, z, meshScale) and numBranches >  5)                         
                        and isInGrid(x, y, z, gridSize) and isFree[x][y][z]):
                            file.write(XMLBlock % (x, y, z))
                            isFree[x][y][z] = False
                    
        file.write('\t</blockList>\n</world>\n')
        file.close()

        print "VisibleSim Configuration generated in file " + filename
    # except:
    #     print "error: An error occured during initialization, please try again."
    #     print "usage: python " + __file__ + " <gridSize> <meshScale> <#branches>"
    #     print "\t\t <gridSize>  [1..n] the cubic size of the lattice"
    #     print "\t\t <meshScale> [1..n] the number of modules on a single mesh branch"
    #     print "\t\t <#branches> [1..6] the number of mesh branches"
    #     print "\t\t <coordinates> {[default], skew} "
    #     return False

if __name__ == "__main__":
    main(sys.argv)
