import sys
import pprint

def isInRange(v, lb, ub):
    return (lb <= v and v <= ub)

def isInGrid(x, y, z, gridSize):
    # return ((not (x - abs(z / 2) < 0 or y - abs(z / 2) < 0 or z < 0)) and
    #         (x - abs(z / 2) < gridSize and y - abs(z / 2) < gridSize and z < gridSize))
    if z % 2 == 0:
        return (isInRange(x, 0 - z/ 2, gridSize - z / 2 - 1) 
                and isInRange(y, 0 - z / 2, gridSize - z / 2 - 1)
                and isInRange(z, 0, gridSize - 1))
    else:
        return (isInRange(x, 0 - z/ 2, gridSize - z / 2 - 1)
                and isInRange(y, 0 - z / 2, gridSize - z / 2 - 1)
                and isInRange(z, 0, gridSize - 1))
    
def isOnXBranch(x, y, z, meshScale):
    return x % meshScale == 0 and z % meshScale == 0

def isOnYBranch(x, y, z, meshScale):
    return y % meshScale == 0 and z % meshScale == 0

def isOnZBranch(x, y, z, meshScale):
    return x % meshScale == 0 and y % meshScale == 0

def isOnRevZBranch(x, y, z, meshScale):
    return (x % meshScale == y % meshScale
            and z % meshScale == (meshScale - x % meshScale))

def isOnMinus45DegZBranch(x, y, z, meshScale):
    return (x % meshScale + z % meshScale == meshScale
            and y % meshScale == 0)

def isOnPlus45DegZBranch(x, y, z, meshScale):
    return (y % meshScale + z % meshScale == meshScale
            and x % meshScale == 0)

XMLBlock = "\t\t<block position=\"%d,%d,%d\" color=\"%d,%d,%d\"/>\n"
def addMeshModule(fd, x, y, z, meshScale):
    r = 255 / (abs(x + z) / meshScale) if abs(x + z) / meshScale != 0 else 1
    g = 255 / (abs(y + z) / meshScale) if abs(y + z) / meshScale != 0 else 1
    b = 255 / (abs(z) / meshScale) if abs(z) / meshScale != 0 else 1
    
    fd.write(XMLBlock % (x, y, z, r, g, b))
    
def main(argv):
    try: 
        gridSize = int(argv[1])
        assert gridSize > 0
        boxSize = int(gridSize)
        meshScale = int(argv[2])
        assert meshScale > 0
        numBranches = int(argv[3])
        assert numBranches > 0 and numBranches < 7

        isFree = [[[True for i in xrange(gridSize)] for j in xrange(gridSize)] for k in xrange(gridSize)]
        
        XMLBlock = "\t\t<block position=\"%d,%d,%d\" orientation=\"0\"/>\n"
        filename = "config.xml"
        
        file = open(filename, "w")
        file.write('<?xml version="1.0" standalone="no" ?>\n\
<world gridSize="%d, %d, %d">\n\
    <camera target="50,50,10" directionSpherical="-20,30,100"\n\
        angle="45" near="0.1" far="2000.0" />\n\n\
    <blockList color="128,128,128" blocksize="10,10,10">\n' %
                   (boxSize, boxSize, boxSize))
	
        # for x in xrange(0 - gridSize / 2 + 1, gridSize / 2):
        #     for y in xrange(0 - gridSize / 2 + 1, gridSize / 2):
        for x in xrange(0, gridSize):
            for y in xrange(0, gridSize):
                for z in xrange(gridSize):
                    xp = x - abs(z / 2)
                    yp = y - abs(z / 2)
                    # 3 Regular Axes
                    if ( ( isOnXBranch(xp, yp, z, meshScale) and numBranches > 0
                           or isOnYBranch(xp, yp, z, meshScale) and numBranches > 1
                           or isOnZBranch(xp, yp, z, meshScale) and numBranches > 2
                           or isOnRevZBranch(xp, yp, z, meshScale) and numBranches > 3
                           or isOnPlus45DegZBranch(xp, yp, z, meshScale) and numBranches > 4
                           or isOnMinus45DegZBranch(xp, yp, z, meshScale) and numBranches >  5)
                         \
                         and isInGrid(xp, yp, z, gridSize)
                         and isFree[x][y][z]):
                        addMeshModule(file, xp, yp, z, meshScale)
                        isFree[x][y][z] = False;
                        
        file.write('\t</blockList>\n\n</world>\n')
        file.close()

        print "VisibleSim Configuration generated in file " + filename
        
    except:
        print "error: An error occured during initialization, please try again."
        print "usage: python " + __file__ + " <gridSize> <meshScale> <#branches>"
        print "\t\t <gridSize>  [1..n] the cubic size of the lattice"
        print "\t\t <meshScale> [1..n] the number of modules on a single mesh branch"
        print "\t\t <#branches> [1..6] the number of mesh branches"
        print "\t\t <coordinates> {[default], skew} indicates whether or not to use skew coordinate system"
        return False

if __name__ == "__main__":
    main(sys.argv)
