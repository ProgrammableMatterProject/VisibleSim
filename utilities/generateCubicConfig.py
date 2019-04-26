import sys

def main(argv):
    try:
        side_length = int(argv[1])

        file = open("config.xml", "w")
        file.write('<?xml version="1.0" standalone="no" ?>\n\
<world gridSize="%d, %d, %d">\n\
    <camera target="50,50,10" directionSpherical="-20,30,100" angle="45" near="0.01" far="2500.0" />\n\
    <spotlight target="600,600,420" directionSpherical="-35,30,2400" angle="30" near="80.0" far="2500.0"/>\n\
    <blockList color="128,128,128" blocksize="10,10,10">\n\n' % (x+1, x+1, x+1))
	
        for i in range(side_length):
            for j in range(side_length):
                for k in range(side_length):
                    file.write("\t\t<block position=\"%d,%d,%d\" orientation=\"0\"/>\n" % (i+1, j+1, k+1))

        file.write('\t</blockList>\n</world>\n')
        file.close()
    except:
        return False

if __name__ == "__main__":
    main(sys.argv)
