import sys

def main(argv):
    try:
        x = int(argv[1])

        print(x)
        file = open("config.xml", "w")
        file.write('<?xml version="1.0" standalone="no" ?>\n\
<world gridSize="%d, %d, %d">\n\
    <camera target="50,50,0" directionSpherical="-20,30,%d" angle="45" near="0.01" far="2500.0" />\n\
    <spotlight target="50,50,0" directionSpherical="-35,40,1500" angle="30" near="80.0" far="2500.0"/>\n\
    <blockList color="128,128,128" blocksize="10,10,10">\n\n' % (x+5, x+5, x+5, 100+x*10))
	
        for i in range(x):
            for j in range(x):
                for k in range(x):
                    file.write("\t\t<block position=\"%d,%d,%d\" orientation=\"0\"/>\n" % (i+1, j+1, k))

        file.write('\t</blockList>\n</world>\n')
        file.close()
    except:
        return False

if __name__ == "__main__":
    main(sys.argv)
