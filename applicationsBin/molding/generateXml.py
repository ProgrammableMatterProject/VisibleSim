import sys

def main(argv):
    try:
        x = int(argv[1])

        print(x)
        file = open("config.xml", "w")
        file.write('<?xml version="1.0" standalone="no" ?>\n\
<world gridSize="%d,%d,%d">\n\
    <camera target="25,20,10" directionSpherical="0,90,30" angle="45" near="0.01" far="1000.0" />\n\
    <spotlight target="25,25,0" directionSpherical="-35,40,150" angle="30"/>\n\
    <blockList color="128,128,128" blocksize="10,10,10">\n\n'%(x+1,x+1,x+1))
	
        for i in range(x):
            for j in range(x):
                for k in range(x):
                    file.write("\t\t<block position=\"%d,%d,%d\" orientation=\"0\"/>\n" % (k-(i/2), j-(i/2), i))

        file.write('\t</blockList>\n</world>\n')
        file.close()
    except:
        return False

if __name__ == "__main__":
    main(sys.argv)
