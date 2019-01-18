import sys

def main(argv):
    try:
        x = int(argv[1])

        print(x)
        file = open("configout.xml", "w")
        file.write('<?xml version="1.0" standalone="no" ?>\n\
<world gridSize="210, 210, 210">\n\
    <camera target="200,200,120" directionSpherical="125,30,250" angle="45" near="0.01" far="2500.0" />\n\
    <spotlight target="200,200,120" directionSpherical="125,40,1500" angle="30" near="80.0" far="2500.0"/>\n\
    <blockList color="128,128,128" blocksize="10,10,10">\n\n')

        for i in range(x):
            for j in range(x):
                for k in range(int(x*1.41)):
                    file.write("\t\t<block position=\"%d,%d,%d\" orientation=\"0\"/>\n" % (i, j, k))

        file.write('\t</blockList>\n</world>\n')
        file.close()
    except:
        return False

if __name__ == "__main__":
    main(sys.argv)
