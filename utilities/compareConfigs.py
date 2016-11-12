import sys
import xml.etree.ElementTree as ET
from bisect import bisect_left

if (len(sys.argv) < 3):
    print ('Usage: ./'+sys.argv[0] ,'xmlfile.xml xmlfile2.xml')
    sys.exit(0)

xml1Element = ET.parse(sys.argv[1]).getroot()
xml2Element = ET.parse(sys.argv[2]).getroot()
countDifferents1 = 0
countDifferents2 = 0
countEquals = 0

def binary_search(a, x, lo=0, hi=None):
    hi = hi if hi is not None else len(a)
    pos = bisect_left(a, x, lo, hi)
    if pos != hi and a[pos] == x:
        return pos
    return -1

xml1Blocks = []
xml2Blocks = []

for block in xml1Element.findall('*block'):
    position = block.get('position')
    m = tuple(map(lambda x: x, map(int, position.split(','))))
    xml1Blocks.append(m)

for block in xml2Element.findall('*block'):
    position = block.get('position')
    m = tuple(map(int, position.split(',')))
    xml2Blocks.append(m)

xml2Blocks.sort()
xml1Blocks.sort()

for xml2Pos in xml2Blocks:
    if (binary_search(xml1Blocks, xml2Pos) == -1):
        countDifferents2 = countDifferents2+1
for xml1Pos in xml1Blocks:
    if (binary_search(xml2Blocks, xml1Pos) == -1):
        countDifferents1 = countDifferents1+1

for xml1Pos in xml1Blocks:
    if (binary_search(xml2Blocks, xml1Pos) != -1):
        countEquals = countEquals+1

print ("Number of equal catoms:", countEquals)
print ("Number of catoms in XML 1 and not XML 2:", countDifferents1)
print ("Number of catoms in XML 2 and not XML 1:", countDifferents2)
print ("Total catoms in XML 1:", len(xml1Blocks))
print ("Total catoms in XML 2:", len(xml2Blocks))


def printDifferenceColored():

    print('<?xml version="1.0" standalone="no" ?>\n\
    <world gridSize="200,200,200">\n\
    <camera target="480,760,444" directionSpherical="0,38,1700" angle="50" far="3544.0" />\n\
    <spotlight target="480,760,444" directionSpherical="-30,50,2112" angle="45"/>\n\
    <blockList color="128,128,128" blocksize="10,10,10">\n\n')

    for xml2Pos in xml2Blocks:
        posStr = str(xml2Pos).replace('(','').replace(')','')
        if (binary_search(xml1Blocks, xml2Pos) == -1):
            print("<block position=\""+posStr+"\" color=\"0,255,0\" />")
        else:
            print("<block position=\""+posStr+"\" color=\"0,0,255\" />")
    for xml1Pos in xml1Blocks:
        posStr = str(xml1Pos).replace('(','').replace(')','')
        if (binary_search(xml2Blocks, xml1Pos) == -1):
            print("<block position=\""+posStr+"\" color=\"255,255,0\" />")
    print("</blockList>")
    print("</world>")

printDifferenceColored();



