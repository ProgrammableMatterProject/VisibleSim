#!/usr/bin/python

import os, sys, argparse

# Parse arguments
text = 'This programs takes a seed cell (x,y,z) that is the front-left corner of a pyramid and a length l and generates a list of all the cells that belong to the square pyramid of origin (x,y,z) and length l. This is to be pasted to the target section of a configuration file.'
parser = argparse.ArgumentParser(description = text)
parser.add_argument("-x", type=int, required=True,
                    help="the x coordinate of the seed of the pyramid")
parser.add_argument("-y", type=int, required=True,
                    help="the x coordinate of the seed of the pyramid")
parser.add_argument("-z", type=int, required=True,
                    help="the x coordinate of the seed of the pyramid")
parser.add_argument("-l", "--length", type=int, required=True,
                    help="the length of the square pyramid in number of tiles with b = 6")
args = parser.parse_args()

l = 6*(args.length - 1) + 1
for z in range(0, l):
    for x in range(0, l - z):
        for y in range(0, l - z):
            print('<cell position="%d,%d,%d" />' % (args.x + x, args.y + y, args.z + z))
