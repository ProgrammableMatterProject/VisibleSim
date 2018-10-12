VisibleSim overview                         {#mainpage}
============
VisibleSim is a simulator of networked modular robots.
Each module is able to compute programs, communicate with its neighbors, light in full RVB colors.
It can admit sensors and actuators.

Many kinds of robots can be used in VisibleSim, each type is associated to a different geometry and different capabilities.
Current Version allows to define 7 different shapes of robots:
- SmartBlocks: box shape robot (25x25x11 mm), placed in a square 2D lattice on the floor, 4 neighbors, can slide along neighbors borders
- BlinkyBlocks: cubic robot (40 mm), placed in a square 3D lattice, 6 neighbors
- RobotBlocks: cubic robot (10 mm), placed in a square 3D lattice, 6 neighbors, can slide along neighbors faces
- Catoms2D: cylindrical robot (40 mm), placed vertically in an hexagonal lattice, 6 neighbors
- Catoms3D: quasi-spherical robot (10 mm), placed in a 3D FCC lattice, 12 neighbors
- Okten: robot with 6 retractable connectors (10mm), placed in a square 3D lattice, can turn and roll over neighbors.
- MultiRobots

Every robot can communicate with their neighbors (the number of neighbors depends on the type of robots, from 4 for Smart Block to 12 for 3D catoms).

Depending on the type of robots, they can change their color, make music or detect a tap (Blinky Blocks), slide on the floor, or convey small objects (Smart Blocks) or slide along a neighbor face (Robot Block).

All these smart robots are linked to a program (called code-block), it defines the actions the robot will execute when it receives an event from its sensors.
