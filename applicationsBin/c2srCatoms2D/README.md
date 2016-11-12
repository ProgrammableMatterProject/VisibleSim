The Cylindrical-Catoms Self-Reconfiguration (C2SR) Algorithm
==================

Implementation of the C2SR algorithm proposed in [c2sr-nca2016].

## How to run it?

```shell
./c2sr -c car.xml -B 38900 -M 1.88
```

This runs the C2SR algorithm with the `car.xml` configuration using an average communication rate of 38.9kpbs and an average motion speed of 1.88 mm/s.

Note that the communication rate and the motion speed follow a normal distribution. By default, the standard-deviation is equal to 1% of the mean. 

## Directory content

This directory contains:
- evaluation/: configurations and scripts used to evaluate C2SR
- *.sh: some shell scripts used to launch the evaluation of C2SR

[c2sr-nca2016]: André Naz, Benoît Piranda, Seth Goldstein and Julien Bourgeois "A Distributed Self-Reconfiguration Algorithm for Cylindrical Lattice-Based Modular Robots." Network Computing and Applications (NCA), 2016 15th IEEE International Symposium on. IEEE, 2016.
