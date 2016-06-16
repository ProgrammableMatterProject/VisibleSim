#include <iostream>
#include <fstream>
#include "../csgUtils.h"
#include "color.h"

const float MAX_SIZE = 20;

fstream myfile;

void writeType(CSG_T t) {
    myfile.write((char *)&t, sizeof(char));
}

void writeChar(char c) {
    myfile.write((char *)&c, sizeof(char));
}

void writeFloat(float f) {
    myfile.write((char *)&f, sizeof(float));
}

void createMugByteCode() {
    myfile.open("mug.bc", ios::binary | ios::out);

    writeType(CSG_T::Difference);
    writeType(CSG_T::Union);
    writeType(CSG_T::Cube);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/8);
    writeType(CSG_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(CSG_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/2);
    writeType(CSG_T::END);
    writeType(CSG_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(CSG_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/4);
    writeType(CSG_T::END);

    myfile.close();
}

void createMugColorByteCode() {
    myfile.open("mug-color.bc", ios::binary | ios::out);

    writeType(CSG_T::Difference);
    writeType(CSG_T::Union);
    writeType(CSG_T::Color);
    writeChar(0);
    writeChar(0);
    writeChar(255);
    writeType(CSG_T::Cube);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/8);
    writeType(CSG_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(CSG_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/2);
    writeType(CSG_T::END);
    writeType(CSG_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(CSG_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/4);
    writeType(CSG_T::END);

    myfile.close();
}

void createCubeByteCode() {
    myfile.open("cube.bc", ios::binary | ios::out);

    writeType(CSG_T::Cube);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);

    myfile.close();
}

void createSphereByteCode() {
    myfile.open("sphere.bc", ios::binary | ios::out);

    writeType(CSG_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);

    writeType(CSG_T::Sphere);
    writeFloat(MAX_SIZE/2); // radius 

    myfile.close();
}


int main() {
    createMugByteCode();
    createMugColorByteCode();
    createCubeByteCode();
    createSphereByteCode();
    return 0;
}

