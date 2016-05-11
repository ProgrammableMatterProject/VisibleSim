#include <iostream>
#include <fstream>
#include "csg.h"

enum class Node_T : unsigned char
{
    Difference, Union, Translate, Rotate, Color, Cube, Cylinder, Sphere, END
};

const float MAX_SIZE = 4;

fstream myfile;

void writeType(Node_T t) {
    myfile.write((char *)&t, sizeof(char));
}

void writeFloat(float f) {
    myfile.write((char *)&f, sizeof(float));
}

void createMugByteCode() {
    myfile.open("mug.bc", ios::binary | ios::out);

    writeType(Node_T::Difference);
    writeType(Node_T::Union);
    writeType(Node_T::Cube);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/8);
    writeType(Node_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(Node_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/2);
    writeType(Node_T::END);
    writeType(Node_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(Node_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/4);
    writeType(Node_T::END);

    myfile.close();
}

void createMugColorByteCode() {
    myfile.open("mug-color.bc", ios::binary | ios::out);

    writeType(Node_T::Difference);
    writeType(Node_T::Union);
    writeType(Node_T::Color);
    writeFloat(1);
    writeType(Node_T::Cube);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/8);
    writeType(Node_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(Node_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/2);
    writeType(Node_T::END);
    writeType(Node_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/8);
    writeType(Node_T::Cylinder);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE/4);
    writeType(Node_T::END);

    myfile.close();
}

void createCubeByteCode() {
    myfile.open("cube.bc", ios::binary | ios::out);

    writeType(Node_T::Cube);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);
    writeFloat(MAX_SIZE);

    myfile.close();
}

void createSphereByteCode() {
    myfile.open("sphere.bc", ios::binary | ios::out);

    writeType(Node_T::Translate);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);
    writeFloat(MAX_SIZE/2);

    writeType(Node_T::Sphere);
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

