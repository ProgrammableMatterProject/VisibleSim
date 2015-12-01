#include <iostream>
#include <fstream>
#include "csg.h"

enum class Node_T : unsigned char
{
    Difference, Union, Translate, Rotate, Cube, Cylinder, Sphere, END
};

const float MAX_SIZE = 8;

fstream myfile;

void writeType(Node_T t) {
    myfile.write((char *)&t, sizeof(char));
}

void writeFloat(float f) {
    myfile.write((char *)&f, sizeof(float));
}

void createMugByteCode() {
    myfile.open("out.bc", ios::binary | ios::out);

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


int main() {
    createMugByteCode();
    return 0;
}

