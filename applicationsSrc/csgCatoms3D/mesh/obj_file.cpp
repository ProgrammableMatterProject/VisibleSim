#include <iostream>
#include <fstream>
#include <algorithm>
#include "geometrics.h"
#include "obj_file.h"
#include "world.h"

void Obj_File::read_obj()
{
    ifstream my_obj(filename.c_str());
    string line;
    string line_desc;

    while (getline(my_obj, line)) {
        stringstream line_stream(line);
        if (line_stream >> line_desc) {
            if (line_desc == "v") {
                read_v_line(line);
            }
            if (line_desc == "f") {
                read_f_line(line);
            }
        }
    }
}

void Obj_File::read_v_line(string line) {
    stringstream line_stream(line);
    char first;
    double x, y, z;
    line_stream >> first >> x >> y >> z;

    MeshWorld::points.push_back(new Point(x, y, z));
}

void Obj_File::read_f_line(string line) {
    int v0a, v1a, v2a;
    char first;
    if (line.find("/") == string::npos) {
        sscanf(line.c_str(), "%c %d %d %d", &first, &v0a, &v1a, &v2a);
        Triangle *t = new Triangle(v0a-1, v1a-1, v2a-1);
        MeshWorld::polygons.push_back(t);
    }
    else {
        printf("Unsupported obj file\n");
    }

}
