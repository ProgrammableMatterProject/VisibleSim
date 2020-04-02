#include "meshUtils.h"
#include "mesh/obj_file.h"

void MeshUtils::readFile(string path_to_file) {
    Obj_File obj(path_to_file);
    w.create_world();
}

bool MeshUtils::isInside(Vector3D catomPosition) {
    int ret;
    Point p(catomPosition.pt[0], catomPosition.pt[1], catomPosition.pt[2]);
    while ((ret = w.point_in_polygon(p, w.get_random_point())) == 1);
    return ret;
}
