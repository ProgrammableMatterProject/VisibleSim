#include "stoyUtils.h"

vector<Brick> StoyUtils::bricks;

void StoyUtils::readFile(string path_to_file) {
    fstream stoyFile;
    stoyFile.open(path_to_file);

    Brick b;
    while (stoyFile >> b.p1.pt[0] >> b.p1.pt[1] >> b.p1.pt[2]) {
        stoyFile >> b.p2.pt[0] >> b.p2.pt[1] >> b.p2.pt[2];
        bricks.push_back(b);
    }

    stoyFile.close();
}

bool StoyUtils::isInside(Vector3D catomPosition) {
    for (unsigned int i = 0; i < bricks.size(); i++) {
        if (bricks[i].p1.pt[0] <= catomPosition.pt[0] &&
            bricks[i].p1.pt[1] <= catomPosition.pt[1] &&
            bricks[i].p1.pt[2] <= catomPosition.pt[2] &&
            bricks[i].p2.pt[0] >= catomPosition.pt[0] &&
            bricks[i].p2.pt[1] >= catomPosition.pt[1] &&
            bricks[i].p2.pt[2] >= catomPosition.pt[2])
            return true;
    }
    return false;
}
