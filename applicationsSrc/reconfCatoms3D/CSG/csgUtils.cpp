#include "csgUtils.h"
#include "utils.h"
#include "csgRead.h"

CSGNode* CsgUtils::csgRoot = NULL;
BoundingBox CsgUtils::boundingBox;

Vector3D gridToWorldPosition(const Cell3DPosition &pos) {
    Vector3D res;

    res.pt[3] = 1.0;
    res.pt[2] = BaseSimulator::utils::M_SQRT2_2 * (pos[2] + 0.5);
    if (IS_EVEN(pos[2])) {
        res.pt[1] = (pos[1] + 0.5);
        res.pt[0] = (pos[0] + 0.5);
    } else {
        res.pt[1] = (pos[1] + 1.0);
        res.pt[0] = (pos[0] + 1.0);
    }
    return res;
}

Vector3D CsgUtils::getWorldPosition(Cell3DPosition gridPosition) {
    Vector3D worldPosition = gridToWorldPosition(gridPosition);
    worldPosition.pt[0] += boundingBox.P0[0]; 
    worldPosition.pt[1] += boundingBox.P0[1]; 
    worldPosition.pt[2] += boundingBox.P0[2]; 
    return worldPosition;
}

void CsgUtils::fixBoundingBox() {
    CsgUtils::boundingBox.P0.pt[0] -= 1;
    CsgUtils::boundingBox.P0.pt[1] -= 1;
    CsgUtils::boundingBox.P0.pt[2] -= 1;
    CsgUtils::boundingBox.P1.pt[0] -= 1;
    CsgUtils::boundingBox.P1.pt[1] -= 1;
    CsgUtils::boundingBox.P1.pt[2] -= 1;
}

void CsgUtils::init(const string &file) {
    CsgRead csgRead;
    CsgUtils::csgRoot = csgRead.readFile(file);
    CsgUtils::csgRoot->toString();
    CsgUtils::csgRoot->boundingBox(CsgUtils::boundingBox);
    cout << "Bounding box: " << CsgUtils::boundingBox.P0 << ' ' << CsgUtils::boundingBox.P1 << endl;
    CsgUtils::fixBoundingBox();
}

bool CsgUtils::isInside(const Vector3D &position) {
    Color c;
    return CsgUtils::csgRoot->isInside(position, c);
}

bool CsgUtils::isInside(const Cell3DPosition &position) {
    Vector3D pos = CsgUtils::getWorldPosition(position);
    return CsgUtils::isInside(pos);
}


