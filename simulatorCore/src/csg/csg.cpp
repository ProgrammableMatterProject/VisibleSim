#include "csg.h"
#include "../base/blockCode.h"
#include "../utils/color.h"
#include "math/cell3DPosition.h"
#include "../base/world.h"
#include "../grid/target.h"

#define EPS 1e-10

string numValue(double v) {
    char buffer[20];
    if (int (v)==v) snprintf(buffer, 20, "%d", int(v));
    else snprintf(buffer, 20, "%.2f", v);

    return string(buffer);
}

string vectorCode(double x,double y,double z) {
    return numValue(x)+","+numValue(y)+","+numValue(z);
}

void CSGNode::glDraw() {
    throw NotImplementedException("CSGNode::glDraw");
}

/******************************************************************/
CSGCube::CSGCube (const Vector3D &p_vec,bool p_center) : size_x(p_vec[0]), size_y(p_vec[1]), size_z(p_vec[2]), center(p_center) {}

void CSGCube::toString() const {
    OUTPUT << "cube([" << size_x << "," << size_y << "," <<  size_z << "]," << (center?"true":"false") << ")" << endl;
}

string CSGCube::toCode() const {
    string res="G"+vectorCode(size_x,size_y,size_z)+","+(center?"1":"0");
    return res;
}

bool CSGCube::isInside(const Vector3D &p, Color &color) const {
    return (center?(p[0]<=size_x/2.0 && p[0]>=-size_x/2.0 && p[1]<=size_y/2.0 && p[1]>=-size_y/2.0 && p[2]<=size_z/2.0 && p[2]>=-size_z/2.0):
                   (p[0]<=size_x && p[0]>=0 && p[1]<=size_y && p[1]>=0 && p[2] <= size_z && p[2] >= 0));
}

bool CSGCube::isInBorder(const Vector3D &p, Color &color, double border) const {
    return (center?((isInside(p, color) &&  (
            (p[0] <= size_x/2 && p[0] >= size_x/2 - border) ||
            (p[1] <= size_y/2 && p[1] >= size_y/2 - border) ||
            (p[2] <= size_z/2 && p[2] >= size_z/2 - border) ||
            (p[0] >= -size_x/2 && p[0] <= -size_x/2 + border) ||
            (p[1] >= -size_y/2 && p[1] <= -size_y/2 + border) ||
            (p[2] >= -size_z/2 && p[2] <= -size_z/2 + border)))):
            ((isInside(p, color) && (
                    (p[0] <= size_x && p[0] >= size_x - border) ||
                    (p[1] <= size_y && p[1] >= size_y - border) ||
                    (p[2] <= size_z && p[2] >= size_z - border) ||
                    (p[0] >= 0 && p[0] <= 0 + border) ||
                    (p[1] >= 0 && p[1] <= 0 + border) ||
                    (p[2] >= 0 && p[2] <= 0 + border)))));
}

void CSGCube::boundingBox(BoundingBox &bb) {
    if (center) {
        bb.P0.set(-size_x/2,-size_y/2,-size_z/2,1);
        bb.P1.set(size_x/2, size_y/2, size_z/2, 1);
    }
    else {
        bb.P0.set(0, 0, 0, 1);
        bb.P1.set(size_x, size_y, size_z, 1);
    }
}

void CSGCube::glDraw() {
    // c.set(1.0f, 0.0f, 0.0,0.5f);
    // glMaterialfv(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE,c.rgba);

    // White side - BACK
    glBegin(GL_POLYGON);
    glVertex3f(  size_x / 2.0, -size_y / 2.0, size_z / 2.0 );
    glVertex3f(  size_x / 2.0,  size_y / 2.0, size_z / 2.0 );
    glVertex3f( -size_x / 2.0,  size_y / 2.0, size_z / 2.0 );
    glVertex3f( -size_x / 2.0, -size_y / 2.0, size_z / 2.0 );
    glEnd();

    // Purple side - RIGHT
    glBegin(GL_POLYGON);
    glVertex3f( size_x / 2.0, -size_y / 2.0, -size_z / 2.0 );
    glVertex3f( size_x / 2.0,  size_y / 2.0, -size_z / 2.0 );
    glVertex3f( size_x / 2.0,  size_y / 2.0,  size_z / 2.0 );
    glVertex3f( size_x / 2.0, -size_y / 2.0,  size_z / 2.0 );
    glEnd();

    // Green side - LEFT
    glBegin(GL_POLYGON);
    glVertex3f( -size_x / 2.0, -size_y / 2.0,  size_z / 2.0 );
    glVertex3f( -size_x / 2.0,  size_y / 2.0,  size_z / 2.0 );
    glVertex3f( -size_x / 2.0,  size_y / 2.0, -size_z / 2.0 );
    glVertex3f( -size_x / 2.0, -size_y / 2.0, -size_z / 2.0 );
    glEnd();

    // Blue side - TOP
    glBegin(GL_POLYGON);
    glVertex3f(  size_x / 2.0,  size_y / 2.0,  size_z / 2.0 );
    glVertex3f(  size_x / 2.0,  size_y / 2.0, -size_z / 2.0 );
    glVertex3f( -size_x / 2.0,  size_y / 2.0, -size_z / 2.0 );
    glVertex3f( -size_x / 2.0,  size_y / 2.0,  size_z / 2.0 );
    glEnd();

    // Red side - BOTTOM
    glBegin(GL_POLYGON);
    glVertex3f(  size_x / 2.0, -size_y / 2.0, -size_z / 2.0 );
    glVertex3f(  size_x / 2.0, -size_y / 2.0,  size_z / 2.0 );
    glVertex3f( -size_x / 2.0, -size_y / 2.0,  size_z / 2.0 );
    glVertex3f( -size_x / 2.0, -size_y / 2.0, -size_z / 2.0 );
    glEnd();

    // glFlush();
    // glutSwapBuffers();
}

/******************************************************************/

void CSGSphere::toString() const {
    printf("sphere(%lf);\n", radius);
}

string CSGSphere::toCode() const {
    string res="S"+numValue(radius);
    return res;
}

bool CSGSphere::isInside(const Vector3D &p, Color &color) const {
    double dist = p[0]*p[0] + p[1]*p[1] + p[2]*p[2];
    return (dist <= radius*radius);
}

bool CSGSphere::isInBorder(const Vector3D &p, Color &color, double border) const {
    double dist = sqrt(p[0]*p[0] + p[1]*p[1] + p[2]*p[2]);
    if (dist <= radius && dist >= radius - border)
        return true;
    return false;
}

void CSGSphere::boundingBox(BoundingBox &bb) {
    bb.P0.set(-radius, -radius, -radius,1);
    bb.P1.set(radius, radius, radius, 1);
}

/******************************************************************/
CSGCylinder::CSGCylinder (double h, double r, bool p_center) : height(h), radius(r), center(p_center) {}

void CSGCylinder::toString() const {
    printf("cylinder(%lf, %lf, %lf, %s);\n", height, radius, radius,
           center ? "true" : "false");
}

string CSGCylinder::toCode() const {
    string res="Y"+numValue(height)+","+numValue(radius)+","+to_string(int(center));
    return res;
}

bool CSGCylinder::isInside(const Vector3D &p, Color &color) const {
    double dist = sqrt(p[0]*p[0] + p[1]*p[1]);
    return (center?(dist<=radius && p[2] <= height/2. && p[2] >= -height/2.):
                        (dist<=radius && p[2] <= height && p[2] >= 0));
}

bool CSGCylinder::isInBorder(const Vector3D &p, Color &color, double border) const {
    double dist = sqrt(p[0]*p[0]+p[1]*p[1]);
    if (center) {
        if (p[2] <= height/2. && p[2] >= -height/2.) {
            if (dist <= radius && dist >= radius - border) {
                return true;
            }
        }
        if ((p[2] <= height/2. && p[2] >= height/2. - border && dist <= radius) ||
            (p[2] >= -height/2. && p[2] <= -height/2. + border && dist <= radius)) {
            return true;
        }
    }
    else { // TOCHECK
        if (p[2] <= height && p[2] >= 0) {
            if (dist <= radius && dist >= radius - border) {
                return true;
            }
        }
    }
    return false;
}

void CSGCylinder::boundingBox(BoundingBox &bb) {
    if (center) {
        bb.P0.set(-radius, -radius, -height/2,1);
        bb.P1.set(radius, radius, height/2, 1);
    }
    else {
        bb.P0.set(-radius, -radius, 0, 1);
        bb.P1.set(radius, radius, height, 1);
    }
}

/******************************************************************/
CSGCone::CSGCone(double p_height, double p_bottomRadius, double p_topRadius, bool p_center) : height(p_height), bottomRadius(p_bottomRadius), topRadius(p_topRadius), center(p_center) {}

void CSGCone::toString() const {
    OUTPUT << "cone(" << height << "," << bottomRadius << "," << topRadius << "," << (center ? "true" : "false") << endl;
}

string CSGCone::toCode() const {
    string res="N"+vectorCode(height,bottomRadius,topRadius)+","+to_string(int(center));
    return res;
}

bool CSGCone::isInside(const Vector3D &p, Color &color) const {
    double dist = sqrt(p[0]*p[0] + p[1]*p[1]);
    double y = center?p[2]+height/2.0:p[2];
    return (y>=0 && y<=height)&&(dist <= (bottomRadius+y*(topRadius-bottomRadius)/height));
}

bool CSGCone::isInBorder(const Vector3D &p, Color &color, double border) const {
    double dist = sqrt(pow(p[0], 2) + pow(p[1], 2));
    double y = center?p[2]+height/2.0:p[2];
    double r = (bottomRadius+y*(topRadius-bottomRadius)/height);
    if (y>=0 && y<=height) {
        return (dist <=r  && dist>r-border);
    }
    if (y>=0 && y<=border) {
        return (dist <= r);
    }
    if (y>=height-border && y<=height) {
        return (dist <= r);
    }
    return false;
}

void CSGCone::boundingBox(BoundingBox &bb) {
    double r = max(bottomRadius,topRadius);
    if (center) {
        bb.P0.set(-r, -r, -height/2,1);
        bb.P1.set(r, r, height/2, 1);
    } else {
        bb.P0.set(-r, -r, 0, 1);
        bb.P1.set(r, r, height, 1);
    }
}

/******************************************************************/
CSGTorus::CSGTorus(double p_radius1, double p_radius2) : radius1(p_radius1), radius2(p_radius2) {}

void CSGTorus::toString() const {
    OUTPUT << "torus(" << radius1 << "," << radius2 << ")" << endl;
}

string CSGTorus::toCode() const {
    string res="O"+numValue(radius1)+","+numValue(radius2);
    return res;
}

bool CSGTorus::isInside(const Vector3D &p, Color &color) const {
    double X = sqrt(p[0]*p[0] + p[1]*p[1] + p[2]*p[2]);

    double r2 = (X-radius1)*(X-radius1) + p[2]*p[2];
    if (r2<radius2*radius2) return true;
    r2 = (X+radius1)*(X+radius1) + p[2]*p[2];
    return (r2<radius2*radius2);
}

bool CSGTorus::isInBorder(const Vector3D &p, Color &color, double border) const {
    double X = sqrt(p[0]*p[0] + p[1]*p[1] + p[2]*p[2]);

    double r = sqrt((X-radius1)*(X-radius1) + p[2]*p[2]);
    if (r<radius2 && r>(radius2-border)) return true;
    r = sqrt((X-radius1)*(X-radius1) + p[2]*p[2]);
    return (r<radius2 && r>(radius2-border));
}

void CSGTorus::boundingBox(BoundingBox &bb) {
    bb.P0.set(-radius1, -radius1, -radius2,1);
    bb.P1.set(radius1, radius1, radius2,1);
}

/******************************************************************/
void CSGTranslate::toString() const {
    printf("translate([%lf, %lf, %lf]) ", translate[0], translate[1], translate[2]);
    for (auto &c:children) {
        c->toString();
    }
}

string CSGTranslate::toCode() const {
    string res="T"+ vectorCode(translate[0], translate[1], translate[2]);
    auto it=children.begin();
    res+=(*it)->toCode();
    it++;
    while (it!=children.end()) {
        res+="|"+(*it)->toCode();
        it++;
    }
    return res;
}

bool CSGTranslate::isInside(const Vector3D &p, Color &color) const {
    Vector3D new_point(p[0]-translate[0], p[1]-translate[1], p[2]-translate[2], 1.0);
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInside(new_point, color)) return true;
    }
    return false;
}

bool CSGTranslate::isInBorder(const Vector3D &p, Color &color, double border) const {
    Vector3D new_point(p[0]-translate[0], p[1]-translate[1], p[2]-translate[2], 1.0);

    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInBorder(new_point, color, border)) return true;
    }
    return false;
}

void CSGTranslate::boundingBox(BoundingBox &bb) {
    for (unsigned int i = 0; i < children.size(); i++) {
        children[i]->boundingBox(bb);
    }
    bb.P0.set(bb.P0[0]+translate[0], bb.P0[1]+translate[1], bb.P0[2]+translate[2],1);
    bb.P1.set(bb.P1[0]+translate[0], bb.P1[1]+translate[1], bb.P1[2]+translate[2],1);
}

/******************************************************************/
CSGRotate::CSGRotate(const Vector3D &p_vec):vec(p_vec) {
    Matrix mat;
    mat.setRotationX(vec[0]);
    rotate = rotate*mat;
    mat.setRotationY(vec[1]);
    rotate = rotate*mat;
    mat.setRotationZ( vec[2]);
    rotate = rotate*mat;
    rotate.inverse(rotate_1);
}

string CSGRotate::toCode() const {
    string res="R"+ vectorCode(vec[0], vec[1], vec[2]);
    auto it=children.begin();
    res+=(*it)->toCode();
    it++;
    while (it!=children.end()) {
        res+="|"+(*it)->toCode();
        it++;
    }
    return res;
}

void CSGRotate::toString() const {
    printf("rotate([%lf, %lf, %lf]) ", vec[0], vec[1], vec[2]);
    for (auto &c:children) {
        c->toString();
    }
}

bool CSGRotate::isInside(const Vector3D &p, Color &color) const {
    Vector3D new_point = rotate_1*((Vector3D&)p);
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInside(new_point, color)) return true;
    }
    return false;
}

bool CSGRotate::isInBorder(const Vector3D &p, Color &color, double border) const {
    Vector3D new_point = rotate_1*((Vector3D&)p);
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInBorder(new_point, color, border)) return true;
    }
    return false;
}

void CSGRotate::boundingBox(BoundingBox &bb) {
    for (unsigned int i = 0; i < children.size(); i++) {
        children[i]->boundingBox(bb);
    }
    Vector3D P0 = rotate*bb.P0;
    Vector3D P1 = rotate*bb.P1;
    bb.P0.set(min(P0[0], P1[0]), min(P0[1],P1[1]), min(P0[2], P1[2]),1);
    bb.P1.set(max(P0[0], P1[0]), max(P0[1],P1[1]), max(P0[2], P1[2]),1);
}

/******************************************************************/
void CSGScale::toString() const {
    printf("scale([%lf, %lf, %lf]) ", scale[0], scale[1], scale[2]);
    for (auto &c:children) {
        c->toString();
    }
}

string CSGScale::toCode() const {
    string res="X" + vectorCode(scale[0], scale[1], scale[2]);
    auto it=children.begin();
    res+=(*it)->toCode();
    it++;
    while (it!=children.end()) {
        res+="|"+(*it)->toCode();
        it++;
    }
    return res;
}

bool CSGScale::isInside(const Vector3D &p, Color &color) const {
    Vector3D new_point(p[0]/scale[0], p[1]/scale[1], p[2]/scale[2], 1.0);
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInside(new_point, color)) return true;
    }
    return false;
}

bool CSGScale::isInBorder(const Vector3D &p, Color &color, double border) const {
    Vector3D new_point(p[0]/scale[0], p[1]/scale[1], p[2]/scale[2], 1.0);
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInBorder(new_point, color, border)) return true;
    }
    return false;
}

void CSGScale::boundingBox(BoundingBox &bb) {
    for (unsigned int i = 0; i < children.size(); i++) {
        children[i]->boundingBox(bb);
    }
    bb.P0.set(bb.P0[0]*scale[0], bb.P0[1]*scale[1], bb.P0[2]*scale[2],1);
    bb.P1.set(bb.P1[0]*scale[0], bb.P1[1]*scale[1], bb.P1[2]*scale[2],1);
}

/******************************************************************/
void CSGUnion::toString() const {
    printf("union() {\n");
    for (auto &c:children) {
        c->toString();
    }
    printf("}\n");
}

string CSGUnion::toCode() const {
    string res="+";
    auto it=children.begin();
    while (it!=children.end()) {
        res+=(*it)->toCode();
        it++;
    }
    res+="|";
    return res;
}

bool CSGUnion::isInside(const Vector3D &p, Color &color) const {
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInside(p, color)) return true;
    }
    return false;
}

bool CSGUnion::isInBorder(const Vector3D &p, Color &color, double border) const {
    bool flagInside;
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInBorder(p, color, border)) {
            flagInside = true;
            for (unsigned int j = 0; j < children.size(); j++) {
                if (i != j && children[j]->isInside(p, color)) flagInside = false;
            }
            if (flagInside)
                return true;
        }
    }
    return false;
}

void CSGUnion::boundingBox(BoundingBox &bb) {
    if (children.size() != 0) {
        children[0]->boundingBox(bb);
    }
    BoundingBox bbChild;
    for (unsigned int i = 1; i < children.size(); i++) {
        children[i]->boundingBox(bbChild);
        bb = bb | bbChild;
    }
}
/******************************************************************/
void CSGDifference::toString() const {
    printf("difference() {\n");
    for (unsigned int i = 0; i < children.size(); i++)
        children[i]->toString();
    printf("}\n");
}

string CSGDifference::toCode() const {
    string res="/";
    auto it=children.begin();
    while (it!=children.end()) {
        res+=(*it)->toCode();
        it++;
    }
    res+="|";
    return res;
}

bool CSGDifference::isInside(const Vector3D &p, Color &color) const {
    if (children.size() > 0 && children[0]->isInside(p, color)) {
        for (unsigned int i = 1; i < children.size(); i++) {
            if (children[i]->isInside(p, color)) return false;
        }
        return true;
    }
    return false;
}

bool CSGDifference::isInBorder(const Vector3D &p, Color &color, double border) const {
    if (children.size() > 0 and isInside(p, color)) {
        if (children[0]->isInBorder(p, color, border)) return true;
        else if (children[0]->isInside(p, color)) {
            const Cell3DPosition pPos = static_cast<BaseSimulator::TargetCSG*>(BaseSimulator::BlockCode::target)->
                CSGToGridPosition(p);

            // OUTPUT << "\t" << pPos << " - p: " << p << endl;
            if (border > 1.0) throw NotImplementedException("CSG difference border > 1.0");

            for (const Cell3DPosition& nPos : getWorld()->lattice->getNeighborhood(pPos)) {
                if (not BlockCode::target->isInTarget(nPos))
                    return true;
            }
        }
    }

    return false;
}

void CSGDifference::boundingBox(BoundingBox &bb) {
    if (children.size() != 0) {
        children[0]->boundingBox(bb);
    }
}

void CSGDifference::glDraw() {
    for (unsigned int i = 1; i < children.size(); i++) {
        children[i]->glDraw();
    }
}

/******************************************************************/
void CSGIntersection::toString() const {
    printf("intersection() {\n");
    for (unsigned int i = 0; i < children.size(); i++)
        children[i]->toString();
    printf("}\n");
}

string CSGIntersection::toCode() const {
    string res="^";
    auto it=children.begin();
    while (it!=children.end()) {
        res+=(*it)->toCode();
        it++;
    }
    res+="|";
    return res;
}

bool CSGIntersection::isInside(const Vector3D &p, Color &color) const {
    for (unsigned int i = 0; i < children.size(); i++) {
        if (!children[i]->isInside(p, color)) return false;
    }
    return children.size() > 0 ? true : false;
}

bool CSGIntersection::isInBorder(const Vector3D &p, Color &color, double border) const {
    bool flagInside;
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInBorder(p, color, border)) {
            flagInside = true;
            for (unsigned int j = 0; j < children.size(); j++) {
                if (i != j && !children[j]->isInside(p, color)) flagInside = false;
            }
            if (flagInside)
                return true;
        }
    }
    return false;
}

void CSGIntersection::boundingBox(BoundingBox &bb) {
    if (children.size() != 0) {
        children[0]->boundingBox(bb);
    }
}
/******************************************************************/
void CSGColor::toString() const {
    printf("color([%d, %d, %d]) ", color[0], color[1], color[2]);
    for (auto &c:children) {
        c->toString();
    }
}

string CSGColor::toCode() const {
    char colorStr[8];
    snprintf(colorStr,8,"#%02X%02X%02X",color[0],color[1],color[2]);
    string res(colorStr);
    auto it=children.begin();
    res+=(*it)->toCode();
    it++;
    while (it!=children.end()) {
        res+="|"+(*it)->toCode();
        it++;
    }
    return res;
}


bool CSGColor::isInside(const Vector3D &p, Color &color) const {
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInside(p, color)) {
            color = this->color;
            return true;
        }
    }
    return false;
}

bool CSGColor::isInBorder(const Vector3D &p, Color &color, double border) const {
    for (unsigned int i = 0; i < children.size(); i++) {
        if (children[i]->isInBorder(p, color, border)) {
            color = this->color;
            return true;
        }
    }
    return false;
}

void CSGColor::boundingBox(BoundingBox &bb) {
    for (unsigned int i = 0; i < children.size(); i++) {
        children[i]->boundingBox(bb);
    }
}
/******************************************************************/

void CSGNode::addChild(CSGNode *node) {
    children.push_back(node);
}

void CSGNode::getStats(CSGTreeStats &stats, int depth) {
    if (children.size() == 0) {
        stats.leaf++;
        stats.depth = max(stats.depth, depth);
    }
    else {
        stats.internal++;
        for (unsigned int i = 0; i < children.size(); i++) {
            children[i]->getStats(stats, depth+1);
        }
    }
}
/******************************************************************/

const BoundingBox operator |(const BoundingBox bb1,const BoundingBox bb2) {
    BoundingBox bb;
    bb.P0.set(min(bb1.P0[0],bb2.P0[0]),min(bb1.P0[1],bb2.P0[1]),min(bb1.P0[2],bb2.P0[2]), 1.0);
    bb.P1.set(max(bb1.P1[0],bb2.P1[0]),max(bb1.P1[1],bb2.P1[1]),max(bb1.P1[2],bb2.P1[2]), 1.0);
    return bb;
}
