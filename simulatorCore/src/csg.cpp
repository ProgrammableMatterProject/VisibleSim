#include "csg.h"
#define EPS 1e-10

void Cube::print() {
    cout << "CUBE " << size_x << ' ' << size_y << ' ' << size_z << endl;
}

bool Cube::isInside(Vecteur basePos, Vecteur p) {
    //center
    if (false) {
        if (fabs(p.pt[0] - basePos.pt[0]) < size_x/2 && fabs(p.pt[1] - basePos.pt[1]) < size_y/2 && fabs(p.pt[2] - basePos.pt[2]) < size_z/2)
            return true;
    }
    else {
        if ((p.pt[0] - basePos.pt[0]) < size_x && (p.pt[1] - basePos.pt[1]) < size_y && (p.pt[2] - basePos.pt[2]) < size_z)
            return true;
    }
    return false;
}

void Sphere::print() {
    cout << "SPHERE " << radius << endl;
}

bool Sphere::isInside(Vecteur basePos, Vecteur p) {
    double dist = sqrt(pow(p.pt[0]-basePos.pt[0], 2) + pow(p.pt[1]-basePos.pt[1], 2) + pow(p.pt[2]-basePos.pt[2], 2));
    if (dist < radius)
        return true;
    return false;
}

void Cylinder::print() {
    cout << "CYLINDER " << h << " " << r << endl;
}

bool Cylinder::isInside(Vecteur basePos, Vecteur p) {
    if (p.pt[2] - basePos.pt[2] < h && p.pt[2] - basePos.pt[2] > -EPS)
        if (sqrt(pow(p.pt[0]-basePos.pt[0], 2) + pow(p.pt[1]-basePos.pt[1],2)) < r)
            return true;
    return false;
}

void BoolOperator::print() {
    switch(my_type) {
        case bool_operator_t::bool_union:
            cout << "UNION" << endl;
            break;
        case bool_operator_t::bool_difference:
            cout << "DIFFERENCE" << endl;
            break;
        case bool_operator_t::bool_intersection:
            cout << "INTERSECTION" << endl;
    }
}

CsgNode::CsgNode() {
    type = node_t::bool_op;
    value = new BoolOperator(BoolOperator::bool_operator_t::bool_union);
}

CsgNode::CsgNode(const CsgNode &other) {
    type = other.type;
    switch (type) {
        case node_t::bool_op: {
            BoolOperator* boolOp = new BoolOperator(*(BoolOperator *)other.value);
            value = (void *) boolOp;
        }
        break;
        case node_t::shape: {
            Shape3D* shape = ((Shape3D*)other.value)->clone();
            value = (void *) shape;
        }
        break;
        case node_t::transformation: {
            Transformation* transformation = new Transformation(*(Transformation *)other.value);
            value = (void *) transformation;
        }
        break;
    }
    vchildren = other.vchildren;
} 

CsgNode& CsgNode::operator=(const CsgNode &other) {
    if (this != &other) {
        switch (type) {
            case node_t::bool_op:
                delete static_cast<BoolOperator*>(value);
                break;
            case node_t::shape:
                delete static_cast<Shape3D*>(value);
                break;
            case node_t::transformation:
                delete static_cast<Transformation*>(value);
                break;
        }
        type = other.type;
        if (type == node_t::bool_op) {
            BoolOperator* boolOp = new BoolOperator(*(BoolOperator *)other.value);
            value = (void *) boolOp;
        }
        else if (type == node_t::shape) {
            Shape3D* shape = ((Shape3D*)other.value)->clone();
            value = (void *) shape;
        }
        else if (type == node_t::transformation) {
            Transformation* transformation = new Transformation(*(Transformation *)other.value);
            value = (void *) transformation;
        }
        vchildren = other.vchildren; 
    }
    return *this;
}

CsgNode::~CsgNode() {
    switch (type) {
        case node_t::bool_op:
            delete static_cast<BoolOperator*>(value);
            break;
        case node_t::shape:
            delete static_cast<Shape3D*>(value);
            break;
        case node_t::transformation:
            delete static_cast<Transformation*>(value);
            break;
    }
};

void CsgNode::addChild(CsgNode &n) {
    vchildren.push_back(n);
}

void CsgNode::printTree() {
    switch(type) {
        case node_t::bool_op:
            cout << "bool operator!" << endl;
            break;
        case node_t::shape:
            cout << "shape!" << endl;
            break;
        case node_t::transformation:
            cout << "transformation!" << endl;
            break;
    }

    cout << "children = " << vchildren.size() << endl;
    for (unsigned int i = 0; i < vchildren.size(); i++) {
        vchildren[i].printTree();
    }
}
