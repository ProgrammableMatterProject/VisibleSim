#include "csgUtils.h"

void CsgUtils::createCSG(int MAX_SIZE) {
    CsgNode difference(node_t::bool_op , new BoolOperator(BoolOperator::bool_operator_t::bool_difference));

    CsgNode union1(node_t::bool_op, new BoolOperator(BoolOperator::bool_operator_t::bool_union));
    CsgNode cube1(node_t::shape, new Cube(MAX_SIZE, MAX_SIZE, MAX_SIZE/8));

    CsgNode translate(node_t::transformation, 
        new Transformation(Transformation::transformation_t::translate, MAX_SIZE/2, MAX_SIZE/2, MAX_SIZE/8));
    CsgNode cylinder1(node_t::shape, new Cylinder(MAX_SIZE, MAX_SIZE/2));
    translate.addChild(cylinder1);

    union1.addChild(cube1);
    union1.addChild(translate);

    CsgNode translate2(node_t::transformation, new Transformation(Transformation::transformation_t::translate, MAX_SIZE/2, MAX_SIZE/2, MAX_SIZE/8));
    CsgNode cylinder2(node_t::shape, new Cylinder(MAX_SIZE, MAX_SIZE/4));
    translate2.addChild(cylinder2);

    difference.addChild(union1);
    difference.addChild(translate2);

    csgTree.addChild(difference);
}

void CsgUtils::readFile(string path_to_file) {
    fstream csgFile;
    csgFile.open(path_to_file, ios::binary | ios::in | ios::ate);
    csgBufferSize = csgFile.tellg();

    csgFile.seekg(0, ios::beg);
    csgBuffer = new char[csgBufferSize];
    csgFile.read(csgBuffer, csgBufferSize);
    csgFile.close();

    csgBufferPos = 0;
    CsgNode node = readCSGNode();
    csgTree.addChild(node);
}

void CsgUtils::readCSGBuffer(char *_csgBuffer, int _csgBufferSize) {
    csgBuffer = _csgBuffer;
    csgBufferSize = _csgBufferSize;
    csgBufferPos = 0;

    CsgNode node = readCSGNode();
    csgTree.addChild(node);
}

CsgNode CsgUtils::readCSGNode() {
    Node_T t;
    memcpy(&t, csgBuffer + csgBufferPos, sizeof (Node_T));
    csgBufferPos += sizeof (Node_T);
    switch (t) {
        case Node_T::Difference: {
            CsgNode node_difference(node_t::bool_op , new BoolOperator(BoolOperator::bool_operator_t::bool_difference));
            CsgNode child;
            while ((child = readCSGNode()).getType() != node_t::null) {
                node_difference.addChild(child);
            }
            return node_difference;
        }
        case Node_T::Union: {
            CsgNode node_union(node_t::bool_op, new BoolOperator(BoolOperator::bool_operator_t::bool_union));
            CsgNode child;
            while ((child = readCSGNode()).getType() != node_t::null) {
                node_union.addChild(child);
            }
            return node_union;
        }
        case Node_T::Translate: {
            float f1, f2, f3;

            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f3, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);

            CsgNode node_transformation(node_t::transformation, 
                new Transformation(Transformation::transformation_t::translate, f1, f2, f3));
            CsgNode child = readCSGNode();
            node_transformation.addChild(child);
            return node_transformation;
        }
        case Node_T::Cube: {
            float f1, f2, f3;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f3, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CsgNode node_cube(node_t::shape, new Cube(f1, f2, f3));
            return node_cube;
        }
        case Node_T::Cylinder: {
            float f1, f2;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CsgNode node_cylinder(node_t::shape, new Cylinder(f1, f2));
            return node_cylinder;
        }
        case Node_T::Sphere: {
            float f1;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CsgNode node_sphere(node_t::shape, new Sphere(f1));
            return node_sphere;
        }
        case Node_T::END: {
            return CsgNode(node_t::null, NULL);
        }
        default: {
            return CsgNode(node_t::null, NULL);
        }
    }
}

bool CsgUtils::isInside(Vecteur catomPosition) {
    Vecteur v(0,0,0);
    return isInside(csgTree, v, catomPosition);
}

bool CsgUtils::isInside(CsgNode &node, Vecteur basePosition, Vecteur catomPosition) {
    switch (node.getType())
    {
        case node_t::shape: {
            Shape3D *shape = static_cast<Shape3D *>(node.getValue());
            return shape->isInside(basePosition, catomPosition);
        } break;

        case node_t::transformation: {
            Transformation* t_op = static_cast<Transformation *>(node.getValue());
            if (t_op->my_type == Transformation::transformation_t::translate) {
                for (unsigned int i = 0; i < node.vchildren.size(); i++) {
                    Vecteur transf_position(t_op->x, t_op->y, t_op->z);
                    if (isInside(node.vchildren[i], transf_position, catomPosition))
                        return true;
                }
                return false;
            }
        } break;

        case node_t::bool_op: {
            BoolOperator* b_op = static_cast<BoolOperator *>(node.getValue());
            if (b_op->my_type == BoolOperator::bool_operator_t::bool_union) {
                for (unsigned int i = 0; i < node.vchildren.size(); i++) {
                    if (isInside(node.vchildren[i], basePosition, catomPosition))
                        return true;
                }
            }
            if (b_op->my_type == BoolOperator::bool_operator_t::bool_difference) {
                if (node.vchildren.size() >= 1) {
                    if (isInside(node.vchildren[0], basePosition, catomPosition)) {
                        for (unsigned int i = 1; i < node.vchildren.size(); i++) {
                            if (isInside(node.vchildren[i], basePosition, catomPosition))
                                return false;
                        }
                        return true;
                    }
                }
            }
        } break;
    }

    return false;
}
