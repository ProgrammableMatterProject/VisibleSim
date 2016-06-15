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
    CSG_T t;
    memcpy(&t, csgBuffer + csgBufferPos, sizeof (CSG_T));
    csgBufferPos += sizeof (CSG_T);
    switch (t) {
        case CSG_T::Difference: {
            CsgNode node_difference(node_t::bool_op , new BoolOperator(BoolOperator::bool_operator_t::bool_difference));
            CsgNode child;
            while ((child = readCSGNode()).getType() != node_t::end) {
                node_difference.addChild(child);
            }
            return node_difference;
        }
        case CSG_T::Union: {
            CsgNode node_union(node_t::bool_op, new BoolOperator(BoolOperator::bool_operator_t::bool_union));
            CsgNode child;
            while ((child = readCSGNode()).getType() != node_t::end) {
                node_union.addChild(child);
            }
            return node_union;
        }
        case CSG_T::Translate: {
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
        case CSG_T::Color: {
            unsigned char c1, c2, c3;

            memcpy(&c1, csgBuffer + csgBufferPos, sizeof(char));
            csgBufferPos += sizeof(char);
            memcpy(&c2, csgBuffer + csgBufferPos, sizeof(char));
            csgBufferPos += sizeof(char);
            memcpy(&c3, csgBuffer + csgBufferPos, sizeof(char));
            csgBufferPos += sizeof(char);

            CsgNode csg_color(node_t::color,  new NodeColor((int)c1, (int)c2, (int)c3, "thadeu"));
            CsgNode child = readCSGNode();
            csg_color.addChild(child);
            return csg_color;
        }
        case CSG_T::Cube: {
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
        case CSG_T::Cylinder: {
            float f1, f2;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CsgNode node_cylinder(node_t::shape, new Cylinder(f1, f2));
            return node_cylinder;
        }
        case CSG_T::Sphere: {
            float f1;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CsgNode node_sphere(node_t::shape, new Sphere(f1));
            return node_sphere;
        }
        case CSG_T::END: {
            return CsgNode(node_t::end, NULL);
        }
        default: {
            return CsgNode(node_t::end, NULL);
        }
    }
}

PositionInfo CsgUtils::isInside(Vector3D catomPosition) {
    Vector3D v(0,0,0);
    Color c(0,0,0);
    return isInside(csgTree, v, c, catomPosition);
}

PositionInfo CsgUtils::isInside(CsgNode &node, Vector3D basePosition, Color color, Vector3D catomPosition) {
    switch (node.getType())
    {
        case node_t::shape: {
            Shape3D *shape = static_cast<Shape3D *>(node.getValue());
            bool inside = shape->isInside(basePosition, catomPosition);
            return PositionInfo(inside, color);
        } break;

        case node_t::color: {
            NodeColor* color_op = static_cast<NodeColor *>(node.getValue());
            PositionInfo pi;
            for (unsigned int i = 0; i < node.vchildren.size(); i++) {
                Color new_color(color_op->r/255., color_op->g/255., color_op->b/255.);
                pi = isInside(node.vchildren[i], basePosition, new_color, catomPosition);
                if (pi.isInside())
                    return pi;
            }
            return PositionInfo(false);
        } break;

        case node_t::transformation: {
            Transformation* t_op = static_cast<Transformation *>(node.getValue());
            PositionInfo pi;
            if (t_op->my_type == Transformation::transformation_t::translate) {
                for (unsigned int i = 0; i < node.vchildren.size(); i++) {
                    Vector3D transf_position(t_op->x, t_op->y, t_op->z);
                    pi = isInside(node.vchildren[i], transf_position, color, catomPosition);
                    if (pi.isInside())
                        return pi;
                }
                return PositionInfo(false);
            }
        } break;

        case node_t::bool_op: {
            BoolOperator* b_op = static_cast<BoolOperator *>(node.getValue());
            PositionInfo pi;
            if (b_op->my_type == BoolOperator::bool_operator_t::bool_union) {
                for (unsigned int i = 0; i < node.vchildren.size(); i++) {
                    pi = isInside(node.vchildren[i], basePosition, color, catomPosition);
                    if (pi.isInside())
                        return pi;
                }
                return PositionInfo(false);
            }
            if (b_op->my_type == BoolOperator::bool_operator_t::bool_difference) {
                if (node.vchildren.size() >= 1) {
                    pi = isInside(node.vchildren[0], basePosition, color, catomPosition);
                    if (pi.isInside()) {
                        for (unsigned int i = 1; i < node.vchildren.size(); i++) {
                            if (isInside(node.vchildren[i], basePosition, color, catomPosition).isInside())
                                return PositionInfo(false);
                        }
                        return pi;
                    }
                }
            }
        } break;
    }

    return false;
}
