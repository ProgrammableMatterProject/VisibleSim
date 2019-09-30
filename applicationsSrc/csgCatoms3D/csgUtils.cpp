#include "csgUtils.h"

/*
void CsgUtils::createCSG(int MAX_SIZE) {
    CSGNode difference(node_t::bool_op , new BoolOperator(BoolOperator::bool_operator_t::bool_difference));

    CSGNode union1(node_t::bool_op, new BoolOperator(BoolOperator::bool_operator_t::bool_union));
    CSGNode cube1(node_t::shape, new Cube(MAX_SIZE, MAX_SIZE, MAX_SIZE/8));

    CSGNode translate(node_t::transformation, 
        new Transformation(Transformation::transformation_t::translate, MAX_SIZE/2, MAX_SIZE/2, MAX_SIZE/8));
    CSGNode cylinder1(node_t::shape, new Cylinder(MAX_SIZE, MAX_SIZE/2));
    translate.addChild(cylinder1);

    union1.addChild(cube1);
    union1.addChild(translate);

    CSGNode translate2(node_t::transformation, new Transformation(Transformation::transformation_t::translate, MAX_SIZE/2, MAX_SIZE/2, MAX_SIZE/8));
    CSGNode cylinder2(node_t::shape, new Cylinder(MAX_SIZE, MAX_SIZE/4));
    translate2.addChild(cylinder2);

    difference.addChild(union1);
    difference.addChild(translate2);

    return difference;
}
*/

CSGNode* CsgUtils::readFile(string path_to_file) {
    fstream csgFile;
    csgFile.open(path_to_file, ios::binary | ios::in | ios::ate);
    csgBufferSize = csgFile.tellg();

    csgFile.seekg(0, ios::beg);
    csgBuffer = new char[csgBufferSize];
    csgFile.read(csgBuffer, csgBufferSize);
    csgFile.close();

    csgBufferPos = 0;
    return readCSGNode();
}

CSGNode* CsgUtils::readCSGBuffer(char *_csgBuffer, int _csgBufferSize) {
    csgBuffer = _csgBuffer;
    csgBufferSize = _csgBufferSize;
    csgBufferPos = 0;

    return readCSGNode();
}

CSGNode* CsgUtils::readCSGNode() {
    CSG_T t;
    memcpy(&t, csgBuffer + csgBufferPos, sizeof (CSG_T));
    csgBufferPos += sizeof (CSG_T);
    switch (t) {
        case CSG_T::Difference: {
            CSGDifference *csgDifference = new CSGDifference();
            CSGNode *child;
            while ((child = readCSGNode()) != NULL) {
                csgDifference->addChild(child);
            }
            return csgDifference;
        }
        case CSG_T::Union: {
            CSGUnion *csgUnion = new CSGUnion();
            CSGNode *child;
            while ((child = readCSGNode()) != NULL) {
                csgUnion->addChild(child);
            }
            return csgUnion;
        }
        case CSG_T::Intersection: {
            CSGIntersection *csgIntersection = new CSGIntersection();
            CSGNode *child;
            while ((child = readCSGNode()) != NULL) {
                csgIntersection->addChild(child);
            }
            return csgIntersection;
        }
        case CSG_T::Translate: {
            float f1, f2, f3;

            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f3, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);

            CSGTranslate *csgTranslate = new CSGTranslate(f1,f2,f3);
            csgTranslate->addChild(readCSGNode());
            return csgTranslate;
        }
        case CSG_T::Rotate: {
            float f1, f2, f3;

            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f3, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);

            CSGRotate *csgRotate = new CSGRotate(f1,f2,f3);
            csgRotate->addChild(readCSGNode());
            return csgRotate;
        }
        case CSG_T::Scale: {
            float f1, f2, f3;

            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f3, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);

            CSGScale *csgScale = new CSGScale(f1,f2,f3);
            csgScale->addChild(readCSGNode());
            return csgScale;
        }
        case CSG_T::Color: {
            unsigned char c1, c2, c3;

            memcpy(&c1, csgBuffer + csgBufferPos, sizeof(char));
            csgBufferPos += sizeof(char);
            memcpy(&c2, csgBuffer + csgBufferPos, sizeof(char));
            csgBufferPos += sizeof(char);
            memcpy(&c3, csgBuffer + csgBufferPos, sizeof(char));
            csgBufferPos += sizeof(char);

            CSGColor *csgColor = new CSGColor(c1,c2,c3);
            csgColor->addChild(readCSGNode());
            return csgColor;
        }
        case CSG_T::Cube: {
            float f1, f2, f3;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f3, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CSGCube *csgCube = new CSGCube(f1, f2, f3);
            return csgCube;
        }
        case CSG_T::Cylinder: {
            float f1, f2;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            memcpy(&f2, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CSGCylinder *csgCylinder =  new CSGCylinder(f1, f2);
            return csgCylinder;
        }
        case CSG_T::Sphere: {
            float f1;
            memcpy(&f1, csgBuffer + csgBufferPos, sizeof(float));
            csgBufferPos += sizeof(float);
            CSGSphere *csgSphere = new CSGSphere(f1);
            return csgSphere;
        }
        case CSG_T::END: {
            return NULL;
        }
        default: {
            cout << "Unrecognized code" << endl;
            return NULL;
        }
    }
}

