
#ifndef CONFIGUTILS_H__
#define CONFIGUTILS_H__

#include <iostream>
#include <string>
#include <map>

#include "buildingBlock.h"
#include "camera.h"
#include "vecteur.h"
#include "color.h"

#include "BuildingBlock.h"

using namespace std;
using namespace BaseSimulator;

namespace ConfigUtils {

    static int configCounter = 0;    
    static inline string generateConfigFilename() { return "config" + to_string(configCounter) + ".xml";};
    static inline string xmlVersion() { return "<?xml version=\"1.0\" standalone=\"no\" ?>"; };

    static inline string array3DToXmlString(double const array[4]) {
        return "\"" + to_string(array[0]) + ", " + to_string(array[1]) + ", " + to_string(array[2]) + "\"";
    }

    static inline string array3DToXmlString(int array[3]) {
        return "\"" + to_string(array[0]) + ", " + to_string(array[1]) + ", " + to_string(array[2]) + "\"";
    }

    static inline string array3DToXmlString(short array[3]) {
        return "\"" + to_string(array[0]) + ", " + to_string(array[1]) + ", " + to_string(array[2]) + "\"";
    }

    static inline string array3DToXmlString(float *array) {
        return "\"" + to_string(array[0]) + ", " + to_string(array[1]) + ", " + to_string(array[2]) + "\"";
    }

    static inline string vecteur3DToXmlString(const Vecteur &v) {
        return array3DToXmlString(v.pt);
    };

    static inline string colorToXmlString(Color &c) {
        return "\"" + to_string(c.rgba[0] * 255) + ", " + to_string(c.rgba[1] * 255) + ", "
            + to_string(c.rgba[2] * 255) + "\"";
    }
    
    static inline string xmlWorldOpen(int gridSize[3], int screenWidth, int screenHeight) {
        return "<world gridSize=" + array3DToXmlString(gridSize)
            + " windowSize=\"" + to_string(screenWidth)
            + "," + to_string(screenHeight) + "\" >";
    }

    static inline string xmlWorldClose() { return "</world>"; };

    static inline string xmlCamera(Camera *camera) {
        return "\t<camera target=" + vecteur3DToXmlString(camera->getTarget()) + " "
            + "directionSpherical=" + vecteur3DToXmlString(camera->getDirectionSpherical()) + " "
            + "angle=\"" + to_string(camera->getAngle()) + "\" />";
    };

    static inline string xmlSpotlight(LightSource *spotlight) {
        return "\t<spotlight target=" + array3DToXmlString(spotlight->getTarget()) + " "
            + "directionSpherical=" + vecteur3DToXmlString(spotlight->getDirectionSpherical()) + " "
            + "angle=\"" + to_string(spotlight->getAngle()) + "\" />";
    };

    string xmlBlockList(Color &color, float blockSize[3],
                        map<int, BuildingBlock*> &blocks);
}

#endif // CONFIGUTILS_H__
