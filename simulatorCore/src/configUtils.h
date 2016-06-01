
#ifndef CONFIGUTILS_H__
#define CONFIGUTILS_H__

#include <iostream>
#include <string>
#include <map>

#include "buildingBlock.h"
#include "blinkyBlocksBlock.h"
#include "camera.h"
#include "vecteur.h"
#include "color.h"

using namespace std;
using namespace BaseSimulator;

namespace ConfigUtils {

    static int configCounter = 0;    
    static inline string generateConfigFilename() { return "config" + to_string(configCounter) + ".xml";};
    static inline string xmlVersion() { return "<? xml version=\"1.0\" standalone=\"no\" ?>"; };

    static inline string array3DToXmlString(double const array[4]) {
        return "\"" + to_string(array[0]) + ", " + to_string(array[1]) + ", " + to_string(array[2]) + "\"";
    }

    static inline string array3DToXmlString(int array[3]) {
        return "\"" + to_string(array[0]) + ", " + to_string(array[1]) + ", " + to_string(array[2]) + "\"";
    }

    static inline string array3DToXmlString(double *array) {
        return "\"" + to_string(array[0]) + ", " + to_string(array[1]) + ", " + to_string(array[2]) + "\"";
    }

    static inline string vecteur3DToXmlString(const Vecteur &v) {
        return array3DToXmlString(v.pt);
    };

    static inline string colorToXmlString(Color &c) {
        return "\"" + to_string(c.rgba[0]) + ", " + to_string(c.rgba[1]) + ", "
            + to_string(c.rgba[2]) + "\"";
    }
    
    static inline string xmlWorldOpen(int gridSize[3]) {
        return "<world gridSize=" + array3DToXmlString(gridSize) + " >";
    };    

    static inline string xmlWorldClose() { return "</world>"; };

    static inline string xmlCamera(Camera *camera) {
        return "<camera target=" + vecteur3DToXmlString(camera->getTarget()) + " "
            + "directionSpherical=" + vecteur3DToXmlString(camera->getDirectionSpherical()) + " "
            + "angle=\"" + to_string(camera->getAngle()) + "\" />";
    };

    static inline string xmlSpotlight(LightSource *spotlight) {
        return "<spotlight target=" + array3DToXmlString(spotlight->getTarget()) + " "
            + "directionSpherical=" + vecteur3DToXmlString(spotlight->getDirectionSpherical()) + " "
            + "angle=\"" + to_string(spotlight->getAngle()) + "\" />";
    };

    static inline string xmlBlinkyBlock(BlinkyBlocks::BlinkyBlocksBlock *bb) {
        return "<block position=" + vecteur3DToXmlString(bb->position)
            + " color=" + colorToXmlString(bb->color) + " >\n";
    }

    static inline string xmlBlockList(Color &color, double blockSize[3],
                                      map<int, BuildingBlock*> &blocks) {
        string str = string("<blockList color=");
        str = str + colorToXmlString(color) + " blockSize=" + array3DToXmlString(blockSize) + " >\n";
        map<int, BaseSimulator::BuildingBlock*>::iterator it;
        for(it = blocks.begin(); 
            it != blocks.end(); it++) {
            BlinkyBlocks::BlinkyBlocksBlock* bb = (BlinkyBlocks::BlinkyBlocksBlock*) it->second;
            str = str + xmlBlinkyBlock(bb);
        }

        str = str + "</blockList>";

        return str;
    }
}

#endif // CONFIGUTILS_H__
