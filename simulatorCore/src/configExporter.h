
#ifndef CONFIGEXPORTER_H__
#define CONFIGEXPORTER_H__

#define TIXML_USE_STL	1
#include "TinyXML/tinyxml.h"

#include <iostream>
#include <vector>
#include "world.h"
#include "BlinkyBlocksWorld.h"
#include "BlinkyBlocksBlock.h"
#include "BuildingBlock.h"
#include "openGlViewer.h"
#include "camera.h"

using namespace std;

namespace BaseSimulator {

    static int configCounter = 0;

    class ConfigExporter {      // Follows the Singleton pattern
        static inline string generateConfigFilename() {
            return "config" + to_string(configCounter++) + ".xml";
        };
    protected:
        Camera *camera;
        TiXmlDocument *config;
        string configName;
        TiXmlElement *worldElt;
        TiXmlElement *blockListElt;
    public:
        ConfigExporter();
        virtual ~ConfigExporter();

        void exportConfiguration();
        void exportCameraAndLightSource();

        virtual void exportWorld() = 0;
        virtual void exportBlockList() = 0;
        virtual void exportBlock(BuildingBlock *bb) = 0;
    };

    // // class ConfigExporter2D : ConfigExporter {
        
    // // protected:
        
    // // public:
        
    // // };
    
    // class ConfigExporter3D : ConfigExporter {
        
    // protected:
    //     virtual static ConfigExporter* getConfigExporter() = 0;        
    // public:
        
    // };

    using namespace BlinkyBlocks;
    
    class BlinkyBlocksConfigExporter : public ConfigExporter { // : ConfigExporter3D
        BlinkyBlocksWorld *world;
    public:
        BlinkyBlocksConfigExporter(BlinkyBlocksWorld *_world);
        virtual ~BlinkyBlocksConfigExporter() { };

        virtual void exportWorld();
        virtual void exportBlockList();
        virtual void exportBlock(BuildingBlock *bb);
    };
    
    // class Catoms3DConfigExporter : ConfigExporter3D {

    // };

    // class RobotBlocksConfigExporter : ConfigExporter3D {

    // };

    // class Catoms2DConfigExporter : ConfigExporter2D {

    // };

    // class SmartBlocksConfigExporter : ConfigExporter2D {

    // };        

    template<typename T>
    static string toXmlAttribute(T a, T b) {
        std::ostringstream out;
        out << a << "," << b;
        return out.str();       
    }
    template string toXmlAttribute<int>(int, int);

    template<typename T>
    static string toXmlAttribute(T a, T b, T c) {
        std::ostringstream out;
        out << a << "," << b << "," << c ;
        return out.str();       
    }
    template string toXmlAttribute<int>(int, int, int);
    template string toXmlAttribute<double>(double, double, double);
    template string toXmlAttribute<short>(short, short, short);
    template string toXmlAttribute<float>(float, float, float);   

}
#endif // CONFIGEXPORTER_H__
