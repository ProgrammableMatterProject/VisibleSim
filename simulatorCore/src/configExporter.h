
#ifndef CONFIGEXPORTER_H__
#define CONFIGEXPORTER_H__

#include <iostream>
#include <vector>
#include "world.h"

using namespace std;

namespace BaseSimulator {

    class ConfigExporter {      // Follows the Singleton pattern

    protected:
        virtual static ConfigExporter* getConfigExporter() = 0;
        
    public:
        
    };

    // class ConfigExporter2D : ConfigExporter {
        
    // protected:
        
    // public:
        
    // };
    
    class ConfigExporter3D : ConfigExporter {
        
    protected:
        virtual static ConfigExporter* getConfigExporter() = 0;        
    public:
        
    };

    class BlinkyBlocksConfigExporter : ConfigExporter3D {

    public:
        BlinkyBlocksConfigExporter();
        BlinkyBlocksConfigExporter(const BlinkyBlocksConfigExporter*);
        BlinkyBlocksConfigExporter * operator = (BlinkyBlocksConfigExporter*);
        virtual static ConfigExporter* getConfigExporter();
    };

    // class Catoms3DConfigExporter : ConfigExporter3D {

    // };

    // class RobotBlocksConfigExporter : ConfigExporter3D {

    // };

    // class Catoms2DConfigExporter : ConfigExporter2D {

    // };

    // class SmartBlocksConfigExporter : ConfigExporter2D {

    // };
        

}

#endif // CONFIGEXPORTER_H__
