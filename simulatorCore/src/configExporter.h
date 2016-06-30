
#ifndef CONFIGEXPORTER_H__
#define CONFIGEXPORTER_H__

#define TIXML_USE_STL	1
#include "TinyXML/tinyxml.h"

#include <iostream>
#include <vector>

#include "world.h"
#include "buildingBlock.h"
#include "openglViewer.h"
#include "camera.h"

using namespace std;

namespace BaseSimulator {

static int configCounter = 0;


/************************************************************
 *   Abstract Configuration Exporter
 ************************************************************/    

class ConfigExporter {      // Follows the Singleton pattern
    static inline string generateConfigFilename() {
        return "config" + to_string(configCounter++) + ".xml";
    };
protected:
    World *world;
    TiXmlDocument *config;
    string configName;
    TiXmlElement *worldElt;
    TiXmlElement *blockListElt;
public:
    ConfigExporter(World *world);
    virtual ~ConfigExporter();

    void exportConfiguration();
    void exportCameraAndLightSource();
    void exportWorld();
    void exportBlockList();
    void exportBlock(BuildingBlock *bb);

    virtual void exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb) {};
};


/************************************************************
 *   Subclasses
 ************************************************************/    

class BlinkyBlocksConfigExporter : public ConfigExporter {

public:
    BlinkyBlocksConfigExporter(World *_world) : ConfigExporter(_world) {};
    virtual ~BlinkyBlocksConfigExporter() { };
};

class Catoms3DConfigExporter : public ConfigExporter {
public:
    Catoms3DConfigExporter(World *_world) : ConfigExporter(_world) {};
    virtual ~Catoms3DConfigExporter() { };

    virtual void exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb);
};

class RobotBlocksConfigExporter : public ConfigExporter {
public:
    RobotBlocksConfigExporter(World *_world) : ConfigExporter(_world) {};
    virtual ~RobotBlocksConfigExporter() { };
};

class Catoms2DConfigExporter : public ConfigExporter {
public:
    Catoms2DConfigExporter(World *_world) : ConfigExporter(_world) {};
    virtual ~Catoms2DConfigExporter() { };

    virtual void exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb);
};

class SmartBlocksConfigExporter : public ConfigExporter {
public:
    SmartBlocksConfigExporter(World *_world) : ConfigExporter(_world) {};
    virtual ~SmartBlocksConfigExporter() { };
};

} // BASESIMULATOR_NAMESPACE

#endif // CONFIGEXPORTER_H__
