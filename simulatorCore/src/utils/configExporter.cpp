
#include <iostream>
#include <vector>

#include "configExporter.h"
#include "../base/simulator.h"
#include "../robots/catoms3D/catoms3DBlock.h"
#include "../robots/datoms/datomsBlock.h"
#include "../robots/catoms2D/catoms2DBlock.h"
#include "utils.h"

namespace BaseSimulator {

/************************************************************
 *   XML Utilities
 ************************************************************/

/**
 * @brief Formats two variable for XML attribute export
 * @return a string with format "x,y"
 */
template<typename T>
static string toXmlAttribute(T a, T b) {
    std::ostringstream out;
    out << a << "," << b;
    return out.str();
}
template string toXmlAttribute<int>(int, int);

/**
 * @brief Formats three variable for XML attribute export
 * @return a string with format "x,y,z"
 */
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

/**
 * @brief Formats a Cell3DPosition variable for XML attribute export
 * @return a string with format "pos.x,pos.y,pos.z"
 */
string toXmlAttribute(Cell3DPosition &pos) {
    return toXmlAttribute(pos[0], pos[1], pos[2]);
}

/**
 * @brief Formats a Vector3D variable for XML attribute export
 * @return a string with format "pos.x,pos.y,pos.z"
 */
string toXmlAttribute(Vector3D &pos) {
    return toXmlAttribute(pos[0], pos[1], pos[2]);
}

/************************************************************
 *   Configuration Exporters Implementation
 ************************************************************/

ConfigExporter::ConfigExporter(World *_world) {
    world = _world;
    config = new TiXmlDocument();

    string exportedConfigNameRoot;
    if (Simulator::configFileName.empty()) {
        exportedConfigNameRoot = "export";
    } else {
        size_t config_pos = Simulator::configFileName.find("config_");
        if (config_pos == string::npos) // no config_ pattern
            exportedConfigNameRoot = string("export_").append(Simulator::configFileName);
        else
            exportedConfigNameRoot = string("export_")
                .append(Simulator::configFileName.substr(config_pos + 7, string::npos));

        // trim extension
        exportedConfigNameRoot = exportedConfigNameRoot
            .substr(0, exportedConfigNameRoot.size()-4);
    }

    configName = Simulator::regrTesting ?
        ".confCheck.xml" : generateTimestampedFilename(exportedConfigNameRoot, "xml");
    config->LinkEndChild(new TiXmlDeclaration("1.0", "", "no"));
}

ConfigExporter::ConfigExporter(World *_world, const string& _filename) {
    world = _world;
    config = new TiXmlDocument();
    configName = _filename;
    config->LinkEndChild(new TiXmlDeclaration("1.0", "", "no"));
}

ConfigExporter::~ConfigExporter() {
    delete config;
}

void ConfigExporter::exportConfiguration() {
    exportWorld();
    exportCameraAndLightSource();
    exportBlockList();
    config->SaveFile(configName);

    cerr << "Configuration exported to file: " << configName << endl;
}

void ConfigExporter::exportCameraAndLightSource() {
    if (GlutContext::GUIisEnabled) {
        // Export Camera
        Camera *camera = world->getCamera();

        TiXmlElement *cam = new TiXmlElement("camera");
        Vector3D target = camera->getTarget();
        Vector3D ds = camera->getDirectionSpherical();
        cam->SetAttribute("target", toXmlAttribute(target.pt[0],
                                                   target.pt[1],
                                                   target.pt[2]));
        cam->SetAttribute("directionSpherical", toXmlAttribute(ds.pt[0],
                                                               ds.pt[1],
                                                               ds.pt[2]));
        cam->SetAttribute("angle", to_string(camera->getAngle()));
        cam->SetAttribute("near", to_string(camera->getNearPlane()));
        cam->SetAttribute("far", to_string(camera->getFarPlane()));

        worldElt->LinkEndChild(cam);

        // Export LightSource
        TiXmlElement *spotlight = new TiXmlElement("spotlight");
        LightSource *ls = &camera->ls;
        float *lsTarget = ls->getTarget();
        ds = ls->getDirectionSpherical();

        spotlight->SetAttribute("target", toXmlAttribute(lsTarget[0],
                                                         lsTarget[1],
                                                         lsTarget[2]));
        spotlight->SetAttribute("directionSpherical", toXmlAttribute(ds.pt[0],
                                                                     ds.pt[1],
                                                                     ds.pt[2]));
        spotlight->SetAttribute("angle", to_string(ls->getAngle()));
        worldElt->LinkEndChild(spotlight);
    }
}

void ConfigExporter::exportWorld() {
    worldElt = new TiXmlElement("world");
    worldElt->SetAttribute("gridSize", toXmlAttribute(world->lattice->gridSize));
    if (GlutContext::GUIisEnabled) {
        worldElt->SetAttribute("visual",
                               toXmlAttribute(GlutContext::screenWidth, GlutContext::screenHeight));
    }

    config->LinkEndChild(worldElt);
}

void ConfigExporter::exportBlockList() {
    blockListElt = new TiXmlElement("blockList");
    Vector3D blockSize = world->lattice->gridScale;
    map<bID, BaseSimulator::BuildingBlock*> blocks = world->getMap();
    blockListElt->SetAttribute("blockSize", toXmlAttribute(blockSize));

    for(auto const& idBBPair : blocks) {
        if (idBBPair.second->getState() != BuildingBlock::REMOVED
            and (idBBPair.second->ptrGlBlock and idBBPair.second->ptrGlBlock->isVisible()))
            exportBlock(idBBPair.second);
    }

    worldElt->LinkEndChild(blockListElt);
}

void ConfigExporter::exportBlock(BuildingBlock *bb) {
    BuildingBlock *blc = bb;
    TiXmlElement *bbElt = new TiXmlElement("block");
    float *color = blc->color.rgba;
    Cell3DPosition pos = blc->position;

    bbElt->SetAttribute("position", toXmlAttribute(pos));
    bbElt->SetAttribute("color", toXmlAttribute(color[0] * 255,
                                                color[1] * 255,
                                                color[2] * 255));
    if (bb->isMaster)
            bbElt->SetAttribute("master", "true");

    exportAdditionalAttribute(bbElt, bb);

    blockListElt->LinkEndChild(bbElt);
}

void Catoms3DConfigExporter::exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb) {
    bbElt->SetAttribute("orientation", static_cast<Catoms3D::Catoms3DBlock *>(bb)->orientationCode);
}

void Catoms2DConfigExporter::exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb) {
    bbElt->SetAttribute("angle", static_cast<Catoms2D::Catoms2DBlock *>(bb)->angle);
}

void DatomsConfigExporter::exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb) {
    bbElt->SetAttribute("orientation", static_cast<Datoms::DatomsBlock *>(bb)->orientationCode);
}

}
