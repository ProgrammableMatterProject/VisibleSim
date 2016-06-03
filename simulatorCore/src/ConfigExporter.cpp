
#include "configExporter.h"

using namespace BlinkyBlocks;

namespace BaseSimulator {

    ConfigExporter::ConfigExporter() {
        config = new TiXmlDocument();
        configName = generateConfigFilename();
        config->LinkEndChild(new TiXmlDeclaration("1.0", "", "no"));
    }

    ConfigExporter::~ConfigExporter() {
        delete config;
    }

    BlinkyBlocksConfigExporter::BlinkyBlocksConfigExporter(BlinkyBlocksWorld *_world)
        : ConfigExporter() {
        world = _world;
        camera = world->getCamera();
    }

    void ConfigExporter::exportConfiguration() {
        exportWorld();
        exportCameraAndLightSource();
        exportBlockList();
        config->SaveFile(configName);

        cerr << "Configuration exported to file: " << configName << endl;
    }

    void ConfigExporter::exportCameraAndLightSource() {
        // Export Camera
        TiXmlElement *cam = new TiXmlElement("camera");
        Vecteur target = camera->getTarget();
        Vecteur ds = camera->getDirectionSpherical();
        cam->SetAttribute("target", toXmlAttribute(target.pt[0],
                                                   target.pt[1],
                                                   target.pt[2]));
        cam->SetAttribute("directionSpherical", toXmlAttribute(ds.pt[0],
                                                               ds.pt[1],
                                                               ds.pt[2]));
        cam->SetAttribute("angle", camera->getAngle());        
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
        spotlight->SetAttribute("angle", ls->getAngle());        
        worldElt->LinkEndChild(spotlight);
        config->LinkEndChild(worldElt);
    }
    
    void BlinkyBlocksConfigExporter::exportWorld() {
        worldElt = new TiXmlElement("world");
        const int *gridSize = world->getGridSize();
        worldElt->SetAttribute("gridSize", toXmlAttribute(gridSize[0],
                                                          gridSize[1],
                                                          gridSize[2]));
        worldElt->SetAttribute("windowSize",
                               toXmlAttribute(GlutContext::screenWidth, GlutContext::screenHeight));
    }

    void BlinkyBlocksConfigExporter::exportBlockList() {
        blockListElt = new TiXmlElement("blockList");
        BlinkyBlocksBlock *bb = (BlinkyBlocksBlock *)world->getMenuBlock();
        float *color = bb->color.rgba;
        float *blockSize = world->getBlockSize();
        map<int, BaseSimulator::BuildingBlock*> blocks = world->getMap();
        
        blockListElt->SetAttribute("color", toXmlAttribute(color[0] * 255,
                                                           color[1] * 255,
                                                           color[2] * 255));
        blockListElt->SetAttribute("blockSize", toXmlAttribute(blockSize[0],
                                                               blockSize[1],
                                                               blockSize[2]));
        
        map<int, BaseSimulator::BuildingBlock*>::iterator it;        
        for(it = blocks.begin(); 
            it != blocks.end(); it++) {
            exportBlock(it->second);
        }
        
        worldElt->LinkEndChild(blockListElt);
    }

    void BlinkyBlocksConfigExporter::exportBlock(BuildingBlock *bb) {
        BlinkyBlocksBlock *blc = (BlinkyBlocksBlock *)bb;
        TiXmlElement *bbElt = new TiXmlElement("block");
        float *color = blc->color.rgba;
        Vecteur pos = blc->position;
        
        bbElt->SetAttribute("position", toXmlAttribute(pos.pt[0],
                                                      pos.pt[1],
                                                      pos.pt[2]));
        bbElt->SetAttribute("color", toXmlAttribute(color[0] * 255,
                                                    color[1] * 255,
                                                    color[2] * 255));

        blockListElt->LinkEndChild(bbElt);
    }
}
