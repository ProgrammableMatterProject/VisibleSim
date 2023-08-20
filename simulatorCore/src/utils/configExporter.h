/**
 * @file configExporter.h
 * Header for Configuration Exporter module
 */


#ifndef CONFIGEXPORTER_H__
#define CONFIGEXPORTER_H__

#define TIXML_USE_STL    1

#include "../deps/TinyXML/tinyxml.h"

#include "../base/world.h"
#include "../base/buildingBlock.h"
#include "../gui/openglViewer.h"
#include "../gui/camera.h"

using namespace std;

namespace BaseSimulator {

/************************************************************
 *   Abstract Configuration Exporter
 ************************************************************/

/**
 * @brief Abstract Configuration Exporter
 *
 * Saves all the information in the world at time of export into an xml file
 *  with name config_hh_mm_ss.xml. The following elements are exported:
 *  1. Properties of the world (dimensions, scale)
 *  2. The current state of the camera and lightsource
 *  3. The list of blocks and their current attributes.
 *    (Common ones + type specific ones exported by the virtual function exportAdditionalAttribute)
 */
    class ConfigExporter {
    protected:
        World *world;          //!< pointer to the world to export
        TiXmlDocument *config; //!< the TiXML Document used for export
        string configName;     //!< the name of the output configuration file
        //TiXmlElement *worldElt; //!< a pointer to the world XML element of the document
        TiXmlElement *vsElt; //!< a pointer to the root XML element of the document
        TiXmlElement *blockListElt; //!< a pointer to the blockList XML element of the document
    public:
        /**
         * @brief Constructor for the abstract configuration exporter
         *  Creates the output document, filename and header
         */
        ConfigExporter(World *world);

        /**
         * @brief Constructor for the abstract configuration exporter
         *  Creates the output document, filename and header
         */
        ConfigExporter(World *world, const string &_filename);

        /**
         * @brief Destructor for the abstract configuration exporter
         *  Deletes the TiXMLDocument used for exporting
         */
        virtual ~ConfigExporter();

        /**
         * @brief Main function of the configuration exporter, calls all export subfunctions sequentially.
         */
        void exportConfiguration();

        /**
         * @brief Exports the camera and lightSource (Current position and orientation) to the configuration file.
         */
        void exportCameraAndLightSource(TiXmlElement *worldElt);

        /**
         * @brief Exports the world and window attributes to the configuration file
         */
        void exportVisuals(TiXmlElement *vsElt);

        /**
         * @brief Exports the world and window attributes to the configuration file
         */
        void exportWorld(TiXmlElement *vsElt);

        /**
         * @brief Initializes the blockList XML element and calls exportBlock on each block for export.
         *  The default color of the blockList will be the one of the user selected block.
         */
        void exportBlockList(TiXmlElement *worldElt);

        /**
         * @brief Exports all the generic attributes of a BuildingBlock
         * @param bb : Pointer to the block to export
         *  If exporting a block family specific attribute is needed, the exportAdditionalAttribute can be used.
         */
        void exportBlock(BuildingBlock *bb);

        /**
         * @brief Exports additional non-generic attributes from block bb.
         * @param bbElt : the TiXmlElement for the current block
         * @param bb : the current block
         */
        virtual void exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb) {};
    };


/************************************************************
 *   Subclasses
 ************************************************************/

/**
 * @brief BlinkyBlocks Configuration Exporter
 */
    class BlinkyBlocksConfigExporter : public ConfigExporter {

    public:
        /**
         * @brief BlinkyBlocks Configuration Exporter constructor
         */
        BlinkyBlocksConfigExporter(World *_world) : ConfigExporter(_world) {};

        BlinkyBlocksConfigExporter(World *_world, const string &_filename)
                : ConfigExporter(_world, _filename) {};

        /**
         * @brief BlinkyBlocks Configuration Exporter destructor
         */
        virtual ~BlinkyBlocksConfigExporter() {};
    };

/**
 * @brief Catoms3D Configuration Exporter
 */
    class Catoms3DConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief Catoms3D Configuration Exporter constructor
         */
        Catoms3DConfigExporter(World *_world) : ConfigExporter(_world) {};

        Catoms3DConfigExporter(World *_world, const string &_filename)
                : ConfigExporter(_world, _filename) {};

        /**
         * @brief Catoms3D Configuration Exporter destructor
         */
        virtual ~Catoms3DConfigExporter() {};

        /**
         * @copydoc ConfigExporter::exportAdditionalAttribute
         *  Exports the rotation attribute of a Catoms3DBlock
         */
        virtual void exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb) override;
    };

/**
 * @brief SlidingCubes Configuration Exporter
 */
    class SlidingCubesConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief SlidingCubes Configuration Exporter constructor
         */
        SlidingCubesConfigExporter(World *_world) : ConfigExporter(_world) {};

        SlidingCubesConfigExporter(World *_world, const string &_filename)
                : ConfigExporter(_world, _filename) {};

        /**
         * @brief SlidingCubes Configuration Exporter destructor
         */
        virtual ~SlidingCubesConfigExporter() {};
    };

/**
 * @brief SmartBlocks Configuration Exporter
 */
    class SmartBlocksConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief SmartBlocks Configuration Exporter constructor
         */
        SmartBlocksConfigExporter(World *_world) : ConfigExporter(_world) {};

        SmartBlocksConfigExporter(World *_world, const string &_filename)
                : ConfigExporter(_world, _filename) {};

        /**
         * @brief SmartBlocks Configuration Exporter constructor
         */
        virtual ~SmartBlocksConfigExporter() {};
    };

/**
 * @brief MultiRobots Configuration Exporter
 */
    class MultiRobotsConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief MultiRobots Configuration Exporter constructor
         */
        MultiRobotsConfigExporter(World *_world) : ConfigExporter(_world) {};

        MultiRobotsConfigExporter(World *_world, const string &_filename)
                : ConfigExporter(_world, _filename) {};

        /**
         * @brief MultiRobots Configuration Exporter constructor
         */
        virtual ~MultiRobotsConfigExporter() {};
    };

/**
 * @brief SlidingCubes Configuration Exporter
 */
    class OkteenConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief SlidingCubes Configuration Exporter constructor
         */
        OkteenConfigExporter(World *_world) : ConfigExporter(_world) {};

        OkteenConfigExporter(World *_world, const string &_filename)
                : ConfigExporter(_world, _filename) {};

        /**
         * @brief SlidingCubes Configuration Exporter destructor
         */
        virtual ~OkteenConfigExporter() {};
    };

/**
 * @brief Datoms Configuration Exporter
 */
    class DatomsConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief Datoms Configuration Exporter constructor
         */
        DatomsConfigExporter(World *_world) : ConfigExporter(_world) {};

        DatomsConfigExporter(World *_world, const string &_filename)
                : ConfigExporter(_world, _filename) {};

        /**
         * @brief Datoms Configuration Exporter destructor
         */
        virtual ~DatomsConfigExporter() {};

        /**
         * @copydoc ConfigExporter::exportAdditionalAttribute
         *  Exports the rotation attribute of a Datoms3DBlock
         */
        virtual void exportAdditionalAttribute(TiXmlElement *bbElt, BuildingBlock *bb) override;
    };

/**
 * @brief Nodes2D Configuration Exporter
 */
    class Nodes2DConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief Nodes2D Configuration Exporter constructor
         */
        Nodes2DConfigExporter(World *_world) : ConfigExporter(_world) {};

        /**
         * @brief Nodes2D Configuration Exporter destructor
         */
        virtual ~Nodes2DConfigExporter() {};
    };

/**
 * @brief Hexanodes Configuration Exporter
 */
    class HexanodesConfigExporter : public ConfigExporter {
    public:
        /**
         * @brief Node Configuration Exporter constructor
         */
        HexanodesConfigExporter(World *_world) : ConfigExporter(_world) {};

        /**
         * @brief Node Configuration Exporter destructor
         */
        virtual ~HexanodesConfigExporter() {};
    };

} // BASESIMULATOR_NAMESPACE

#endif // CONFIGEXPORTER_H__
