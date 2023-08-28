/*
 * @file world.h
 *
 *  Created on: 23 mars 2013
 *      Author: dom
 */

#ifndef WORLD_H_
#define WORLD_H_

#include <random>
#include <iostream>
#include <map>
#include <vector>
#include <mutex>
#include <unordered_map>

#include <cassert>
#include "buildingBlock.h"
#include "glBlock.h"
#include "../utils/tDefs.h"
#include "../utils/trace.h"
#include "../utils/utils.h"
#include "../math/cell3DPosition.h"
#include "../grid/lattice.h"
#include "../events/scheduler.h"
#include "../gui/objLoader.h"
#include "../replay/replayTags.h"

#ifdef WIN32
const string textureDirectory = string(ROOT_DIR) + "/simulatorCore/resources/textures/";
#else
const string textureDirectory = "../../simulatorCore/resources/textures/";
#endif

const string menuTextureDirectory = textureDirectory + "menuTextures/";

using namespace BaseSimulator::utils;
using namespace std;

namespace BaseSimulator {

/**
 * @class World
 * @brief Represents the simulation world and manages all blocks
 */
    class World {
        std::mutex mutex_gl;
    public:
        /************************************************************
         *   Global variable
         ************************************************************/
        static World *world;        //!< Global variable to access the single simulation instance of World
        // static vector<GlBlock*>tabGlBlocks; //!< A vector containing pointers to all graphical blocks
        static unordered_map<bID, GlBlock *> mapGlBlocks; //!< A hash map containing pointers to all graphical blocks, indexed by block id
        static map<bID, BuildingBlock *> buildingBlocksMap; //!< A map containing all BuildingBlocks in the world, indexed by their blockId

        /************************************************************
         *   Graphical / UI Attributes
         ************************************************************/
        //bool background = true; //!< Option for visible background /// replaced by showGrid
        GlBlock *selectedGlBlock; //!< A pointer to the GlBlock selected by the user
        uint8_t numSelectedFace; //!< The id of the face (NeighborDirection) selected by the user
        GLuint numSelectedGlBlock; //!< The index of the block selected by the user in the tabGlBlock

        ObjLoader::ObjLoader *objBlock = nullptr;           //!< Object loader for a block
        ObjLoader::ObjLoader *objBlockForPicking = nullptr; //!< Object loader for a block used during picking
        ObjLoader::ObjLoader *objRepere = nullptr;          //!< Object loader for the frame
        GLint menuId;

        bool isBlinkingBlocks = false;
        Camera *camera = nullptr; //!< Pointer to the camera object for the graphical simulation, also includes the light source

        /************************************************************
         *   Simulation Attributes
         ************************************************************/

        bID maxBlockId = 0; //!< The block id of the block with the highest id in the world
        // vector<ScenarioEvent&> tabEvents;

        /**
         * @brief World constructor, initializes the camera, light, and user interaction attributes
         */
        World(int argc, char *argv[]);

        /**
         * @brief World destructor, deletes the blocks and their GL counterparts, the lattice and camera
         */
        virtual ~World();

    public:
        Lattice *lattice;           //!< The lattice on which the blocks are placed, manages the blocks positions and neighborhoods

        /**
         * @brief Returns the global instance of world, or raises an error if it has not been allocated
         */
        static World *getWorld() {
            assert(world != nullptr);
            return (world);
        }

        /**
         * @brief Sets the value of static world variable
         * @param _world pointer to a world instance
         */
        static void setWorld(World *_world) {
            world = _world;
        }

        /**
         * @brief Global function to call the world destructor
         */
        static void deleteWorld() {
            delete (world);
            world = nullptr;
        }

        /**
         * Return an ID of the type of current Blocks
         * @return uint8_t value of Block type from 'replayTags.h' list
         */
        virtual ReplayTags::u1 getBlockType() = 0;

        /**
         * @brief Getter for the map containing all blocks of the world
         */
        map<bID, BuildingBlock *> &getMap() {
            return buildingBlocksMap;
        }

        /**
         * @brief Getter for the map containing all Gl blocks of the world
         */
        unordered_map<bID, GlBlock *> &getMapGl() {
            return mapGlBlocks;
        }

        /**
         * @brief Returns the number of blocks in the world
         * @return Number of blocks in the world
         */
        inline int getSize() { return buildingBlocksMap.size(); };

        /**
         * @brief Prints a string identifying the world to OUTPUT
         */
        inline void printInfo() { OUTPUT << "I'm a World" << endl; };

        /**
         * Returns a boolean indicating if a block can be added to face #numSelectedFace
         *  of block identified by numSelectedGlBlock
         *
         * @param numSelectedGlBlock id of selected block
         * @param numSelectedFace id of face to consider
         * @return true if corresponding cell is free and inside the grid, false otherwise
         */
        bool canAddBlockToFace(bID numSelectedGlBlock, uint8_t numSelectedFace);

        /**
         * @brief Returns a pointer to the block of id BId
         * @param bId : id of the block to get
         * @return a pointer to block of id bId, or nullptr if it does not exist
         */
        virtual BuildingBlock *getBlockById(int bId);

        /**
         * @brief Returns a pointer to the block of id BId
         * @param pos : position of the block to get
         * @return a pointer to block of id bId, or nullptr if it does not exist
         */
        BuildingBlock *getBlockByPosition(const Cell3DPosition &pos);

        /**
         * @brief Updates color and position of glBlock associated with block bb
         *
         * @param bb : Block to update
         */
        [[deprecated]] virtual void updateGlData(BuildingBlock *bb);

        /**
         * @brief Set color c to glBlock associated with block bb
         *
         * Used by setColor
         *
         * @param blc : Block to update
         * @param p : Position to set to blc's glBlock
         */
        virtual void updateGlData(BuildingBlock *bb, const Color &c);

        /**
         * @brief Set position p to glBlock associated with block blc
         *	 *
         * @param blc : Block to update
         * @param p : Position in the grid to set to blc's glBlock
         */
        virtual void updateGlData(BuildingBlock *bb, const Cell3DPosition &p);

        /**
         * @brief Set position p to glBlock associated with block blc
         *
         * Used when glBlocks and their corresponding BuildingBlock have different positions,
         *  as it is the case during motion events
         *
         * @param blc : Block to update
         * @param p : Position to set to blc's glBlock
         */
        virtual void updateGlData(BuildingBlock *blc, Vector3D &p);

        /**
         * @brief Creates a block and adds it to the simulation
         *
         * @param blockId : id of the block to be created. If 0, its id will be set to the MAX_CURRENT_ID + 1
         * @param bcb : a pointer to the user fonction return the CodeBlock to execute on the block
         * @param pos : the position of the block on the lattice grid
         * @param col : the color of the block
         * @param orient : For C2D, the rotation angle of the block on its axis.
         *                      For C3D, the number of the block's connector on the x axis.
         *                      0 by default and for all other blocks
         */
        virtual void addBlock(bID blockId, BlockCodeBuilder bcb,
                              const Cell3DPosition &pos, const Color &col,
                              uint8_t orient = 0) = 0;

        /**
         * @brief Deletes a block from the simulation after disconnecting it and all of
         *  its neighbors and notifying them
         *
         * @param blc : a pointer to the block to remove from the world
         */
        void deleteBlock(BuildingBlock *blc);

        /**
         * @brief Connects the interfaces of a block to all of its neighbors and notifiy them
         * @param blc : a pointer to the block to connect to its neighborhood
         * @param count indicates whether the inserted modules should be counted towards nbModules
         */
        void connectBlock(BuildingBlock *block, bool count = true);

        /**
         * @brief Disconnects the interfaces of a block from all of its neighbors and notify them
         * @param blc : a pointer to the block to disconnect from its neighborhood
         * @param count indicates whether the removed modules should be discounted from nbModules
         */
        void disconnectBlock(BuildingBlock *block, bool count = true);

        /**
         * @brief add an obstacle to the grid as a disabled block
         * @param pos : position of the inactive block
         * @param col : color of the obstacle
         */
        void addObstacle(const Cell3DPosition &pos, const Color &col);

        /**
         * @brief Getter for selectedGlBlock
         *
         * @return pointer to the block selected by the user, or nullptr
         */
        virtual GlBlock *getselectedGlBlock() { return selectedGlBlock; };

        /**
         * @brief Setter for selectedGlBlock, updates the value of selected block with the block of id n, and returns it
         * @param n : id of the new selectedGlBlock
         * @return a pointer to the selected GlBlock
         */
        inline GlBlock *setselectedGlBlock(int n) {
            auto const &glBlock = mapGlBlocks.find(n);

            if (glBlock != mapGlBlocks.end()) {
                selectedGlBlock = (*glBlock).second;
                numSelectedGlBlock = n;
            } else {
                selectedGlBlock = nullptr;
                numSelectedGlBlock = -1;
            }
            return selectedGlBlock;
        };

        /**
         * @brief Setter for selected picking face
         * @param n : id of the texture that has been clicked by the user
         *  This function retrieves the names of the picking textures for the compares it to set the
         *   numSelectedFace variable to the corresponding face
         */
        virtual void setSelectedFace(int n) = 0;

/**
     * @brief Returns the Glblock of id n
     * @param n : id of the Glblock to retrieve
     */
        inline BuildingBlock *getBlock(bID n) {
            auto const &block = buildingBlocksMap.find(n);
            return block != buildingBlocksMap.end() ? (*block).second : nullptr;
        };


/**
     * @brief Returns the Glblock of id n
     * @param n : id of the Glblock to retrieve
     */
        inline GlBlock *getGlBlock(bID n) {
            auto const &glBlock = mapGlBlocks.find(n);
            return glBlock != mapGlBlocks.end() ? (*glBlock).second : nullptr;
        };

        /**
         * @brief Returns the total number of blocks in the world
         * @return the number of blocks in the world
         */
        inline int getNbBlocks() { return buildingBlocksMap.size(); };

        /**
         * @brief Locks the world mutex to avoid concurrency issues with the gl process
         */
        inline void lock() { mutex_gl.lock(); };

        /**
         * @brief Unlocks the world mutex to re-enable access from the gl process
         */
        inline void unlock() { mutex_gl.unlock(); };

        /**
         * @brief Draws the environment of the world and all included blocks
         */
        virtual void glDraw() {};

        /**
         * @brief Draws all blocks for shadows, list of objects that produce shadows
         */
        virtual void glDrawShadows() { glDraw(); };

        /**
         * @brief Draws the block ids of the block contained in the world
         */
        virtual void glDrawId() {};

        /**
         * @brief Draws the blocks material used for user interactions
         */
        virtual void glDrawIdByMaterial() {};

        /**
         * @brief Draws the world background
         */
        virtual void glDrawBackground() {};

        /**
         * @brief Linearly scans the grid for blocks and calls linkBlock to connect the interfaces of neighbors
         */
        void linkBlocks();

        /**
         * @brief Updates the neighborhood of all alive neighbors of cell pos
         * @param pos : Position of the block whose neighbors need an update
         */
        void linkNeighbors(const Cell3DPosition &pos);

        /**
         * @brief Connects block on grid cell pos to its neighbor
         * @param pos : Position of the block to connect
         */
        virtual void linkBlock(const Cell3DPosition &pos) = 0;

        /**
         * @brief Displays an interactive popup menu at coordinates (ix, iy)
         *
         * @param ix x coordinate of popup menu display location
         * @param iy y coordinate of popup menu display location
         */
        virtual void createPopupMenu(int ix, int iy);

        /**
         * @brief Creates a new help window at a fixed location of the screen
         */
        virtual void createHelpWindow();

        /**
         * @brief Getter for the camera instance of the simulation
         * @return a pointer to the camera instance of the simulation
         */
        virtual Camera *getCamera() { return camera; };

        /**
         * @brief Handles a user click on one of the options from the menu
         * @param id of the clicked menu button
         */
        virtual void menuChoice(int);

        /**
         * @brief Exports the current world configuration to an XML file. Triggered from the menu.
         */
        virtual void exportConfiguration() = 0;

        /**
         * @brief Sets the path to the texture folder for drawing
         */
        virtual void loadTextures(const string &str) {};

        /**
         * Get the number of selected face and -1 if none
         */
        virtual int getNumSelectedFace() {
            return numSelectedFace;
        }

        /**
         * @brief Returns the BuildingBlock corresponding to the selected GlBlock
         * @return a pointer to the BuildingBlock corresponding to the selected GlBlock, or nullptr if there is none
         */
        inline BuildingBlock *getSelectedBuildingBlock() {
            auto const &glBlock = mapGlBlocks.find(numSelectedGlBlock);
            return glBlock != mapGlBlocks.end() ? getBlockById((*glBlock).second->blockId) : nullptr;
        };

        /**
         * @brief Schedules a tap event for block with id bId, at time date.
         *
         * @param date the date at which the tap event must be consumed
         * @param bId the id of the target block
         * @param face id of the tapped face, or -1 if not a picking face
         */
        void tapBlock(Time date, bID bId, int face);

        /**
         * @brief Stops all block in the world
         */
        void stopSimulation();

        /**
         * @brief Increment the maximum block id present the world by one and returns it
         * @return the maximum block id present in the world + 1
         */
        inline bID incrementBlockId() { return ++maxBlockId; }
        /**
         * @brief Toggle world background
         */
        // void toggleBackground() { background = !background; } // replaced by showGrid

        /**
         * \brief Export a 3D model in STL format to print the whole configuration
         * \param title : title of the STL file
         * \result Returns true if the faces was well written
         */
        virtual bool exportSTLModel(string title) { return false; };

        /**
        * @brief get bounding box coordinate from centers of glBlocks
        */
        void getBoundingBox(float &xmin, float &ymin, float &zmin, float &xmax, float &ymax, float &zmax);

        bool hasBlinkingBlocks() { return isBlinkingBlocks; };

        virtual void onEndOfSimulation() {
            buildingBlocksMap.begin()->second->blockCode->onEndOfSimulation();
        }
    };

/**
 * @brief Global function to call the world destructor
 */
    inline void deleteWorld() {
        World::deleteWorld();
    }

/**
 * @brief Global getter for the world
 */
    static inline World *getWorld() { return (World::getWorld()); }

/**
 * @brief Global setter for the world
 */
    static inline void setWorld(World *_world) { World::setWorld(_world); }


} // BaseSimulator namespace

#endif /* WORLD_H_ */
