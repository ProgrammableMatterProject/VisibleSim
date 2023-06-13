/**
 *  @file buildingBlock.h
 *  @date 22 mars 2013
 *  @author: dom, bpiranda
 *  @brief Defines a an abstract single module of the simulated ensemble
 */

#ifndef BUILDINGBLOCK_H_
#define BUILDINGBLOCK_H_

#include <list>
#include <random>
#include <atomic>
#include <memory>
#include <cstddef>

#include "../utils/tDefs.h"
#include "../base/glBlock.h"
#include "../base/blockCode.h"
#include "../clock/clock.h"
#include "math/cell3DPosition.h"
#include "../stats/statsIndividual.h"
#include "../utils/random.h"

class Event;

typedef std::shared_ptr<Event> EventPtr;

using namespace std;

class P2PNetworkInterface;

namespace BaseSimulator {

    class BlockCode;

    class BuildingBlock;

    class Clock;

    typedef BlockCode *(*BlockCodeBuilder)(BuildingBlock *);

//===========================================================================================================
//
//          BuildingBlock  (class)
//
//===========================================================================================================

/**
 * \brief Abstract parent class of any block
 */
    class BuildingBlock {
    public:
        //!< State of a BuildingBlock. The block is considered alive if its state is >= 2
        enum State {
            STOPPED = 0, REMOVED = 1, ALIVE = 2, COMPUTING = 3, MOVING = 4, ACTUATING = 5
        };
    private:
        /**
         * \brief state of the block, with atomic access
         */
        std::atomic<State> state;
    protected:
        static bID nextId;
        static bool userConfigHasBeenParsed; //!< Indicates if the user parsing as already been performed by blockCode->parseUserElements. Used to ensure that user configuration is parsed only once.

        vector<P2PNetworkInterface *> P2PNetworkInterfaces; //!< Vector of size equal to the number of interfaces of the block, contains pointers to the block's interfaces

        list <EventPtr> localEventsList; //!< List of local events scheduled for this block
    public:
        bID blockId; //!< id of the block
        uintRNG generator; //!< random number generator
        BlockCode *blockCode; //!< blockcode program executed by the block
        Clock *clock; //!< internal clock of the block
        Color color; //!< color of the block
        Cell3DPosition position; //!< position of the block in the grid of cells;
        GlBlock *ptrGlBlock; //!< ptr to the GL object corresponding to this block
        BlockCodeBuilder buildNewBlockCode; //!< function ptr to the block's blockCodeBuilder
        uint8_t orientationCode; //!< Identifier of the modules connector's along the x-axis
        utils::StatsIndividual *stats = NULL; //!< Module stats collected during the simulation
        /**
         * @brief BuildingBlock constructor
         * @param bId : the block id of the block to create
         * @param bcb : function pointer to the getter for the block's CodeBlock
         * @param nbInterfaces : number of initial interfaces of the block
         * @param seed : seed used to create the block random generator
         */
        BuildingBlock(int bId, BlockCodeBuilder bcb, int nbInterfaces);

        /**
         * @brief BuildingBlock destructor
         */
        virtual ~BuildingBlock();

        /**
         * @brief Getter for P2PNetworkInterfaces attribute
         * @return A vector containing pointers to the block's interfaces
         */
        const vector<P2PNetworkInterface *> &getP2PNetworkInterfaces() const {
            return P2PNetworkInterfaces;
        }

        /**
         * @brief Getter for a specific P2PNetworkInterface
         * @param i : index of interface to return
         * @return A pointer to the P2PNeighborInterface at index i of interface vector
         */
        P2PNetworkInterface *getInterface(int i) const { return P2PNetworkInterfaces[i]; }

        /**
         * @brief Find the id of a block's interface
         * @param itf : interface for which to determine id
         * @return the id of the interface, or -1 if it could not be found
         */
        [[deprecated]] short getInterfaceId(const P2PNetworkInterface *itf) const;

        uint8_t getInterfaceBId(const P2PNetworkInterface *itf) const;
        /**
         * @brief Getter for a specific P2PNetworkInterface, identified by its direction
         * For all blocks that cannot rotate, the direction will always be equal to the index in the P2PNetworkInterfaces array.
         * However, for all rotation-enabled blocks, we have to consider the angle as an offset to the index.
         * @param direction : Lattice::Direction to which the interface is pointing
         * @return A pointer to the P2PNeighborInterface at direction of the block */
        // virtual P2PNetworkInterface *getInterfaceForDirection(int direction) { return P2PNetworkInterfaces[i]; }
        /**
         * @brief Returns the interface from this block that is connected to block of id destBlockId
         * @param destBlockId : id of the block connected to the interface we are looking for
         * @return a pointer to the interface connected to the requested block, or NULL
         */
        P2PNetworkInterface *getP2PNetworkInterfaceByDestBlockId(bID destBlockId) const;

        /**
         * @brief Creates a new interface to this block and connects it to destBlock
         * @param destBlock : pointer to the building block to connect to the newly created interface
         * @return true if connection succeeded, false otherwise
         */
        bool addP2PNetworkInterfaceAndConnectTo(BuildingBlock *destBlock);

        /**
         * @brief Creates a new interface to this block and connects it to block of id destBlockId
         * @param destBlockId : Id of the building block to connect to the newly created interface
         * @return true if connection succeeded, false otherwise
         */
        bool addP2PNetworkInterfaceAndConnectTo(int destBlockId);

        /**
         * @brief Finds the block's interface that is connected to destBlock
         * @param destBlock : pointer to a connected block
         * @return a pointer to the interface connected to destBlock if there is one, NULL otherwise
         */
        P2PNetworkInterface *getP2PNetworkInterfaceByBlockRef(BuildingBlock *destBlock) const;

        /**
         * @brief Schedules a local event for this block to process when available
         * @param pev : pointer to the event to schedule
         */
        void scheduleLocalEvent(EventPtr pev);

        /**
         * @brief Processes the first event from the event queue
         */
        void processLocalEvent();

        /**
         * @brief Returns the GlBlock corresponding to this BuildingBlock
         */
        inline virtual GlBlock *getGlBlock() const { return ptrGlBlock; };

        /**
         * @brief Setter for ptrGlBlock
         * @param ptr : a ptr to a GlBlock corresponding to this block
         */
        inline void setGlBlock(GlBlock *ptr) { ptrGlBlock = ptr; };

        /**
         * @brief Sets the color for this block with the referenced color parameter
         * @param c : a reference to the new color of the block
         */
        void setColor(const Color &);

        /**
         * @brief Sets the color for this block with the color of id idColor
         * @param idColor : id of the Color as defined in color.h
         */
        void setColor(int idColor);

        /**
         * @brief Sets the grid position of the block
         *
         * @param p :  the grid position (x,y,z) of the block as a Cell3DPosition
         */
        virtual void setPosition(const Cell3DPosition &p);

        /**
         * @brief Sets the grid position and orientation of the block
         *
         * @param p :  the grid position (x,y,z) of the block as a Cell3DPosition
         * @param orient : the orientation code for the block
         */
        virtual void setPositionAndOrientation(const Cell3DPosition &p, uint8_t orient)=0;

        /**
         * @brief Returns a Vector3D corresponding to the Cell3DPosition of the current block.
         *
         * @return the position of the block represented as a double Vector3D
         */
        inline Vector3D getPositionVector() const {
            return Vector3D(position[0], position[1], position[2]);
        };

        /**
         * @brief Schedules an AddNewNeighbor event
         * @param ni : pointer to the interface that was just connected
         * @param target : pointer to the BuildingBlock connected to interface ni
         */
        virtual void addNeighbor(P2PNetworkInterface *ni, BuildingBlock *target) {};

        /**
         * @brief Schedules a RemoveNeighborEvent
         * @param ni : pointer to the disconnected interface
         */
        virtual void removeNeighbor(P2PNetworkInterface *ni) {};

        /**
         * @brief Returns the number of interfaces for this block
         * @return number of interface for this block
         */
        inline unsigned short getNbInterfaces() const { return P2PNetworkInterfaces.size(); };

        /**
         * @brief Returns the number of neighbors (connected interfaces) for this block
         * @return number of neighbor for this block
         */
        uint8_t getNbNeighbors() const;
        uint8_t neighborCount() const { return getNbNeighbors(); };

        /**
         * @return Returns a vector of pointers to all neighbor module
         */
        vector<BuildingBlock *> getNeighbors() const;

        /**
         * @brief Schedules a stop event for this block at a given date and update its state
         * @param date : date at which the stop event must be processed
         * @param s : new state of the block
         */
        virtual void stop(Time date, State s) {};

        /**
         * @brief Returns the direction (defined in lattice.h) corresponding to the interface p2p
         * @param p2p interface to consider
         * @return direction on which p2p is
         */
        virtual int getDirection(P2PNetworkInterface *p2p) const = 0;

        /**
         * @brief Returns if the neighbor is in the grid
         * @param connectorId : id of the face in the direction of the neighbors
         * @param pos : position of the neighbor in the grid
         */
        virtual bool getNeighborPos(uint8_t connectorId, Cell3DPosition &pos) const;

        /**
         * @brief Atomic getter for the block's state
         * No guarantee that state value will remain the same, it just avoids
         * date race condition.
         * @return the state of the block
         */
        inline State getState() { return state.load(); }

        /**
         * @brief Atomically sets the state of the block
         * No guarantee that state value will remain the same, it just avoids
         * date race condition.
         * @param s : new state of the block
         */
        inline void setState(State s) { state.store(s); }

        /**
         * @brief Return a random unsigned int (ruint) using the generator field
         * @return random ruint
         */
        ruint getRandomUint();

        /**
         * @brief Schedules a tap event at a given date for this blocks
         * When triggered from the simulation menu,
         *  can be used as an interactive event for debug on all catom types
         * @param date : date of the tap event
         * @param face : id of the tapped face
         */
        void tap(Time date, int face);

        /**
         * @brief Set the internal clock to the clock in parameter
         * @param c clock which the internal clock will be set
         */
        void setClock(Clock *c);

        /**
         * @brief Returns the current local time of the block according to its internal clock
         * @return current local time of the block according to its internal clock
         */
        Time getLocalTime() const;

        /**
         * @brief Returns the local time of the block according to its internal clock
         * @para simTime simulation time for which this function returns the block clock local time
         * @return local time of the block according to its internal clock
         */
        Time getLocalTime(Time simTime) const;

        /**
         * @brief Converts the block's local time into the global time of the simulation and returns it
         * @return global time corresponding to the local time in parameter
         */
        Time getSimulationTime(Time localTime) const;

        /**
         * @brief Returns the id of the block connected on interface #faceNum of this block
         * @param faceNum : id of the connected interface
         * @return id of the block connected to interface faceNum
         */
        unsigned short getNeighborIDForFace(int faceNum) const;

        /**
         * @brief Returns the id of the face from this block connected to block of id nId
         * @param nId : id of the connected block
         * @return the id of the face connected to block nId, or -1 if the two blocks are not neighbors
         */
        int getFaceForNeighborID(int nId) const;

        void setBlinkMode(bool b) { ptrGlBlock->setHighlight(b); };

        /**
         * @brief Return if there is a neighbor in direction dir of the current robot
         * @param dir : direction
         * @return true if there is a robot in dir cell.
        */
        virtual bool hasNeighbor(int dir);

        /**
         * @param dest destination of the candidate motion
         * @return true if the module can move to cell dest
         */
        virtual bool canMoveTo(const Cell3DPosition &dest) const = 0;

        /**
         * Moves module to lattice cell dest if possible
         * @param dest destination of the motion
         * @return true if motion is possible and has been scheduled, false otherwise
         */
        virtual bool moveTo(const Cell3DPosition &dest) = 0;

        /**
         * Get the list of possible motions as a vector of pairs of (position and orientation code)
         * @return the vector of possible motions
         */
        virtual vector<pair<Cell3DPosition,uint8_t>> getAllMotions() const = 0;

        /**
         * Serializes (converts to a stream of bits) relevant data from the building block object
         *  for the purpose of simulation replay
         *
         *  By default, serializes as: <id><position><orientation><color>
         *  Extra attributes can be serialized in children classes
         *
         * @param bStream output binary stream
         */
        virtual void serialize(std::ofstream &bStream);

        /**
         * Clear-text equivalent of the BuildingBlock::serialize function, for debugging purpose
         * @see BuildingBlock::serialize
         * @param dbStream output binary stream
         */
        virtual void serialize_cleartext(std::ofstream &dbStream);
    };

    std::ostream &operator<<(std::ostream &stream, BuildingBlock const &bb);

} // BaseSimulator namespace

#endif /* BUILDINGBLOCK_H_ */
