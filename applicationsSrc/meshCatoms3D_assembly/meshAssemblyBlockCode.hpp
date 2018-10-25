/**
 * @file   meshCatoms3DBlockCode_mvmt.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 13:47:48 2018
 * 
 * @brief  
 * 
 * 
 */


#ifndef MESHCATOMS3DBLOCKCODE_H_
#define MESHCATOMS3DBLOCKCODE_H_

#include "catoms3DBlockCode.h"
#include "catoms3DSimulator.h"
#include "catoms3DMotionRules.h"
#include "rotation3DEvents.h"
#include "catoms3DBlock.h"
#include "cell3DPosition.h"

#include "meshAssemblyMessages.hpp"
#include "meshRuleMatcher.hpp"

#include "meshAssemblyLocalRules.hpp"

#define IT_MODE_TILEROOT_ACTIVATION 1 

// #define INTERACTIVE_MODE

// enum EntryPointLocation { RevZ_EPL, RevZ_R_EPL, RZ_L_EPL, RZ_EPL,
//                           RZ_R_EPL, Z_R_EPL, Z_EPL,
//                           Z_L_EPL, LZ_R_EPL, LZ_EPL,
//                           LZ_LEFT_EPL, RevZ_L_EPL,
//                           N_EPL };

class MeshAssemblyBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
    static constexpr std::array<Cell3DPosition, 12> entryPointRelativePos =
    {
        Cell3DPosition(-1,-1,-1), // RevZ_EPL
        Cell3DPosition(0,-1,-1), // RevZ_R_EPL
        Cell3DPosition(1,-1,-1), // RZ_L_EPL
        Cell3DPosition(2,-1,-1), // RZ_EPL

        Cell3DPosition(2,0,-1), // RZ_R_EPL
        Cell3DPosition(2,1,-1), // Z_R_EPL
        Cell3DPosition(2,2,-1), // Z_EPL

        Cell3DPosition(1,2,-1), // Z_L_EPL
        Cell3DPosition(0,2,-1), // LZ_R_EPL
        Cell3DPosition(-1,2,-1), // LZ_EPL

        Cell3DPosition(-1,1,-1), // LZ_L_EPL
        Cell3DPosition(-1,0,-1) // RevZ_L_EPL
    };
public:
    static const uint B = 6;
    static uint X_MAX, Y_MAX, Z_MAX; // const
    static constexpr Cell3DPosition meshSeedPosition = Cell3DPosition(3,3,3);    

    static constexpr std::array<Cell3DPosition, 6> incidentTipRelativePos =
    {
        Cell3DPosition(0,0,-1), // ZBranch
        Cell3DPosition(1,1,-1), // RevZBranch
        Cell3DPosition(1,0,-1), // LZBranch
        Cell3DPosition(0,1,-1), // RZBranch
        Cell3DPosition(-1,0,0), // XBranch
        Cell3DPosition(0,-1,0) // YBranch
    };
               
    inline static Time getRoundDuration() {
        return (2.2 * Rotations3D::ANIMATION_DELAY * Rotations3D::rotationDelayMultiplier
                + Rotations3D::COM_DELAY);// + (getScheduler()->now() / 1000);
    }

    inline const Cell3DPosition& getEntryPointRelativePos(MeshComponent mc) const {
        VS_ASSERT_MSG(mc >= RevZ_EPL, "Attempting to getEntryPointRelativePos for non EPL component");
        return entryPointRelativePos[mc - RevZ_EPL];
    }
    
    inline const Cell3DPosition getTileRelativePosition() const {
        return catom->position - coordinatorPos;
    }
    
    int debugColorIndex = 0;
    
    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;
    MeshCoating::MeshRuleMatcher *ruleMatcher;

    BranchIndex branch;
    AgentRole role;
    Cell3DPosition coordinatorPos;
    Cell3DPosition targetPosition;

    // Free Agent Vars
    short step = 1; // For moving FreeAgents

    // Support Agent Vars
    Cell3DPosition branchTipPos; // For Support role only    
    
    // Coordinator Vars
    int itCounter = 0; // When t > 5 all supports are in place
    bool fedCatomsOnLastRound = false;
    std::array<int, 6> catomsReqByBranch = {-1,-1,-1,-1,-1,-1}; // We could have -1 if branch should not be grown
    // std::array<bool, 6> fedCatomOnLastRound = { false, false, false, false, false, false };
    std::array<Cell3DPosition*, 6> openPositions = {0};
    std::array<Cell3DPosition, 12> targetForEntryPoint; //<! for a coordinator, the target cells to which each of the modules that it has called in should move to once they are initialized
    std::array<bool, 6> feedBranch = {0};
   
    // TargetCSG *target;
    MeshAssemblyBlockCode(Catoms3D::Catoms3DBlock *host);
    ~MeshAssemblyBlockCode();          
    
    /**
     * \brief Global message handler for this instance of the blockcode
     * \param msg Message received b
y the module
     * \param sender Connector that has received the message and hence that is connected to the sender */
    void processReceivedMessage(MessagePtr msg, P2PNetworkInterface* sender);
    
    void startup();
    void processLocalEvent(EventPtr pev);
    void onBlockSelected();

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new MeshAssemblyBlockCode((Catoms3DBlock*)host));
    }

    /** 
     * Transforms a shifted grid position into a mesh absolute position.
     * @note This has to be used due to the mesh seed being offsetted in order to leave space 
     *  for spawning modules 
     * @param pos position to normalize
     * @return the corresponding position of pos in the coordinate system of the mesh
     */
    static const Cell3DPosition norm(const Cell3DPosition& pos);

    /** 
     * Inverse function of norm
     * @note This has to be used due to the mesh seed being offsetted in order to leave space 
     *  for spawning modules 
     * @param pos position to denormalize
     * @return the corresponding position of pos in the absolute coordinate system 
     */
    static const Cell3DPosition denorm(const Cell3DPosition& pos);

    /** 
     * Transforms an absolute position into a tile-relative position
     * @param pos position to relatify
     * @return the corresponding position of pos in the tile-relative coordinate system 
     */
    const Cell3DPosition relatify(const Cell3DPosition& pos);

    /** 
     * Inverse operation of relatify
     */
    const Cell3DPosition derelatify(const Cell3DPosition& pos);
    
    void updateOpenPositions();
        
    /** 
     * Computes the entry point direction identifier corresponding to neighbor cell pos
     * @param pos a cell that is adjacent to the current module 
     * @attention pos is not a relative position but an absolute one!
     * @return entry point direction id corresponding to pos or -1 if pos not adjacent to module or if cell is not on a module's lower connectors
     */
    short getEntryPointLocationForCell(const Cell3DPosition& pos);

    /** 
     * Adds a new mesh component to the grid at its predetermined entry point, 
     *  and prepare to send it its coordinates
     * @param mc mesh component of catom to add
     */
    void handleMeshComponentInsertion(MeshComponent mc);

    /** 
     * Finds an entry point index for a catom required to fill component mc
     * @param mc mesh component type of the catom
     * @return entry point position / spawn location for component mc
     */
    const Cell3DPosition getEntryPointForMeshComponent(MeshComponent mc);

    /** 
     * Checks if all the incident branches around a tile root are complete
     * @param pos position of the tile root
     * @return true if all incident branches are complete, false otherwise
     */
    bool incidentBranchesToRootAreComplete(const Cell3DPosition& pos);

    /** 
     * Checks if branch tip from branch bi is in place around tile root at trp
     * @param trp tile root position
     * @param bi index of the incident branch that needs checking
     * @return true if the tip of branch bi is in place next ot trp
     */
    bool isIncidentBranchTipInPlace(const Cell3DPosition& trp, BranchIndex bi);

    // TODO:
    void scheduleRotationTo(const Cell3DPosition& pos);

    // TODO:
    void initializeTileRoot();
    void initializeSupportModule();

    /** 
     * @param epl Entry point location name of the desired entry point
     * @return absolute position of the requested entry point location
     */
    const Cell3DPosition getEntryPointPosition(MeshComponent epl) const;

    /** 
     * Indicates whether position pos is an entry point location
     * @param pos position to evaluate
     * @return true if pos is an EPL, false otherwise
     */
    bool isOnEntryPoint(const Cell3DPosition& pos) const;    
};

#endif /* MESHCATOMS3DBLOCKCODE_H_ */
    
