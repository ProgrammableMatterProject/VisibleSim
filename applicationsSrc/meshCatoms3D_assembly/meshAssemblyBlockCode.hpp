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

#include "messages.hpp"
#include "meshRuleMatcher.hpp"

#define IT_MODE_TILEROOT_ACTIVATION 1 

#define INTERACTIVE_MODE

enum AgentRole { FreeAgent, Coordinator, PassiveBeam };
enum EntryPointDirection { Dir8, Dir9, Dir10, Dir11, NUM_EPD };

class MeshAssemblyBlockCode : public Catoms3D::Catoms3DBlockCode {
private:
public:
    static const uint B = 6;
    static uint X_MAX, Y_MAX, Z_MAX; // const
    static constexpr Cell3DPosition meshSeedPosition = Cell3DPosition(1,1,1);
    static constexpr std::array<Cell3DPosition, 6> incidentTipRelativePos =
        {
         Cell3DPosition(0,0,-1), // ZBranch
         Cell3DPosition(1,1,-1), // RevZBranch
         Cell3DPosition(0,1,-1), // LeftZBranch
         Cell3DPosition(1,0,-1), // RightZBranch
         Cell3DPosition(-1,0,0), // XBranch
         Cell3DPosition(0,-1,0) // YBranch
        };
    
    int debugColorIndex = 0;
    
    Scheduler *scheduler;
    World *world;
    Lattice *lattice;
    Catoms3D::Catoms3DBlock *catom;
    MeshCoating::MeshRuleMatcher *ruleMatcher;

    AgentRole role;
    Cell3DPosition coordinatorPos;
    Cell3DPosition targetPosition;
    std::array<int, 6> catomsReqByBranch = {-1,-1,-1,-1,-1,-1}; // We could have -1 if branch should not be grown
    std::array<bool, 6> fedCatomOnLastRound = { false, false, false, false, false, false };
    std::array<Cell3DPosition*, 6> openPositions = {NULL, NULL, NULL, NULL, NULL, NULL};
    std::array<Cell3DPosition, 4> targetForEntryPoint; //<! for a coordinator, the target cells to which each of the modules that it has called in should move to once they are initialized
    bool hasToGrowFourDiagBranches = false;
    
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
    void updateOpenPositions();
        
    /** 
     * Computes the entry point direction identifier corresponding to neighbor cell pos
     * @param pos a cell that is adjacent to the current module 
     * @attention pos is not a relative position but an absolute one!
     * @return entry point direction id corresponding to pos or -1 if pos not adjacent to module or if cell is not on a module's lower connectors
     */
    short getEntryPointDirectionForCell(const Cell3DPosition& pos);

    /** 
     * Checks if a new catom should be inserted for branch bi, proceed if necessary, and update internal state
     * @param bi index of branch to be handled
     * @return true if a catom has been inserted for that branch, false otherwise.
     */
    bool handleNewCatomInsertion(BranchIndex bi);

    /** 
     * Finds an entry point for a catom required by branch whose index is bi
     * @param bi index of branch for which to get an entry point
     * @return position of chosen entry point (relative to the root calling this function)
     */
    Cell3DPosition getEntryPointForBranch(BranchIndex bi);

    /** 
     * Finds an entry point index for a catom required by branch whose index is bi
     * @param bi index of branch for which to get an entry point index
     * @return index of chosen entry point
     */
    short getEntryPointDirectionForBranch(BranchIndex bi);

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

};

#endif /* MESHCATOMS3DBLOCKCODE_H_ */
    
