
#include "catoms3DMotionEngine.h"

#include "utils.h"
#include <vector>

const Catoms3DMotionRulesLink*
Catoms3DMotionEngine::findConnectorLink(Catoms3DBlock *module,
                                        short conFrom, short conTo,
                                        RotationLinkType ft) {
    vector<Catoms3DMotionRulesLink*> motionRulesLinksFrom;
    getMotionRules()->getValidMotionList(module, conFrom, motionRulesLinksFrom);
    
    for (const Catoms3DMotionRulesLink* link : motionRulesLinksFrom) {
        if (link->getConToID() == conTo) {            
            switch (ft) {
                case Any: return link;
                case HexaFace: if (link->isOctaFace() == HexaFace) return link; break;
                case OctaFace: if (link->isOctaFace() == OctaFace) return link; break;
                    // NOTE: Maybe throw exception instead?
                default: VS_ASSERT_MSG(false, "invalid rotation link type");
            }
        }
    }
    
    return NULL;
}

const Catoms3DMotionRulesLink*
Catoms3DMotionEngine::findPivotConnectorLink(Catoms3DBlock *pivot,
                                             short conFrom, short conTo,
                                             RotationLinkType ft) {
    vector<Catoms3DMotionRulesLink*> motionRulesLinks;
    getMotionRules()->getValidSurfaceLinksOnCatom(pivot, motionRulesLinks);
    
    for (Catoms3DMotionRulesLink* link : motionRulesLinks) {
        if (link->getConFromID() == conFrom and link->getConToID() == conTo) {
            // TODO:
            // const Cell3DPosition& fPos = link->getFinalPosition(pivot);
            // if (static_cast<FCCLattice*>(getWorld()->lattice)->
            //     isPositionBlocked(fPos, pivot->position)) return NULL;
            // END TODO
            
            switch (ft) {
                case Any: return link;
                case HexaFace: if (link->isOctaFace() == HexaFace) return link; break;
                case OctaFace: if (link->isOctaFace() == OctaFace) return link; break;
                    // NOTE: Maybe throw exception instead?
                default: VS_ASSERT_MSG(false, "invalid rotation link type");
            }
        }
    }
    
    return NULL;
}

short Catoms3DMotionEngine::getMirrorConnectorOnModule(Catoms3DBlock *m1, Catoms3DBlock *m2,
                                                       short dockingConM1, short dockingConM2,
                                                       short mirroringCon) {
    bool inverted = m1->areOrientationsInverted(m2->orientationCode);
    short mirroringConDir = getMotionRules()->getConnectorDirection(dockingConM1, mirroringCon);

    short mirrorCon = mirroringConDir >= 0 ? getMotionRules()->
        getMirrorNeighborConnector(dockingConM2, (ConnectorDirection)mirroringConDir, inverted)
        : -1;

    // cerr << "#" << m1->blockId << "(" << mirroringCon << ") -> #" << m2->blockId
    //      << "(" << mirrorCon << ") --- dir: "
    //      << mirroringConDir << " / inv: " << inverted << endl;

    return mirrorCon;
}

Catoms3DBlock* Catoms3DMotionEngine::findMotionPivot(Catoms3DBlock* m,
                                                     const Cell3DPosition& tPos) {
    if (m == NULL) return NULL;
    
    // TODO:
    // A pivot module is necessarily one which is both adjacent to m and the target position
    /// (1) Get all occupied positions that qualify
    Catoms3DWorld* world = Catoms3D::getWorld();
    Lattice* lattice = world->lattice;
    
    vector<Cell3DPosition> mActiveCells = lattice->getActiveNeighborCells(m->position);
    vector<Cell3DPosition> tPosActiveCells = lattice->getActiveNeighborCells(tPos);

    const vector<Cell3DPosition>& adjacentCells =
        utils::intersection(mActiveCells, tPosActiveCells);

    // Check for a pivot with a direct connector path between the two cells
    Catoms3DBlock* pivot = NULL;
    for (const Cell3DPosition& pPos : adjacentCells) {
        pivot = static_cast<Catoms3DBlock*>(lattice->getBlock(pPos));

        // Determine pivot connectors and check if a possible path exists 
        short conFrom = pivot->getConnectorId(m->position);
        short conTo = pivot->getConnectorId(tPos);
        const Catoms3DMotionRulesLink *link = findPivotConnectorLink(pivot, conFrom, conTo,
                                                                     RotationLinkType::Any);

        // Return first available pivot
        if (link) return pivot;
    }

    return NULL;
}
