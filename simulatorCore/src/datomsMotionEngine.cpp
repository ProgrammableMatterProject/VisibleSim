
#include "datomsMotionEngine.h"

#include "utils.h"
#include <vector>

const DatomsMotionRulesLink*
DatomsMotionEngine::findConnectorLink(const DatomsBlock *module,
                                        short conFrom, short conTo,
                                        DeformationLinkType  ft) {
    vector<DatomsMotionRulesLink*> motionRulesLinksFrom;
    getMotionRules()->getValidMotionList(module, conFrom, motionRulesLinksFrom);
    
    for (const DatomsMotionRulesLink* link : motionRulesLinksFrom) {
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

const DatomsMotionRulesLink*
DatomsMotionEngine::findPivotConnectorLink(const DatomsBlock *pivot,
                                             short conFrom, short conTo,
                                             DeformationLinkType  ft) {
    vector<DatomsMotionRulesLink*> motionRulesLinks;
    getMotionRules()->getValidSurfaceLinksOnDatom(pivot, motionRulesLinks);
    
    for (DatomsMotionRulesLink* link : motionRulesLinks) {
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

short DatomsMotionEngine::getMirrorConnectorOnModule(const DatomsBlock *m1,
                                                       const DatomsBlock *m2,
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

std::vector<std::pair<DatomsBlock*, const DatomsMotionRulesLink*>>
DatomsMotionEngine::findPivotLinkPairsForTargetCell(const DatomsBlock* m,
                                                      const Cell3DPosition& tPos,
                                                      DeformationLinkType  faceReq) {
    std::vector<std::pair<DatomsBlock*, const DatomsMotionRulesLink*>> allLinkPairs;
    
    if (m) {    
        // A pivot module is necessarily one which is both adjacent to m and the target position
        /// (1) Get all occupied positions that qualify
        DatomsWorld* world = Datoms::getWorld();
        Lattice* lattice = world->lattice;
    
        vector<Cell3DPosition> mActiveCells = lattice->getActiveNeighborCells(m->position);
        vector<Cell3DPosition> tPosActiveCells = lattice->getActiveNeighborCells(tPos);

        const vector<Cell3DPosition>& adjacentCells =
            utils::intersection(mActiveCells, tPosActiveCells);

        // Check for a pivot with a direct connector path between the two cells
        DatomsBlock* pivot = NULL;
        for (const Cell3DPosition& pPos : adjacentCells) {
            pivot = static_cast<DatomsBlock*>(lattice->getBlock(pPos));

            // Determine pivot connectors and check if a possible path exists 
            short conFrom = pivot->getConnectorId(m->position);
            short conTo = pivot->getConnectorId(tPos);

            if (faceReq == DeformationLinkType ::HexaFace or faceReq == DeformationLinkType ::Any) {
                const DatomsMotionRulesLink *link = findPivotConnectorLink(pivot, conFrom,
                                                                             conTo, HexaFace);

                // Ensure that no-only a path is available on the pivot,
                //  but also that this module can actually use that path
                const DatomsMotionRulesLink *matchingModuleLink =
                    getMotionRules()->getMobileModuleLinkMatchingPivotLink(link, m, pivot);
            
                // Mark pivot
                if (link and matchingModuleLink)
                    allLinkPairs.push_back(std::make_pair(pivot, matchingModuleLink));
            }

            if (faceReq == DeformationLinkType ::OctaFace or faceReq == DeformationLinkType ::Any) {
                const DatomsMotionRulesLink *link = findPivotConnectorLink(pivot, conFrom,
                                                                             conTo, OctaFace);

                // Ensure that no-only a path is available on the pivot,
                //  but also that this module can actually use that path
                const DatomsMotionRulesLink *matchingModuleLink =
                    getMotionRules()->getMobileModuleLinkMatchingPivotLink(link, m, pivot);
            
                // Mark pivot
                if (link and matchingModuleLink)
                    allLinkPairs.push_back(std::make_pair(pivot, matchingModuleLink));
            }
        }
    }
    
    return allLinkPairs;
}


DatomsBlock* DatomsMotionEngine::findMotionPivot(const DatomsBlock* m,
                                                     const Cell3DPosition& tPos,
                                                     DeformationLinkType  faceReq) {
    const auto &allLinkPairs = findPivotLinkPairsForTargetCell(m, tPos, faceReq);

    for (const auto& pair : allLinkPairs) {
        cout << "{ " << *pair.first << ", " << *pair.second << " }" << endl;
        if (pair.second->getMRLT() == faceReq or faceReq == DeformationLinkType ::Any)
            return pair.first;
    }
    
    return NULL;
}

const vector<std::pair<const DatomsMotionRulesLink*, Deformation>>
DatomsMotionEngine::getAllDeformationsForModule(const DatomsBlock* m) {
    vector<std::pair<const DatomsMotionRulesLink*, Deformation>> allDeformations;

    if (m) {
        for (const Cell3DPosition& nPos : World::getWorld()->lattice->
                 getFreeNeighborCells(m->position)) {
            const vector<std::pair<DatomsBlock*, const DatomsMotionRulesLink*>>
                pivotLinkPairs = findPivotLinkPairsForTargetCell(m, nPos);
            
            for (const auto& pair : pivotLinkPairs) {
                if (pair.first and pair.second) {
                    Deformation r = pair.second->getDeformations(m, pair.first);
                    allDeformations.push_back(make_pair(pair.second, r));
                }
            }
        }
    }
    
    return allDeformations;
}
