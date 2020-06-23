#include <vector>
#include "../../utils/utils.h"
#include "catoms3DMotionEngine.h"


const Catoms3DMotionRulesLink*
Catoms3DMotionEngine::findConnectorLink(const Catoms3DBlock *module,
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
Catoms3DMotionEngine::findPivotConnectorLink(const Catoms3DBlock *pivot,
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

short Catoms3DMotionEngine::getMirrorConnectorOnModule(const Catoms3DBlock *m1,
                                                       const Catoms3DBlock *m2,
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

std::vector<std::pair<Catoms3DBlock*, const Catoms3DMotionRulesLink*>>
Catoms3DMotionEngine::findPivotLinkPairsForTargetCell(const Catoms3DBlock* m,
                                                      const Cell3DPosition& tPos,
                                                      RotationLinkType faceReq) {
    std::vector<std::pair<Catoms3DBlock*, const Catoms3DMotionRulesLink*>> allLinkPairs;

    if (m) {
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

            // Do no allow rotating modules to actuate for others
            if (pivot->getState() == BuildingBlock::State::MOVING) {
                // cout << pPos << " is rotating" << endl;
                continue;
            }

            // Determine pivot connectors and check if a possible path exists
            short conFrom = pivot->getConnectorId(m->position);
            short conTo = pivot->getConnectorId(tPos);

            if (faceReq == RotationLinkType::HexaFace or faceReq == RotationLinkType::Any) {
                const Catoms3DMotionRulesLink *link = findPivotConnectorLink(pivot, conFrom,
                                                                             conTo, HexaFace);

                // Ensure that no-only a path is available on the pivot,
                //  but also that this module can actually use that path
                const Catoms3DMotionRulesLink *matchingModuleLink =
                    getMotionRules()->getMobileModuleLinkMatchingPivotLink(link, m, pivot);

                // Mark pivot
                if (link and matchingModuleLink)
                    allLinkPairs.push_back(std::make_pair(pivot, matchingModuleLink));
            }

            if (faceReq == RotationLinkType::OctaFace or faceReq == RotationLinkType::Any) {
                const Catoms3DMotionRulesLink *link = findPivotConnectorLink(pivot, conFrom,
                                                                             conTo, OctaFace);

                // Ensure that no-only a path is available on the pivot,
                //  but also that this module can actually use that path
                const Catoms3DMotionRulesLink *matchingModuleLink =
                    getMotionRules()->getMobileModuleLinkMatchingPivotLink(link, m, pivot);

                // Mark pivot
                if (link and matchingModuleLink)
                    allLinkPairs.push_back(std::make_pair(pivot, matchingModuleLink));
            }
        }
    }

    return allLinkPairs;
}


Catoms3DBlock* Catoms3DMotionEngine::findMotionPivot(const Catoms3DBlock* m,
                                                     const Cell3DPosition& tPos,
                                                     RotationLinkType faceReq) {
    const auto &allLinkPairs = findPivotLinkPairsForTargetCell(m, tPos, faceReq);

    // cout << "size: " << allLinkPairs.size() << endl;

    for (const auto& pair : allLinkPairs) {
        // cout << "{ " << *pair.first << ", " << *pair.second << " }" << endl;
        if (pair.second->getMRLT() == faceReq or faceReq == RotationLinkType::Any)
            return pair.first;
    }

    return NULL;
}

const vector<std::pair<const Catoms3DMotionRulesLink*, Catoms3DRotation>>
Catoms3DMotionEngine::getAllRotationsForModule(const Catoms3DBlock* m) {
    vector<std::pair<const Catoms3DMotionRulesLink*, Catoms3DRotation>> allRotations;

    if (m) {
        for (const Cell3DPosition& nPos : World::getWorld()->lattice->
                 getFreeNeighborCells(m->position)) {
            const vector<std::pair<Catoms3DBlock*, const Catoms3DMotionRulesLink*>>
                pivotLinkPairs = findPivotLinkPairsForTargetCell(m, nPos);

            for (const auto& pair : pivotLinkPairs) {
                if (pair.first and pair.second) {
                    Catoms3DRotation r = pair.second->getRotations(m, pair.first);
                    allRotations.push_back(make_pair(pair.second, r));
                }
            }
        }
    }

    return allRotations;
}

bool Catoms3DMotionEngine::canMoveTo(const Catoms3DBlock* m, const Cell3DPosition& tPos,
                                     RotationLinkType faceReq) {
    return findMotionPivot(m, tPos, faceReq) != NULL;
}

const vector<Cell3DPosition>
Catoms3DMotionEngine::getAllReachablePositions(const Catoms3DBlock* m,
                                               RotationLinkType faceReq) {
    vector<Cell3DPosition> reachablePositions;
    for (const Cell3DPosition& rPos : relReachablePosition) {
        const Cell3DPosition& pos = rPos + m->position;
        if (canMoveTo(m, pos)) {
            reachablePositions.push_back(pos);
        }
    }
    return reachablePositions;
}
