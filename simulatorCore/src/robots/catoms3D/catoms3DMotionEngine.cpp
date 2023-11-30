#include <vector>
#include "../../utils/utils.h"
#include "catoms3DMotionEngine.h"


const Catoms3DMotionRulesLink *
Catoms3DMotionEngine::findConnectorLink(const Catoms3DBlock *module,
                                        short conFrom, short conTo,
                                        RotationLinkType ft) {
    vector<Catoms3DMotionRulesLink *> motionRulesLinksFrom;
    getMotionRules()->getValidMotionList(module, conFrom, motionRulesLinksFrom);

    for (const Catoms3DMotionRulesLink *link: motionRulesLinksFrom) {
        if (link->getConToID() == conTo) {
            switch (ft) {
                case Any:
                    return link;
                case HexaFace:
                    if (link->isOctaFace() == HexaFace) return link;
                    break;
                case OctaFace:
                    if (link->isOctaFace() == OctaFace) return link;
                    break;
                    // NOTE: Maybe throw exception instead?
                default:
                    VS_ASSERT_MSG(false, "invalid rotation link type");
            }
        }
    }

    return nullptr;
}

const Catoms3DMotionRulesLink *
Catoms3DMotionEngine::findPivotConnectorLink(const Catoms3DBlock *pivot,
                                             short conFrom, short conTo,
                                             RotationLinkType ft) {
    vector<Catoms3DMotionRulesLink *> motionRulesLinks;
    getMotionRules()->getValidSurfaceLinksOnCatom(pivot, motionRulesLinks);

    for (Catoms3DMotionRulesLink *link: motionRulesLinks) {
        if (link->getConFromID() == conFrom and link->getConToID() == conTo) {
            // TODO:
            // const Cell3DPosition& fPos = link->getFinalPosition(pivot);
            // if (static_cast<FCCLattice*>(getWorld()->lattice)->
            //     isPositionBlocked(fPos, pivot->position)) return nullptr;
            // END TODO

            switch (ft) {
                case Any:
                    return link;
                case HexaFace:
                    if (link->isOctaFace() == HexaFace) return link;
                    break;
                case OctaFace:
                    if (link->isOctaFace() == OctaFace) return link;
                    break;
                    // NOTE: Maybe throw exception instead?
                default:
                    VS_ASSERT_MSG(false, "invalid rotation link type");
            }
        }
    }

    return nullptr;
}

short Catoms3DMotionEngine::getMirrorConnectorOnModule(const Catoms3DBlock *m1,
                                                       const Catoms3DBlock *m2,
                                                       short dockingConM1, short dockingConM2,
                                                       short mirroringCon) {
    bool inverted = m1->areOrientationsInverted(m2->orientationCode);
    short mirroringConDir = getMotionRules()->getConnectorDirection(dockingConM1, mirroringCon);

    short mirrorCon = mirroringConDir >= 0 ? getMotionRules()->
            getMirrorNeighborConnector(dockingConM2, (ConnectorDirection) mirroringConDir, inverted)
                                           : -1;

    // cerr << "#" << m1->blockId << "(" << mirroringCon << ") -> #" << m2->blockId
    //      << "(" << mirrorCon << ") --- dir: "
    //      << mirroringConDir << " / inv: " << inverted << endl;

    return mirrorCon;
}

std::vector<std::pair<Catoms3DBlock *, const Catoms3DMotionRulesLink *>>
Catoms3DMotionEngine::findPivotLinkPairsForTargetCell(const Catoms3DBlock *m,
                                                      const Cell3DPosition &tPos,
                                                      RotationLinkType faceReq) {
    std::vector<std::pair<Catoms3DBlock *, const Catoms3DMotionRulesLink *>> allLinkPairs;

    if (m) {
        // A pivot module is necessarily one which is both adjacent to m and the target position
        /// (1) Get all occupied positions that qualify
        Catoms3DWorld *world = Catoms3D::getWorld();
        Lattice *lattice = world->lattice;

        // the pivot place is a present neighbor of m and the final position
        vector<Cell3DPosition> mActiveCells = lattice->getActiveNeighborCells(m->position);
        vector<Cell3DPosition> tPosActiveCells = lattice->getActiveNeighborCells(tPos);

        const vector<Cell3DPosition> &adjacentCells =
                utils::intersection(mActiveCells, tPosActiveCells);

                // Check for a pivot with a direct connector path between the two cells
        Catoms3DBlock *pivot = nullptr;
        for (const Cell3DPosition &pPos: adjacentCells) {
            pivot = static_cast<Catoms3DBlock *>(lattice->getBlock(pPos));

            // Do no allow rotating modules to actuate for others
            if (pivot->getState() == BuildingBlock::State::MOVING) {
                cout << pPos << " is rotating" << endl;
                continue;
            }

            // Determine pivot connectors and check if a possible path exists
            short conFrom = pivot->getConnectorId(m->position);
            short conTo = pivot->getConnectorId(tPos);
            /*cout << "connectors:" << conFrom << "->" << conTo << endl;*/

            if (faceReq == RotationLinkType::HexaFace or faceReq == RotationLinkType::Any) {
                const Catoms3DMotionRulesLink *link = findPivotConnectorLink(pivot, conFrom,
                                                                             conTo, HexaFace);

                // Ensure that no-only a path is available on the pivot,
                //  but also that this module can actually use that path
                const Catoms3DMotionRulesLink *matchingModuleLink =
                        getMotionRules()->getMobileModuleLinkMatchingPivotLink(link, m, pivot);

                // Mark pivot
                if (link and matchingModuleLink)
                    allLinkPairs.emplace_back(pivot, matchingModuleLink);
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
                    allLinkPairs.emplace_back(pivot, matchingModuleLink);
            }
        }
    }

    return allLinkPairs;
}


Catoms3DBlock *Catoms3DMotionEngine::findMotionPivot(const Catoms3DBlock *m,
                                                     const Cell3DPosition &tPos,
                                                     RotationLinkType faceReq) {
    const auto &allLinkPairs = findPivotLinkPairsForTargetCell(m, tPos, faceReq);

    // cout << "size: " << allLinkPairs.size() << endl;

    for (const auto &pair: allLinkPairs) {
        // cout << "{ " << *pair.first << ", " << *pair.second << " }" << endl;
        if (pair.second->getMRLT() == faceReq or faceReq == RotationLinkType::Any)
            return pair.first;
    }

    return nullptr;
}

bool Catoms3DMotionEngine::isNotLockedForMotion(const Cell3DPosition &origin, const Cell3DPosition &final, bool isHexaFace) {
    Lattice *lattice = Catoms3DWorld::getWorld()->lattice;
    if (!lattice->isFree(final)) return false;
    auto conn = lattice->getRelativeConnectivity(final);
    Cell3DPosition pos0, pos1;
    // Check if the destination cell is in grid and not locked by two opposite modules
    if (!isHexaFace) {
        for (int i = 0; i < 6; i++) {
            pos0 = final + conn[i];
            pos1 = final + conn[i + 6];
            if (pos0 != origin && pos1 != origin && lattice->isInGrid(pos0) && !lattice->isFree(pos0) &&
                lattice->isInGrid(pos1) && !lattice->isFree(pos1)) return false;
        }
    }
    return true;
}

const vector<std::pair<const Catoms3DMotionRulesLink *, Catoms3DRotation>>
Catoms3DMotionEngine::getAllRotationsForModule(const Catoms3DBlock *m) {
    vector<std::pair<const Catoms3DMotionRulesLink *, Catoms3DRotation>> allRotations;

    if (m) {
        Cell3DPosition finalPos;
        short finalOrient;
        auto freeCells = World::getWorld()->lattice->getFreeNeighborCells(m->position);
        cout << "#freeCells=" << freeCells.size() << endl;
        for (const Cell3DPosition &nPos: freeCells) {
            const vector<std::pair<Catoms3DBlock *, const Catoms3DMotionRulesLink *>>
                    pivotLinkPairs = findPivotLinkPairsForTargetCell(m, nPos);
            cout << "#pivotLinkPairs=" << pivotLinkPairs.size() << endl;
            for (const auto &pair: pivotLinkPairs) {
                cout << "pair" << pair.first->position << endl;
                if (pair.first and pair.second) {
                    Catoms3DRotation r = pair.second->getRotations(m, pair.first);
                    // filter final position that are blocked by far away obstacle
                    r.init(((Catoms3DGlBlock *) m->ptrGlBlock)->mat);
                    r.getFinalPositionAndOrientation(finalPos, finalOrient);
                    if (isNotLockedForMotion(m->position, finalPos, pair.second->isHexaFace())) {
                        allRotations.emplace_back(pair.second, r);
                    }
                }
            }
        }
    }

    return allRotations;
}

bool Catoms3DMotionEngine::canMoveTo(const Catoms3DBlock *m, const Cell3DPosition &tPos,
                                     RotationLinkType faceReq) {
    return findMotionPivot(m, tPos, faceReq) != nullptr;
}

const vector<Cell3DPosition>
Catoms3DMotionEngine::getAllReachablePositions(const Catoms3DBlock *m,
                                               RotationLinkType faceReq) {
    vector<Cell3DPosition> reachablePositions;
    for (const Cell3DPosition &rPos: relReachablePosition) {
        const Cell3DPosition &pos = rPos + m->position;
        if (canMoveTo(m, pos)) {
            reachablePositions.push_back(pos);
        }
    }
    return reachablePositions;
}
