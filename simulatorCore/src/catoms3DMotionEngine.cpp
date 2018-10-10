
#include "catoms3DMotionEngine.h"

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

    return mirroringConDir >= 0 ? getMotionRules()->
        getMirrorNeighborConnector(dockingConM2, (ConnectorDirection)mirroringConDir, inverted)
        : -1;
}
