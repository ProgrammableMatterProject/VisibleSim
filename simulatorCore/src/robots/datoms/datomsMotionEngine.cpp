#include <vector>
#include "../../utils/utils.h"
#include "datomsMotionEngine.h"


const DatomsMotionRulesLink* DatomsMotionEngine::findConnectorLink(const DatomsBlock *module,
                                        short conFrom, short conTo) {
    vector<DatomsMotionRulesLink*> motionRulesLinksFrom;
    getMotionRules()->getValidMotionList(module, conFrom, motionRulesLinksFrom);

    for (const DatomsMotionRulesLink* link : motionRulesLinksFrom) {
        if (link->getConToID() == conTo) return link;
    }

    return NULL;
}

const vector<std::pair<const DatomsMotionRulesLink*, Deformation>>
DatomsMotionEngine::getAllDeformationsForModule(const DatomsBlock* m) {
    vector<std::pair<const DatomsMotionRulesLink*, Deformation>> allDeformations;

    for (short i=0; i<12; i++) {
        P2PNetworkInterface *p2p = m->getInterface(i);
        if (p2p!=nullptr && p2p->connectedInterface!=nullptr) { // if connected
            DatomsBlock *pivot=(DatomsBlock *)m->getInterface(i)->connectedInterface->hostBlock;
            vector<DatomsMotionRulesLink*>v;
            short j = pivot->getInterfaceId(m->getInterface(i)->connectedInterface);

//            OUTPUT << "Interface #" << m->blockId << ":" << i << " / #" << pivot->blockId << ":" << j << endl;

            if (getMotionRules()->getValidMotionList(pivot,j,v)) { // list of theoretical motion
                for (DatomsMotionRulesLink* validRule : v) {
                    vector<pair<DatomsBlock*,PistonId>> blockingDatoms = validRule->getBlockingDatoms(pivot);

                    Deformation d = validRule->getDeformations(m,pivot,blockingDatoms);
                    allDeformations.push_back(make_pair(validRule, d));
//                    OUTPUT << validRule->getConFromID() << "=>" << validRule->getConToID() << "-------------------------" << endl;
                }
            }
        }
    }
    return allDeformations;
}
