#include <climits>
#include <algorithm>
#include "catomsCombinationCode.h"
#include <fstream>
#include <iostream>
#include <stack>

vector<Node> configurations;
const int computationStages=12;

bool CatomsCombinationCode::generateSolutions() {
// search a first goal configuration
    auto it = configurations.begin();
    while (it!=configurations.end() && !(*it).reachedTarget(*target)) {
        it++;
    }
    if (it!=configurations.end()) {
        vector<Node> list;
        list.push_back(*it);
        int index=(*it).parents[0];
        while (index!=0) {
            list.push_back(configurations[index]);
            index = configurations[index].parents[0];
        }
        list.push_back(configurations[0]);

        ofstream fout("result.txt");
        for (auto &l:list) {
            fout << l;
        }

        // {[0] [id] [pos] [orient] [color]}

        fout << "-------------------\n";
        for (auto m:configurations[0].modules) {
            fout << "[0]" << m.id << "\t" << m.pos << "\t" << int(m.orientCode) <<"\tgrey\n";
        }
        int time=1;
        fout << "vector<ModuleData> order= {";
        for (int i=list.size()-2; i>=0; i--) {
            // cherche le module de list[i] qui bouge par rapport list[i+1]
            int j=0;
            while (j<confSize && !(list[i].modules[j].hasMoved)) { j++; }
            fout << "{" << list[i].modules[j].id << ",{"
            << list[i].modules[j].pos[0] << "," << list[i].modules[j].pos[1] << "," << list[i].modules[j].pos[2] << "},"
            << int(list[i].modules[j].orientCode) << (i==0?"}":"},");
        }
        fout << "};\n";
        fout.close();
        return true;
    }
    return false;
}

void Node::config(ostream &os) {
    for (auto &m:modules) {
        os << "<block position=\"" << int(m.pos[0]) << "," << int(m.pos[1]) << "," << int(m.pos[2]) <<
        "\" orientation=\"" << int(m.orientCode) <<"\" mobile=\"true\" />\n";
    }
}

/** @warning arrays must be sorted **/
bool Node::operator==(const Node& other) {
    int i=0;
    /* position equal only */
    while (i<confSize && modules[i]==other.modules[i]) {i++;}
    return i==confSize;
}

ostream& operator<<(ostream& os, const Node &node) {
    os << node.id << ":";
    for (auto e:node.modules) {
        os << "{" << e.id << "," << e.pos << "," << int(e.orientCode) << "," << (e.hasMoved?"-":"|") << "} ";
    }
    for (auto p:node.parents) {
        os << "[" << int(p) << "]";
    }
    os << endl;
    return os;
}

void CatomsCombinationCode::printConfigurations() {
    ofstream fout("output.txt");
    uint32_t stage = 0;
    for (auto &c: configurations) {
        if (c.level > stage) {
            fout << "----------------- " << c.level << " -----------------\n";
            stage = c.level;
        }
        fout << c;
        //c.config(fout);
    }
    fout.close();
}

bool CatomsCombinationCode::addConfiguration(Node& newNode) {
    static uint32_t counter=1;

    newNode.sortPositions();
    auto conf = configurations.begin();
    while (conf!=configurations.end() && !(newNode==(*conf))) { conf++; }
    if (conf==configurations.end()) {
        newNode.id=counter++;
        configurations.push_back(newNode);
        return newNode.reachedTarget(*target);
    }
    return false;
}

/** @warning la taille de module doit être égale à confSize */
bool CatomsCombinationCode::addConfigurationFromMotion(uint32_t level,map<bID,BuildingBlock*> &modules, BuildingBlock*mobile, Cell3DPosition targetPos, uint8_t targetRot, uint32_t parent) {
    bool goalReached=false;
    // create new array
    Node newNode(level,parent);
    auto it = modules.begin();
    for (int i=0; i<confSize && it!=modules.end(); i++) {
        newNode.set(i,((*it).second==mobile)?
            ModuleData((*it).second->blockId,targetPos,targetRot,true):
            ModuleData((*it).second->blockId,(*it).second->position,(*it).second->orientationCode,false));
        it++;
    }
    // sort array for comparisons
    return addConfiguration(newNode);

    // search array in the existing list
    /*auto conf = configurations.begin();
    while (conf!=configurations.end() && !(newNode==(*conf))) { conf++; }
    if (conf==configurations.end()) {
        goalReached=newNode.reachedTarget(*target);
        addConfiguration(newNode);
    } else {
        // add a parent to the duplicated
        /*vector<uint32_t> list;
        int index=configurations[parent].id;
        while (index!=0) {
            list.push_back(index);
            cout << int(index) << ", ";
            index = configurations[index].parents[0];
        }
        cout << endl;
        (*conf).addParent(parent);
    }
    return goalReached;*/
}

// return the first and last indexes of configuration of a given level
pair<uint32_t,uint32_t> CatomsCombinationCode::getConfigurations(uint32_t level) {
    pair<uint32_t,uint32_t> res;

    uint32_t n=0;
    auto it=configurations.begin();
    while (it!=configurations.end() && (*it).level<level) {
        it++;
        n++;
    }
    res.first=n;
    while (it!=configurations.end() && (*it).level==level) {
        it++;
        n++;
    }
    res.second=n;
    return res;
}

bool CatomsCombinationCode::isInModulePos(const Cell3DPosition& pos,const map<bID,BuildingBlock*> &modules) {
    auto it = modules.begin();
    while (it!=modules.end() && pos!=(*it).second->position) {
        it++;
    }
    return it!=modules.end();
}

bool CatomsCombinationCode::isSetConnectedWithout(BuildingBlock *bb) {
    auto modules = wrld->buildingBlocksMap;
    lattice->remove(bb->position, false);
    wrld->disconnectBlock(bb,false);

    //cout << "remove :" << bb->blockId << endl;
    map<bID, bool> isConnected;
    stack<bID> st;
    // mark the first module as connected
    // and other to not connected
    auto it_bb = modules.begin();
    if (it_bb->second==bb) {
        it_bb++;
    }
    isConnected[it_bb->first] = true;
    st.push(it_bb->first);
    //cout << "push " << it_bb->first << endl;
    it_bb++;
    while (it_bb != modules.end()) {
        isConnected[it_bb->first] = false;
        it_bb++;
    }
    // treats stack
    int nbConnected=1;
    while (!st.empty()) {
        auto bb = modules[st.top()];
        st.pop();
        auto neighbors = bb->getNeighbors();
        for (auto &neighor : neighbors) {
            if (!isConnected[neighor->blockId]) {
                isConnected[neighor->blockId]=true;
                st.push(neighor->blockId);
                nbConnected++;
            }
        }
    }
    //cout << "count : " << nbConnected << "/" << modules.size() << endl;
    wrld->connectBlock(bb,false);
    return nbConnected==modules.size()-1;
}

void CatomsCombinationCode::resetConfiguration(uint32_t ind) {
    auto modules = wrld->buildingBlocksMap;
    // extract all mobile modules from lattice
    for (auto &m: modules) {
        if (static_cast<CatomsCombinationCode *>(m.second->blockCode)->isMobile) {
            lattice->remove(m.second->position, false);
            wrld->disconnectBlock(m.second,false);
        }
    }
    // move all mobile module in the lattice
    Node &node = configurations[ind];
    for (auto &md:node.modules) {
        auto module=modules[md.id];
        if (static_cast<CatomsCombinationCode *>(module->blockCode)->isMobile) {
            module->setPositionAndOrientation(md.pos, md.orientCode);
        }
    }
    // connect modules
    for (auto &m: modules) {
        if (static_cast<CatomsCombinationCode *>(m.second->blockCode)->isMobile) {
            wrld->connectBlock(m.second, false);
        }
    }
}

void CatomsCombinationCode::worldRun() {
    auto modules = wrld->buildingBlocksMap;
    // initialize configuration with initial set
    // create new array
    Node newNode(0,0);
    auto it = modules.begin();
    int i=0;
    while (it!=modules.end()) {
        if (static_cast<CatomsCombinationCode *>((*it).second->blockCode)->isMobile) {
            newNode.set(i++, ModuleData((*it).second->blockId, (*it).second->position, (*it).second->orientationCode,
                                      static_cast<CatomsCombinationCode *>((*it).second->blockCode)->isMobile));
        }
        it++;
    }
    // sort array for comparisons
    newNode.sortPositions();
    configurations.push_back(newNode);

    // motions
    int step=1;
    bool solutionFound=false;
    do {
    //for (int step=1; step<=computationStages; step++) {
        auto indexes = getConfigurations(step-1);
        //cout << "InitialConfs:"<< indexes.first << " to " << indexes.second << endl;
        uint32_t l = (indexes.second-indexes.first)/10;
        uint32_t stp=0;
        for (uint32_t ind=indexes.first; ind<indexes.second; ind++) {
            if (++stp==l) {
                cout << "-+";
                stp=0;
            }
            //cout << (ind-indexes.first+1) << "/" << (indexes.second-indexes.first) << endl;

            resetConfiguration(ind);

            int indexConfBegin=configurations.size();
            for (auto &m: modules) {
                if (static_cast<CatomsCombinationCode *>(m.second->blockCode)->isMobile) {
                    //cout << "for module #" << m.second->blockId << ": ";
                    // disconnect to test connection
                    Catoms3DBlock *bb = (Catoms3DBlock *) m.second;
                    if (isSetConnectedWithout(bb)) {
                        auto tab = Catoms3DMotionEngine::getAllRotationsForModule(bb);
                        //cout << tab.size() << endl;
                        Cell3DPosition finalPos;
                        short finalOrient;
                        for (auto &elem: tab) {
                            elem.second.init(((Catoms3DGlBlock *) bb->ptrGlBlock)->mat);
                            elem.second.getFinalPositionAndOrientation(finalPos, finalOrient);
                            //cout << finalPos << endl;
                            if (lattice->isInGrid(finalPos) && lattice->isFree(finalPos)) {
                                if (addConfigurationFromMotion(step, modules, m.second, finalPos, finalOrient,configurations[ind].id)) {
                                    solutionFound=true;
                                }
                            }
                        }
                    }
                }
            }
            /**********************************************************************/
            auto indexConfEnd=configurations.size()-1;
            // combinaisons de mouvements pour la configuration courante
            cout << "combinations: " << indexConfBegin << " to " << indexConfEnd << endl;
            while (indexConfBegin<indexConfEnd) {
                auto conf1 = configurations[indexConfBegin];
                cout << "conf1:" << conf1;
                // on essaye de combiner conf1 avec les autres
                // reset la configuration pere de conf1
                resetConfiguration(ind);
                // déterminer le module mobile dans conf1
                vector<ModuleData> mobiles1;
                for (auto &md: conf1.modules) {
                    if (md.hasMoved) {
                        mobiles1.push_back(md);
                    }
                }
                cout << "mobiles1:" << mobiles1.size() << ", " << mobiles1[0].id << endl;
                if (mobiles1.size() == 1) {
                    auto bb1 = static_cast<Catoms3DBlock *>(modules[mobiles1[0].id]);
                    auto tab = Catoms3DMotionEngine::getAllRotationsForModule(bb1);
                    // déterminer son mouvement
                    auto it_tab = tab.begin();
                    Cell3DPosition finalPos;
                    short finalOrient;
                    bool found = false;
                    while (it_tab != tab.end() && !found) {
                        (*it_tab).second.init(((Catoms3DGlBlock *) bb1->ptrGlBlock)->mat);
                        (*it_tab).second.getFinalPositionAndOrientation(finalPos, finalOrient);
                        found = (finalPos == mobiles1[0].pos && finalOrient == mobiles1[0].orientCode);
                        if (!found) it_tab++;
                    };
                    if (!found) cerr << "error: impossible motion!" << endl;
                    cout << "motion from " << bb1->position << " to " << finalPos << endl;
                    // construire la liste de cellules brulées
                    auto burnt1 = (*it_tab).first->getBlockingCellsList(bb1);
                    cout << "burnt cells1:";
                    for (auto &b: burnt1) {
                        cout << b << ",";
                    }
                    cout << "\n";

                    int indexConf = indexConfBegin + 1;
                    while (indexConf <= indexConfEnd &&
                           configurations[indexConf].parents[0] == configurations[indexConfBegin].parents[0]) {
                        auto conf2 = configurations[indexConf];
                        cout << "conf2(" << indexConf << "): " << conf2;
                        // déterminer le module mobile dans indConf
                        vector<ModuleData> mobiles2;
                        for (auto &md: conf2.modules) {
                            if (md.hasMoved) {
                                mobiles2.push_back(md);
                            }
                        }
                        // si différent de mobile1
                        if (mobiles2.size() == 1 && mobiles2[0].id != bb1->blockId) {
                            cout << "mobile2: " << mobiles2[0].id << endl;
                            // déterminer son mouvement
                            auto bb2 = static_cast<Catoms3DBlock *>(modules[mobiles2[0].id]);
                            tab = Catoms3DMotionEngine::getAllRotationsForModule(bb2);
                            it_tab = tab.begin();
                            Cell3DPosition finalPos;
                            short finalOrient;
                            bool found = false;
                            while (it_tab != tab.end() && !found) {
                                (*it_tab).second.init(((Catoms3DGlBlock *) bb2->ptrGlBlock)->mat);
                                (*it_tab).second.getFinalPositionAndOrientation(finalPos, finalOrient);
                                found = (finalPos == mobiles2[0].pos && finalOrient == mobiles2[0].orientCode);
                                if (!found) it_tab++;
                            };
                            if (!found) cerr << "error: impossible motion!" << endl;
                            cout << "motion from " << bb2->position << " to " << finalPos << endl;
                            // construire la liste de cellules brulée
                            auto burnt2 = (*it_tab).first->getBlockingCellsList(bb2);
                            // afficher
                            cout << "burnt cells:";
                            for (auto &b: burnt2) {
                                cout << b << ",";
                            }
                            cout << "\n";
                            // si les cellules brulée sont disjointes ajouter la combinaison des deux mouvements.
                            auto b1 = burnt1.begin();
                            while (b1 != burnt1.end() && (find(burnt2.begin(), burnt2.end(), *b1) == burnt2.end())) {
                                b1++;
                            }
                            if (b1 == burnt1.end()) {
                                cout << "fusion: " << conf1 << " : " << conf2;
                                Node newNode(conf1, conf2, bb2->blockId);
                                cout << "generate: " << newNode << endl;
                                addConfiguration(newNode);
                            }
                        }
                        indexConf++;
                    }
                }
                indexConfBegin++;
            }

        }

        cout << "\n----------------------------------- depth " << step << "-----------------------------------\n";

        step++;
    } while (step<3 && !solutionFound);
    printConfigurations();
    if (generateSolutions()) cout << "Goal reached!" << endl;
}

void CatomsCombinationCode::startup() {
    if (getId() == 1) worldRun();
}

void CatomsCombinationCode::parseUserBlockElements(TiXmlElement *config) {
    const char *attr = config->Attribute("mobile");
    if (attr != nullptr) {
        string str(attr);
        if (str == "true" || str == "1" || str == "yes") {
            isMobile = true;
            std::cout << getId() << " is mobile!" << std::endl; // complete with your code
        }
    }
}