#include <climits>
#include <algorithm>
#include "catomsCombinationCode.h"
#include <fstream>
#include <iostream>
#include <stack>

vector<Node> configurations;

class StackData {
public:
    uint32_t confId;
    Node ptrNode;
    uint32_t parentId;
    uint8_t level;

    StackData(uint32_t _id, Node _ptr, uint32_t _parent, uint8_t _level) : confId(_id), ptrNode(_ptr),
                                                                            parentId(_parent), level(_level) {};
};

uint16_t CatomsCombinationCode::generateSolutions() {
    uint16_t n = 0;
    uint8_t nStepMax=0;
    ofstream fout("result.txt");

// search the goal configuration
    vector<vector<Solution>> solutions;
    auto itConf = configurations.begin();
    while (itConf != configurations.end() && !(*itConf).modules.reachedTarget(*target)) {
        itConf++;
    }
    if (itConf != configurations.end()) {
        cout << "final=" << itConf->id << endl;
        queue<StackData> q;
        for (auto &parents: itConf->mobilesPerParent) {
            //cout << "a traiter #" << n << ": " << itConf->id << "," << parents.first << endl;
            q.push({n++, (*itConf), parents.first, 0});

            //cout << "create sol#" << int(n - 1) << endl;
            vector<Solution> Sn;
            solutions.push_back(Sn);
        }
        while (!q.empty()) {
            auto current = q.front();
            q.pop();
            //cout << "traite #" << int(current.confId) << ":" << current.ptrNode.id << "," << current.parentId << endl;
            if (current.level>nStepMax) nStepMax=current.level;
// créer la liste des solutions
            // pour tous les mobiles
            for (auto &p: current.ptrNode.mobilesPerParent[current.parentId]) {
                //cout << "add to solutions[" << int(current.level) << "] " << p.first << "," << p.second << endl;
                solutions[current.confId].push_back({current.level,p.first,p.second});
            }

            if (current.parentId != 0) {
                // go to parent line
                auto parentConf = configurations[current.parentId];
                //cout << "parentConf=" << parentConf.id << endl;
                auto itParent = parentConf.mobilesPerParent.begin();
                //cout << "a traiter #" << current.confId << ": " << parentConf.id << "," << (*itParent).first << endl;
                q.push({current.confId, parentConf, (*itParent).first, uint8_t(current.level+1)});
                itParent++;
                while (itParent != parentConf.mobilesPerParent.end()) {
                    //cout << "a traiter #" << n << ": " << parentConf.id << "," << (*itParent).first << endl;
                    q.push({n++, parentConf, (*itParent).first, uint8_t(current.level+1)});
                    //cout << "create sol#" << int(n - 1) << endl;
                    vector<Solution> Sn;
                    solutions.push_back(Sn);
                    // copier current.confId dans n-1
                    for (auto &cpy:solutions[current.confId]) {
                        solutions[n-1].push_back(cpy);
                    }
                    itParent++;
                }
            }
        }
    }
    // affichage
    fout << "// # solutions: " << n << "\nvector<vector<ModuleData>> order= {";
    bool firstsol = true;
    int cpt = 0;
    // for each solution
    for (auto &sol: solutions) {
        fout << (firstsol ? "{" : ",{");
        firstsol = false;
        bool firstconf = true;
        for (int i = sol.size() - 1; i >= 0; i--) {
            if (!firstconf) fout << ",";
            firstconf = false;
            fout << "{{" << sol[i].fromPos[0] << ","
                 << sol[i].fromPos[1] << ","
                 << sol[i].fromPos[2] << "},{"
                 << sol[i].toPos[0] << ","
                 << sol[i].toPos[1] << ","
                 << sol[i].toPos[2] << "},"
                 << int(nStepMax-sol[i].step) << "}";
        }
        fout << "}\n";
    }
    fout << "};\n";
    fout.close();
    return n;
}

void Node::config(ostream &os) {
    for (auto &m: modules.pos) {
        os << "<block position=\"" << int(m[0]) << "," << int(m[1]) << "," << int(m[2]) <<
           //           "\" orientation=\"" << int(m.orientCode) << "\" mobile=\"true\" />\n";
           "\" mobile=\"true\" />\n";
    }
}

/** @warning arrays must be sorted **/
bool ModuleConfig::operator==(const ModuleConfig &other) {
    int i = 0;
    /* position equal only */
    while (i < confSize && pos[i] == other.pos[i]) { i++; }
    return i == confSize;
}

/** @warning arrays must be sorted **/
bool Node::operator==(const Node &other) {
    return modules == other.modules;
}

ostream &operator<<(ostream &os, const Node &node) {
    os << node.id << ":";
    for (auto &m: node.mobilesPerParent) {
        os << "[" << m.first << "] ";
        for (auto &p: node.modules.pos) {
            // is p mobile
            auto itMobile = m.second.begin();
            while (itMobile != m.second.end() && itMobile->second != p) { itMobile++; }
            if (itMobile == m.second.end()) {
                os << "{" << p << "} ";
            } else {
                os << "{" << itMobile->first << "," << itMobile->second << "} ";
            }
        }
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

bool CatomsCombinationCode::addConfiguration(Node &newNode) {
    static uint32_t counter = 1;

    newNode.modules.sortPositions();
    auto conf = configurations.begin();
    while (conf != configurations.end() && !(newNode == (*conf))) { conf++; }
    if (conf == configurations.end()) {
        newNode.id = counter++;
        configurations.push_back(newNode);
        return newNode.modules.reachedTarget(*target);
    } else {
        if (newNode.level == (*conf).level) {
            // the new node has only one parent
            auto ptr = newNode.mobilesPerParent.begin();
            (*conf).addParent(ptr->first, ptr->second);
        }
    }
    return false;
}

/** @warning la taille de module doit être égale à confSize */
bool CatomsCombinationCode::addConfigurationFromMotion(uint32_t level, map<bID, BuildingBlock *> &modules,
                                                       BuildingBlock *mobile, Cell3DPosition targetPos,
                                                       uint32_t parent) {
    //cout << "add " << mobile->position << " to " << targetPos << " parent=" << parent << endl;
    // create new array
    vector<pair<Cell3DPosition, Cell3DPosition>> mobilePositions;
    ModuleConfig config;
    auto it = modules.begin();
    for (int i = 0; i < confSize && it != modules.end(); i++) {
        bool isMobile = ((*it).second->position == mobile->position);
        config.set(i, isMobile ? targetPos : (*it).second->position);
        if (isMobile) {
            //cout << "add (" << (*it).second->position << "," << targetPos << ")" << endl;
            mobilePositions.push_back({(*it).second->position, targetPos});
        }
        it++;
    }
    Node newNode(level, config, parent, mobilePositions);
    // sort array for comparisons
    return addConfiguration(newNode);
}

// return the first and last indexes of configuration of a given level
pair<uint32_t, uint32_t> CatomsCombinationCode::getConfigurations(uint32_t level) {
    pair<uint32_t, uint32_t> res;

    uint32_t n = 0;
    auto it = configurations.begin();
    while (it != configurations.end() && (*it).level < level) {
        it++;
        n++;
    }
    res.first = n;
    while (it != configurations.end() && (*it).level == level) {
        it++;
        n++;
    }
    res.second = n;
    return res;
}

bool CatomsCombinationCode::isInModulePos(const Cell3DPosition &pos, const map<bID, BuildingBlock *> &modules) {
    auto it = modules.begin();
    while (it != modules.end() && pos != (*it).second->position) {
        it++;
    }
    return it != modules.end();
}

bool CatomsCombinationCode::isSetConnectedWithout(BuildingBlock *bb) {
    auto modules = wrld->buildingBlocksMap;
    lattice->remove(bb->position, false);
    wrld->disconnectBlock(bb, false);

    //cout << "remove :" << bb->blockId << endl;
    map<bID, bool> isConnected;
    stack<bID> st;
    // mark the first module as connected
    // and other to not connected
    auto it_bb = modules.begin();
    if (it_bb->second == bb) {
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
    int nbConnected = 1;
    while (!st.empty()) {
        auto bb = modules[st.top()];
        st.pop();
        auto neighbors = bb->getNeighbors();
        for (auto &neighor: neighbors) {
            if (!isConnected[neighor->blockId]) {
                isConnected[neighor->blockId] = true;
                st.push(neighor->blockId);
                nbConnected++;
            }
        }
    }
    //cout << "count : " << nbConnected << "/" << modules.size() << endl;
    wrld->connectBlock(bb, false);
    return nbConnected == modules.size() - 1;
}

void CatomsCombinationCode::resetConfiguration(uint32_t ind) {
    auto modules = wrld->buildingBlocksMap;
    // extract all mobile modules from lattice
    for (auto &m: modules) {
        if (static_cast<CatomsCombinationCode *>(m.second->blockCode)->isMobile) {
            lattice->remove(m.second->position, false);
            wrld->disconnectBlock(m.second, false);
        }
    }
    // move all mobile module in the lattice
    Node &node = configurations[ind];
    uint8_t i = 0;
    for (auto &m: modules) {
        if (static_cast<CatomsCombinationCode *>(m.second->blockCode)->isMobile) {
//             module->setPositionAndOrientation(md.pos, md.orientCode);
            m.second->setPositionAndOrientation(node.modules.pos[i++], 0);
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
    vector<pair<Cell3DPosition, Cell3DPosition>> mobilePositions;
    ModuleConfig config;
    auto it = modules.begin();
    int i = 0;
    while (it != modules.end()) {
        auto isMobile = static_cast<CatomsCombinationCode *>((*it).second->blockCode)->isMobile;
        cout << (*it).second->blockId << ":" << (*it).second->position << ", mob:" << isMobile << endl;
        if (isMobile) {
            config.set(i++, (*it).second->position);
//            mobilePositions.push_back({(*it).second->position,(*it).second->position});
        }
        it++;
    }
    Node newNode(0, config, 0, mobilePositions);
    // sort array for comparisons
    newNode.modules.sortPositions();
    configurations.push_back(newNode);

    // motions
    int step = 1;
    bool solutionFound = false;
    do {
        auto indexes = getConfigurations(step - 1);
        //cout << "InitialConfs:"<< indexes.first << " to " << indexes.second << endl;
        uint32_t l = (indexes.second - indexes.first) / 10;
        uint32_t stp = 0;
        for (uint32_t ind = indexes.first; ind < indexes.second; ind++) {
            if (++stp == l) {
                cout << "-+";
                stp = 0;
            }
            //cout << (ind-indexes.first+1) << "/" << (indexes.second-indexes.first) << endl;

            resetConfiguration(ind);

            int indexConfBegin = configurations.size();
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
                                if (addConfigurationFromMotion(step, modules, m.second, finalPos,
                                                               configurations[ind].id)) {
                                    solutionFound = true;
                                }
                            }
                        }
                    }
                }
            }
            /**********************************************************************/
            auto indexConfEnd = configurations.size() - 1;
            // combinaisons de mouvements pour la configuration courante
            //cout << "combinations: " << indexConfBegin << " to " << indexConfEnd << endl;
            while (indexConfBegin < indexConfEnd) {
                auto conf1 = configurations[indexConfBegin];
                //cout << "conf1:" << conf1;
                // on essaye de combiner conf1 avec les autres
                // reset la configuration pere de conf1
                resetConfiguration(ind);
                // déterminer le module mobile dans conf1
                vector<Solution> mobiles1;
                auto conf1Mobiles = conf1.mobilesPerParent.begin();
                for (auto &c1: conf1Mobiles->second) {
                    mobiles1.push_back({1, c1.first, c1.second});
                }
                //cout << "mobiles1:" << mobiles1.size() << ", " << mobiles1[0].id << endl;
                if (mobiles1.size() == 1) {
                    //auto bb1 = static_cast<Catoms3DBlock *>(modules[mobiles1[0].id]);
                    // bb1 est le module en mobiles1[0].fromPos
                    auto it = modules.begin();
                    while (it != modules.end() && it->second->position != mobiles1[0].fromPos) {
                        it++;
                    }
                    auto bb1 = static_cast<Catoms3DBlock *>(it->second);
                    auto tab = Catoms3DMotionEngine::getAllRotationsForModule(bb1);
                    // déterminer son mouvement
                    auto it_tab = tab.begin();
                    Cell3DPosition finalPos;
                    short finalOrient;
                    bool found = false;
                    while (it_tab != tab.end() && !found) {
                        (*it_tab).second.init(((Catoms3DGlBlock *) bb1->ptrGlBlock)->mat);
                        (*it_tab).second.getFinalPositionAndOrientation(finalPos, finalOrient);
//                        found = (finalPos == mobiles1[0].pos && finalOrient == mobiles1[0].orientCode);
                        found = (finalPos == mobiles1[0].toPos);
                        if (!found) it_tab++;
                    };
                    if (!found) cerr << "error: impossible motion!" << endl;
                    //cout << "motion from " << bb1->position << " to " << finalPos << endl;
                    // construire la liste de cellules brulées
                    auto burnt1 = (*it_tab).first->getBlockingCellsList(bb1);
                    burnt1.push_back(bb1->position);
                    burnt1.push_back(finalPos);
                    burnt1.push_back(it_tab->second.pivot->position);
                    /*cout << "burnt cells1:";
                    for (auto &b: burnt1) {
                        cout << b << ",";
                    }
                    cout << "\n";*/

                    int indexConf = indexConfBegin + 1;
                    while (indexConf <= indexConfEnd &&
                           configurations[indexConf].mobilesPerParent.begin()->first ==
                           configurations[indexConfBegin].mobilesPerParent.begin()->first) {
                        auto conf2 = configurations[indexConf];
                        //cout << "conf2(" << indexConf << "): " << conf2;
                        // déterminer le module mobile dans indConf
                        /*vector<ModuleData> mobiles2;
                        for (auto &md: conf2.modules) {
                            if (md.hasMoved) {
                                mobiles2.push_back(md);
                            }
                        }*/
                        vector<Solution> mobiles2;
                        auto conf2Mobiles = conf2.mobilesPerParent.begin();
                        for (auto &c2: conf1Mobiles->second) {
                            mobiles2.push_back({1, c2.first, c2.second});
                        }

                        // si différent de mobile1
                        if (mobiles2.size() == 1 && mobiles1[0].fromPos != mobiles2[0].fromPos) {
                            //cout << "mobile2: " << mobiles2[0].id << endl;
                            // déterminer son mouvement
                            //auto bb2 = static_cast<Catoms3DBlock *>(modules[mobiles2[0].id]);
                            // bb2 est le module en mobiles1[0].fromPos
                            auto it = modules.begin();
                            while (it != modules.end() && it->second->position != mobiles2[0].fromPos) {
                                it++;
                            }
                            auto bb2 = static_cast<Catoms3DBlock *>(it->second);
                            tab = Catoms3DMotionEngine::getAllRotationsForModule(bb2);
                            it_tab = tab.begin();
                            Cell3DPosition finalPos;
                            short finalOrient;
                            bool found = false;
                            while (it_tab != tab.end() && !found) {
                                (*it_tab).second.init(((Catoms3DGlBlock *) bb2->ptrGlBlock)->mat);
                                (*it_tab).second.getFinalPositionAndOrientation(finalPos, finalOrient);
//                                found = (finalPos == mobiles2[0].pos && finalOrient == mobiles2[0].orientCode);
                                found = (finalPos == mobiles2[0].toPos);
                                if (!found) it_tab++;
                            };
                            if (!found) cerr << "error: impossible motion!" << endl;
                            //cout << "motion from " << bb2->position << " to " << finalPos << endl;
                            // construire la liste de cellules brulée
                            auto burnt2 = (*it_tab).first->getBlockingCellsList(bb2);
                            burnt2.push_back(bb2->position);
                            burnt2.push_back(finalPos);
                            burnt2.push_back(it_tab->second.pivot->position);

                            // afficher
                            /*cout << "burnt cells:";
                            for (auto &b: burnt2) {
                                cout << b << ",";
                            }
                            cout << "\n";*/
                            // si les cellules brulée sont disjointes ajouter la combinaison des deux mouvements.
                            auto b1 = burnt1.begin();
                            while (b1 != burnt1.end() && (find(burnt2.begin(), burnt2.end(), *b1) == burnt2.end())) {
                                b1++;
                            }
                            if (b1 == burnt1.end()) {
                                //cout << "fusion: " << conf1 << " : " << conf2;
                                Node newNode(conf1, mobiles2[0].fromPos, mobiles2[0].toPos);
                                //cout << "generate: " << newNode << endl;
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
    } while (!solutionFound);
    //} while (step<2);
    printConfigurations();
    cout << "Goal reached! " << generateSolutions() << " times." << endl;
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