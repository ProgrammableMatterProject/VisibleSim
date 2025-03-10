#ifndef CatomsCombinationCode_H_
#define CatomsCombinationCode_H_
#include <algorithm>
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DMotionEngine.h"
#include "motion/teleportationEvents.h"

using namespace Catoms3D;

const int confSize=3;

class Solution {
public:
    uint8_t step;
    Cell3DPosition fromPos;
    Cell3DPosition toPos;
    Solution(uint8_t s, const Cell3DPosition &from, const Cell3DPosition &to):step(s),fromPos(from),toPos(to) {};
};

class ModuleConfig {
public:
    array<Cell3DPosition,confSize> pos;
    //uint8_t orientCode;
    //bool isConnected;
//    ModuleData(uint16_t _id, Cell3DPosition _pos, uint8_t _orientCode,bool _hm):id(_id),pos(_pos),orientCode(_orientCode),hasMoved(_hm) {};
    //ModuleData(Cell3DPosition _pos),pos(_pos) {};

    ModuleConfig() {
        for (auto &p:pos) {
            p.set(numeric_limits<short>::max(),numeric_limits<short>::max(),numeric_limits<short>::max());
        }
    };
    void set(uint8_t i,Cell3DPosition p) { pos[i]=p; };
    void sortPositions() {
        sort(pos.begin(),pos.end(),[](Cell3DPosition &n1,Cell3DPosition &n2) { return n1<n2; });
    };
    bool operator==(const ModuleConfig& other);
    bool reachedTarget(const Target &target) {
        auto it=pos.begin();
        while (it!=pos.end() && target.isInTarget((*it))) { it++; }
        return it==pos.end();
    }
};

class Node {
public:
    uint16_t level;
    ModuleConfig modules;
    uint32_t id;
    map<uint32_t,vector<pair<Cell3DPosition,Cell3DPosition>>> mobilesPerParent; ///< List of parent with array of mobiles and itself !

    Node(uint16_t _level,const ModuleConfig &conf,uint32_t _parent,const vector<pair<Cell3DPosition,Cell3DPosition>> &motions):id(0),level(_level),modules(conf) {
        mobilesPerParent[_parent]=motions;
    };
    Node(const Node &src,const Cell3DPosition &mobileFrom,const Cell3DPosition &mobileTo):
        id(0),level(src.level),modules(src.modules) {
        mobilesPerParent.begin()->second.push_back({mobileFrom,mobileTo});
    }

    bool operator==(const Node& other);
    /*ModuleData operator[](int i) { return modules[i]; };*/
    void addParent(uint32_t p,const vector<pair<Cell3DPosition,Cell3DPosition>> &motions) {
        mobilesPerParent[p]=motions;
    }
    friend ostream& operator<<(ostream& os, const Node &node);
    void config(ostream &os);

};

class CatomsCombinationCode : public Catoms3DBlockCode {
private:
    FCCLattice *lattice;
    bool isMobile;
    Catoms3DWorld *wrld;
public :
    CatomsCombinationCode(Catoms3DBlock *host):Catoms3DBlockCode(host) {
        wrld=Catoms3DWorld::getWorld();
        isMobile=false;
        lattice = (FCCLattice*)(Catoms3D::getWorld()->lattice);
    };
    ~CatomsCombinationCode() {};

    void startup();
    void parseUserBlockElements(TiXmlElement *config);
    void worldRun();
    bool addConfiguration(Node &newNode);
    bool addConfigurationFromMotion(uint32_t level,map<bID,BuildingBlock*> &modules, BuildingBlock*mobile, Cell3DPosition targetPos, uint32_t parent);
    void printConfigurations();
    pair<uint32_t,uint32_t> getConfigurations(uint32_t level);
    bool isInModulePos(const Cell3DPosition& pos,const map<bID,BuildingBlock*> &modules);
    uint16_t generateSolutions();
    bool isSetConnectedWithout(BuildingBlock *bb);
    void resetConfiguration(uint32_t ind);

    /*****************************************************************************/
    /** needed to associate code to module                                      **/
    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return(new CatomsCombinationCode((Catoms3DBlock*)host));
    };
    /*****************************************************************************/
};

#endif /* CatomsCombinationCode_H_ */
