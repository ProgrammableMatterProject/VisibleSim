#ifndef CatomsCombinationCode_H_
#define CatomsCombinationCode_H_
#include <algorithm>
#include "robots/catoms3D/catoms3DSimulator.h"
#include "robots/catoms3D/catoms3DBlockCode.h"
#include "robots/catoms3D/catoms3DMotionEngine.h"
#include "motion/teleportationEvents.h"

using namespace Catoms3D;

const int confSize=4;

class ModuleData {
public:
    uint16_t id;
    Cell3DPosition pos;
    uint8_t orientCode;
    bool hasMoved;
    bool isConnected;
    ModuleData(uint16_t _id, Cell3DPosition _pos, uint8_t _orientCode,bool _hm):id(_id),pos(_pos),orientCode(_orientCode),hasMoved(_hm) {};
    ModuleData() { id=0; orientCode=0; };
    bool operator==(const ModuleData& other) { return pos==other.pos; }
};

class Node {
public:
    uint16_t level;
    array<ModuleData,confSize> modules;
    uint32_t id;
    vector<uint32_t> parents;

    Node(uint16_t _level,uint32_t _parent):id(0),level(_level) {
        parents.push_back(_parent);
    };
    Node(const Node &src,const Node &add,int mobileId):
        id(0),level(src.level),parents(src.parents),modules(src.modules) {
        // seach mobile in local
        auto itSrc=modules.begin();
        while (itSrc!=modules.end() && (*itSrc).id!=mobileId) { itSrc++; }
        // search mobile in add
        auto itAdd=add.modules.begin();
        while (itAdd!=add.modules.end() && (*itAdd).id!=mobileId) { itAdd++; }
        *itSrc = (*itAdd);
    }
    void sortPositions() {
        sort(modules.begin(),modules.end(),[](ModuleData &n1,ModuleData &n2) { return n1.pos<n2.pos; }); };

    void set(int i,ModuleData md) { modules[i]=md; };
    bool operator==(const Node& other);
    ModuleData operator[](int i) { return modules[i]; };
    void addParent(uint32_t p) {
        if (find(parents.begin(), parents.end(), p) == parents.end()) {
            parents.push_back(p);
        }
    }
    bool reachedTarget(const Target &target) {
        auto it=modules.begin();
        while (it!=modules.end() && target.isInTarget((*it).pos)) { it++; }
        return it==modules.end();
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
    bool addConfigurationFromMotion(uint32_t level,map<bID,BuildingBlock*> &modules, BuildingBlock*mobile, Cell3DPosition targetPos, uint8_t targetRot, uint32_t parent);
    void printConfigurations();
    pair<uint32_t,uint32_t> getConfigurations(uint32_t level);
    bool isInModulePos(const Cell3DPosition& pos,const map<bID,BuildingBlock*> &modules);
    bool generateSolutions();
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
