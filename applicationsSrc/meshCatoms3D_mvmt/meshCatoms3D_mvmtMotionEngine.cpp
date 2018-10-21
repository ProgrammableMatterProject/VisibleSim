/**
 * @file   catoms3DMotionEngine.cpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Jul 10 15:41:09 2018
 * 
 * @brief  
 * 
 * 
 */

#include "catoms3DMotionEngine.hpp"
#include "messages.hpp"
#include "catoms3DMotionRules.h"

#include <algorithm>

static std::random_device random_dev;
static std::mt19937 generator(random_dev());
P2PNetworkInterface *Catoms3DMotionEngine::getNextUnprocessedInterface(bool randomly) {
    vector<P2PNetworkInterface*> unflaggedItfs;
    
    for (const auto& element : flag) {
        if (!element.second) {
            if (not randomly) return element.first;
            else unflaggedItfs.push_back(element.first);
        }
    }

    // Randomly 
    std::shuffle(unflaggedItfs.begin(), unflaggedItfs.end(), generator);

    return unflaggedItfs.empty() ? NULL : unflaggedItfs[0];
}

void Catoms3DMotionEngine::resetDFS() {
    flag.clear();
    
    for (P2PNetworkInterface *itf : catom.getP2PNetworkInterfaces()) {
        if (itf->isConnected()) {
            flag[itf] = false;
        }
    }
}

void Catoms3DMotionEngine::attemptMovingTo(const Cell3DPosition& dest) {
    resetDFS();

    goalPosition = dest;
    path.clear();
    BaseSimulator::getWorld()->lattice->highlightCell(dest, YELLOW);

    if (catom.position == dest) return;
    
    P2PNetworkInterface *unprocessedNeighbor = getNextUnprocessedInterface();
    if (unprocessedNeighbor) {        
        bc.sendMessage(new FindPathMessage(goalPosition),
                       unprocessedNeighbor, 0, 0);
    } else {
        // console << "Houston we have a problem." << "\n";
        catom.setColor(BLACK); // todo
        cout << "No Neighbor found" << endl;
        awaitKeyPressed();
        assert(false);
    }
}

void Catoms3DMotionEngine::handleRotationEnd() {
    assert(!path.empty());

    trimPath();
    
    if (catom.position == goalPosition) {
#ifdef DEBUG_GROWTH_BLOCK                   
        cout << "--- Module #" << catom.blockId
             << " has reached target position " << goalPosition
             << " ---" << endl;
#endif
        BaseSimulator::getWorld()->lattice->unhighlightCell(goalPosition);
    } else {
        // Perform next move towards current target
        performNextMoveTowardsGoal();
    }
}

void Catoms3DMotionEngine::performNextMoveTowardsGoal() {
    assert(!path.empty());
        
    // List all connectors that could be filled in order to connect
    //  a neighbor module to the last hop or that would help reach parent's path connectors
    PathHop &lastHop = path.back();

    if (lastHop.catomIsAlreadyOnBestConnector(catom.position)) {
        path.pop_back();
        if (path.empty()) return;
        else lastHop = path.back();
    }
    
    bool rotationIsPossible = computeNextRotation(path);
    if (!rotationIsPossible) 
    {
#ifdef DEBUG_ROTATIONS
        cout << "performNextMoveTowardsGoal: "
             << "Could not compute feasible rotation plan to parent" << endl;
#endif
                
        awaitKeyPressed();        
        
        assert(false);
    } 
}

bool Catoms3DMotionEngine::computeNextRotation(list<PathHop>& path) 
{
    PathHop &lastHop = path.back();

    short catomDockingConnector = catom.getConnectorId(lastHop.getPosition());
    short pivotDockingConnector =
        lastHop.getHopConnectorAtPosition(catom.position); // so-so    

#ifdef DEBUG_ROTATIONS
    cout << "computeNextRotation: " << "module is on " << pivotDockingConnector
         << " -> " << catomDockingConnector << endl
         << "input path: " << lastHop << endl;
#endif

    std::vector<short> adjacentPathConnectors;
    std::map<short, short> mirrorConnector;
    findAdjacentConnectors(catom, lastHop,
                           pivotDockingConnector, catomDockingConnector,
                           adjacentPathConnectors, mirrorConnector);
        
    vector<Catoms3DMotionRulesLink*> mrl;
    getMotionRulesFromConnector(catom, catomDockingConnector, mrl);
            
    // Mobile Module:
    // Check if module can move to any of the adjacent path connectors of pivot.
    // Input connector set is ordered by distance to target, so the search should stop
    // as soon as a solution has been found
    nextRotation =
        findConnectorsPath(mrl, catomDockingConnector, adjacentPathConnectors, catom);
            
    if (nextRotation) {
        // FIXME:
        for (auto const& ph : path)
            cout << "ph: " << ph;
        cout << *nextRotation << endl;
        // FIXME:
        
        short dirTo = nextRotation->getConToID();
        lastHop.prune(mirrorConnector[dirTo]);
        
#ifdef DEBUG_ROTATIONS
        cout << "Moving : " << *nextRotation << endl << endl;
#endif

        // // FIXME:
        // awaitKeyPressed();
        // // FIXME:
        nextRotation->sendRotationEvent(&catom,
                                        catom.getNeighborOnCell(lastHop.getPosition()),
                                        getScheduler()->now() + MSG_DELAY);
        nextRotation = NULL;

        return true;        
    }

    return false;    
}

bool
Catoms3DMotionEngine::addModuleToPath(const Catoms3DBlock& catom,
                                      list<PathHop>& path,
                                      short pivotDockingConnector,
                                      short catomDockingConnector)
{
    // List all connectors that could be filled in order to connect
    //  a neighbor module to the last hop
    // cout << "addModuleToPath: " << "lastHop: " << lastHop << endl;
    
    std::map<short, int> pathConnectorsDistance;
    if (!path.empty()) {
        PathHop &lastHop = path.back();
        findAdjacentConnectorsAndDistances(catom, lastHop,
                                           pivotDockingConnector,
                                           catomDockingConnector,
                                           pathConnectorsDistance);
    } else {
        InitializeConnectorsAndDistances(pivotDockingConnector,
                                         pathConnectorsDistance);
    }
    
    std::set<short> targetCons; // Adjacent target connectors on current module
    for (auto const& pair : pathConnectorsDistance) {
        targetCons.insert(pair.first);
    }
    
    computePathConnectorsAndDistances(catom, targetCons,
                                      pathConnectorsDistance);

    // Module cannot connect to path, notify caller
    if (pathConnectorsDistance.empty()) return false;
    
    // Module can connect to path, create entry and add to path
    PathHop newHop = PathHop(catom.position, catom.orientationCode,
                             pathConnectorsDistance);
                             // pathAbsoluteDirectionsDistance);

#ifdef DEBUG_API
    cout << "addModuleToPath: " << "newHop: " << newHop << endl;
#endif
    
    path.push_back(newHop);

    // for (PathHop& hop : path) cout << hop << endl;
    // cout << endl;
    
    return true;
}

bool Catoms3DMotionEngine::getMotionRulesFromConnector(const Catoms3DBlock &catom,
                                                       short conFrom,
                                                       vector<Catoms3DMotionRulesLink*>& links)
{
    return getMotionRules()->getValidMotionList(&catom, conFrom, links);
}


bool
Catoms3DMotionEngine::computePathConnectorsAndDistances(const Catoms3DBlock &catom,
                                                        std::set<short> consFrom,
                                                        std::map<short, int>& distance)
{
    vector<Catoms3DMotionRulesLink*> motionRulesLinks;
    getMotionRules()->getValidSurfaceLinksOnCatom(&catom, motionRulesLinks);
    
    for (short conFrom : consFrom) {
        computePathConnectorsAndDistances(motionRulesLinks, catom, conFrom, distance);
    }

    return !distance.empty();
}

bool
Catoms3DMotionEngine::computePathConnectorsAndDistances(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                                        const Catoms3DBlock& catom,
                                                        short conFrom,
                                                        std::map<short, int>& distance)
{
    if (motionRulesLinks.empty()) return false;
    
    // Build an adjacency matrix out of the motion rules link for easier graph traversal
    list<Catoms3DMotionRulesLink*> linkMatrix[12][12];
    std::array<list<short>, 12> adj;
    for (const auto& link : motionRulesLinks) {
        std::array<short, 2> linkCons = link->getConnectors();
        linkMatrix[linkCons[0]][linkCons[1]].push_back(link);
        adj[linkCons[0]].push_back(linkCons[1]);
    }

    // BFS-parent of every connector
    // -1 represents no parent
    // 12 represents BFS source
    short parent[12] = {
        -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1
    }; parent[conFrom] = 12;

    list<short> queue;
    queue.push_back(conFrom);

    list<short>::iterator itCon;
    FCCLattice* lattice = static_cast<FCCLattice*>(Catoms3DWorld::getWorld()->lattice);
    
    while(!queue.empty()) {
        short connector = queue.front();
        // cout << connector << " ";
        queue.pop_front();

        // Get all adjacent connectors of dequeued connector.
        // If one of the adjacent connectors has not been visited, mark
        //  it as visited and enqueue it
        for (itCon = adj[connector].begin();
             itCon != adj[connector].end();
             itCon++)
        {
            if (parent[*itCon] == -1) {
                Cell3DPosition nPosCatom; catom.getNeighborPos(*itCon, nPosCatom);

                if (!lattice->isPositionBlocked(nPosCatom, catom.position)) {
                    parent[*itCon] = connector;

                    int depth = distance[parent[*itCon]] + 1;
                    if (distance.find(*itCon) == distance.end()
                        || distance[*itCon] > depth) {

                        distance[*itCon] = depth;
                    }

                    queue.push_back(*itCon);
                } else {
#ifdef DEBUG_API                    
                    cout << "findAdjacentConnectors: " << *itCon << " dist is -> "
                         << " => UNREACHABLE!" << endl;
#endif
                }
            }
        }
    }

    return !distance.empty();
}


Catoms3DMotionRulesLink*
Catoms3DMotionEngine::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                         short conFrom,
                                         short conTo,
                                         Catoms3DBlock& catom)
{
    if (motionRulesLinks.empty()) return NULL;

    // Build an adjacency matrix out of the motion rules link for easier graph traversal
    list<Catoms3DMotionRulesLink*> linkMatrix[12][12];
    std::array<list<short>, 12> adj;
    for (Catoms3DMotionRulesLink* link : motionRulesLinks) {
        std::array<short, 2> linkCons = link->getConnectors();
        linkMatrix[linkCons[0]][linkCons[1]].push_back(link);
        adj[linkCons[0]].push_back(linkCons[1]);
    }

    // BFS-parent of every connector
    // -1 represents no parent
    // 12 represents BFS source
    short parent[12] = {
        -1, -1, -1, -1, -1, -1,
        -1, -1, -1, -1, -1, -1
    }; parent[conFrom] = 12;

    list<short> queue;
    queue.push_back(conFrom);

    list<short>::iterator itCon;

    FCCLattice *lattice = static_cast<FCCLattice*>(Catoms3DWorld::getWorld()->lattice);
    
    while(!queue.empty()) {
        short connector = queue.front();
        // cout << connector << " ";
        queue.pop_front();

        // Get all adjacent connectors of dequeued connector.
        // If one of the adjacent connectors has not been visited, mark
        //  it as visited and enqueue it
        for (itCon = adj[connector].begin();
             itCon != adj[connector].end();
             itCon++)
        {
            if (parent[*itCon] == -1) {                
                parent[*itCon] = connector;

                if (*itCon == conTo) {
                    // Found target connector, stop BFS and rebuild path
                    // for (short currentCon = *itCon;
                    //      parent[currentCon] != 12;
                    //      currentCon = parent[currentCon])
                    // {
                        // For now, we simply take the first available link between the two connectors
                    Catoms3DMotionRulesLink* nextMotion =
                        linkMatrix[parent[*itCon]][*itCon].front();
                    assert(nextMotion);

                    Cell3DPosition futurePos =
                        nextMotion->getFinalPosition(&catom);
                    if (!lattice->isPositionBlocked(futurePos, catom.position))
                        return nextMotion;
                    else
                        cout << "POS IS BLOCKED: " << futurePos
                             << " with motion: " << *nextMotion << endl;
                    
                    //     path.push_front(nextMotion);
                    // }
                    

                    // return !path.empty();
                }

                queue.push_back(*itCon);
            }            
        }
    }

    // Could not find direct link from conFrom to conTo, return false
    return NULL;
}                                        

Catoms3DMotionRulesLink*
Catoms3DMotionEngine::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                         short conFrom,
                                         const vector<short>& consTo,
                                         Catoms3DBlock& catom)
{
    if (motionRulesLinks.empty()
        || consTo.empty())
        return NULL;

    Catoms3DMotionRulesLink* shortestLink;
    // Compute path to each connector and return the shortest
    // @attention They are all the shortest now, as we are only considering 1-link rotations
    for (short conTo : consTo) {
        shortestLink =
            Catoms3DMotionEngine::findConnectorsPath(motionRulesLinks, conFrom, conTo, catom);

        if (shortestLink) {
#ifdef DEBUG_API
            cout << "shortestLink:\t" << *shortestLink << endl;
#endif
            return shortestLink;
        }
    }

    return NULL; // might be empty if no path found
}

void
Catoms3DMotionEngine::InitializeConnectorsAndDistances(short tailConnector,
                                                       std::map<short, int>& adjacentConnectors)
{
    adjacentConnectors.insert({tailConnector, 0});
}


bool
Catoms3DMotionEngine::findAdjacentConnectorsAndDistances(const Catoms3DBlock& catom,
                                                         const PathHop& hop,
                                                         short pivotDockingConnector,
                                                         short catomDockingConnector,
                                                         std::map<short, int>& adjacentConnectors)
{
    bool inverted = catom.areOrientationsInverted(hop.getOrientation());
    
    const short *pivotDockingConNeighbors =
        getMotionRules()->getNeighborConnectors(pivotDockingConnector);
    std::set<short> pivotCons; hop.getConnectors(pivotCons);

    FCCLattice* lattice = static_cast<FCCLattice*>(Catoms3DWorld::getWorld()->lattice);
    Catoms3DBlock *pivot = static_cast<Catoms3DBlock*>
        (Catoms3DWorld::getWorld()->lattice->getBlock(hop.getPosition()));

    for (short i = 0; i < 6; i++) {
        short c = pivotDockingConNeighbors[i];
        
        if (pivotCons.count(c)) {
            short oppC = getMotionRules()->
                getMirrorNeighborConnector(catomDockingConnector, (ConnectorDirection)i,
                                           inverted);

            
            Cell3DPosition nPosPivot, nPosCatom;
            pivot->getNeighborPos(c, nPosPivot); catom.getNeighborPos(oppC, nPosCatom);
            
            if (nPosPivot != nPosCatom) { // NOT ADJACENT

#ifdef DEBUG_API
                cout << "findAdjacentConnectorsAndDistances: " << c << " and "
                     << oppC << " are NOT ADJACENT" << endl;
#endif
            } else if (!lattice->isPositionBlocked(nPosCatom, catom.position)) {
                adjacentConnectors.insert({oppC, hop.getDistance(c)});

#ifdef DEBUG_API            
                cout << "findAdjacentConnectorsAndDistances: " << c << " opp is -> "
                     << oppC << " (" << hop.getDistance(c)
                     << ")" << endl;
#endif
            } else {
#ifdef DEBUG_API
                cout << "findAdjacentConnectorsAndDistances: " << c << " opp is -> "
                     << oppC << " => UNREACHABLE!" << endl;
#endif
            }
        }
    }

    return !adjacentConnectors.empty();
}

bool
Catoms3DMotionEngine::findAdjacentConnectors(const Catoms3DBlock& catom,
                            PathHop& hop,
                            short pivotDockingConnector,
                            short catomDockingConnector,
                            std::vector<short>& adjacentConnectors,
                            std::map<short, short>& mirrorConnector)
{
    std::vector<short> inputConnectors;
    hop.getConnectorsByIncreasingDistance(inputConnectors);

    bool inverted = catom.areOrientationsInverted(hop.getOrientation());
    bool lastHopIsSelf = catom.position == hop.getPosition();

    FCCLattice* lattice = static_cast<FCCLattice*>(Catoms3DWorld::getWorld()->lattice);
    const short *pivotDockingConNeighbors =
        getMotionRules()->getNeighborConnectors(pivotDockingConnector);

    for (short c : inputConnectors) {
        auto pdcn_end = pivotDockingConNeighbors + 6;
        auto it = std::find(pivotDockingConNeighbors,
                            pdcn_end,
                            c);
        if (lastHopIsSelf || it != pdcn_end) {
            short idx = std::distance(pivotDockingConNeighbors, it);
            short oppC = getMotionRules()->
                getMirrorNeighborConnector(catomDockingConnector,
                                           (ConnectorDirection)idx,
                                           inverted);
            Cell3DPosition nPosCatom; catom.getNeighborPos(oppC, nPosCatom);

            if (!lattice->isPositionBlocked(nPosCatom, catom.position)) {
                adjacentConnectors.push_back(oppC);            
                mirrorConnector[oppC] = c;

#ifdef DEBUG_API
                cout << "findAdjacentConnectors: " << c << " opp is -> " << oppC
                     << " (" << hop.getDistance(c) << ")" << endl;
#endif
            } else  {
#ifdef DEBUG_API
                cout << "findAdjacentConnectors: " << c << " opp is -> "
                     << oppC << " => UNREACHABLE!" << endl;
#endif
            }
        } 
    }

    return !adjacentConnectors.empty();
}

void Catoms3DMotionEngine::trimPath() {
    for (auto it = path.begin(); it != path.end(); it = std::next(it)) {
        PathHop& someHop = *(it);
 
        // Next next hop is within reach, clear current hop
        if (someHop.isInVicinityOf(catom.position)
            || someHop.catomIsAlreadyOnBestConnector(catom.position)) {
            OUTPUT << "Skipping until: " << *it << endl;
            path.erase(++it, path.end());

            OUTPUT << "Updated path: " << endl;
            for (auto hop:path) OUTPUT << hop << endl;
                            
            break;
        } 
    }
}
