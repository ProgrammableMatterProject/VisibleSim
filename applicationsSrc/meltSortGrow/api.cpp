
#include "api.hpp"
#include "catoms3DWorld.h"

#include <algorithm>

// #define DEBUG_API

bool
API::addModuleToPath(Catoms3DBlock *catom,
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
    PathHop newHop = PathHop(catom->position, catom->orientationCode,
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

bool API::getMotionRulesFromConnector(const Catoms3DBlock *catom,
                                      short conFrom,
                                      vector<Catoms3DMotionRulesLink*>& links)
{
    return getMotionRules()->getValidMotionList(catom, conFrom, links);
}


bool
API::computePathConnectorsAndDistances(const Catoms3DBlock *catom,
                                       std::set<short> consFrom,
                                       std::map<short, int>& distance)
{
    vector<Catoms3DMotionRulesLink*> motionRulesLinks;
    getMotionRules()->getValidSurfaceLinksOnCatom(catom, motionRulesLinks);
    
    for (short conFrom : consFrom) {
        computePathConnectorsAndDistances(motionRulesLinks, catom, conFrom, distance);
    }

    return !distance.empty();
}

bool
API::computePathConnectorsAndDistances(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                       const Catoms3DBlock *catom,
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
    short connector;
    FCCLattice* lattice = static_cast<FCCLattice*>(Catoms3DWorld::getWorld()->lattice);
    
    while(!queue.empty()) {
        connector = queue.front();
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
                Cell3DPosition nPosCatom; catom->getNeighborPos(*itCon, nPosCatom);

                if (!lattice->isPositionBlocked(nPosCatom, catom->position)) {
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
API::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conFrom,
                        short conTo,
                        Catoms3DBlock *catom)
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
    short connector;

    FCCLattice *lattice = static_cast<FCCLattice*>(Catoms3DWorld::getWorld()->lattice);
    
    while(!queue.empty()) {
        connector = queue.front();
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
                        nextMotion->getFinalPosition(catom);
                    if (!lattice->isPositionBlocked(futurePos, catom->position))
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
API::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conFrom,
                        const vector<short>& consTo,
                        Catoms3DBlock *catom)
{
    if (motionRulesLinks.empty()
        || consTo.empty())
        return NULL;

    Catoms3DMotionRulesLink* shortestLink;
    // Compute path to each connector and return the shortest
    // @attention They are all the shortest now, as we are only considering 1-link rotations
    for (short conTo : consTo) {
        shortestLink =
            API::findConnectorsPath(motionRulesLinks, conFrom, conTo, catom);

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
API::InitializeConnectorsAndDistances(short tailConnector,
                                      std::map<short, int>& adjacentConnectors)
{
    adjacentConnectors.insert({tailConnector, 0});
}


bool
API::findAdjacentConnectorsAndDistances(const Catoms3DBlock *catom,
                                        const PathHop& hop,
                                        short pivotDockingConnector,
                                        short catomDockingConnector,
                                        std::map<short, int>& adjacentConnectors)
{
    bool inverted = catom->areOrientationsInverted(hop.getOrientation());
    
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
            pivot->getNeighborPos(c, nPosPivot); catom->getNeighborPos(oppC, nPosCatom);
            
            if (nPosPivot != nPosCatom) { // NOT ADJACENT

#ifdef DEBUG_API
                cout << "findAdjacentConnectorsAndDistances: " << c << " and "
                     << oppC << " are NOT ADJACENT" << endl;
#endif
            } else if (!lattice->isPositionBlocked(nPosCatom, catom->position)) {
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
API::findAdjacentConnectors(const Catoms3DBlock *catom,
                            PathHop& hop,
                            short pivotDockingConnector,
                            short catomDockingConnector,
                            std::vector<short>& adjacentConnectors,
                            std::map<short, short>& mirrorConnector)
{
    std::vector<short> inputConnectors;
    hop.getConnectorsByIncreasingDistance(inputConnectors);

    bool inverted = catom->areOrientationsInverted(hop.getOrientation());
    bool lastHopIsSelf = catom->position == hop.getPosition();

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
            Cell3DPosition nPosCatom; catom->getNeighborPos(oppC, nPosCatom);

            if (!lattice->isPositionBlocked(nPosCatom, catom->position)) {
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



short
API::getConnectorForCell(Catoms3DBlock *catom, const Cell3DPosition cell)
{
    return catom->getConnectorId(cell);
}
