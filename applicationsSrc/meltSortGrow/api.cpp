
#include "api.hpp"
#include "catoms3DWorld.h"

#include <algorithm>

bool
API::addModuleToPath(Catoms3DBlock *catom,
                     vector<PathHop>& path,
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

    // cout << "addModuleToPath: " << "newHop: " << newHop << endl;
    path.push_back(newHop);

    for (PathHop& hop : path) cout << hop << endl;
    cout << endl;
    
    return true;
}

bool
API::buildRotationSequenceToTarget(Catoms3DBlock *pivot,
                                   short pivotCon,
                                   vector<PathHop>& path,                                   
                                   list<Catoms3DMotionRulesLink*>& rotations)
{
    if (path.empty() || pivot == NULL) return false;

    vector<Catoms3DMotionRulesLink*> startMotionRules;
    // getMotionRules()->getValidRotationsListForCatom(bc->catom, startMotionRules);
    // getMotionRules()->getValidMotionListFromPivot()
    
    // The algorithm is simple, all we have to do is find
    //  a sequence of motion from the catom to the connector of the neighbor
    //  with the minimum distance to the target, and then unfold the hop list
    for (auto &hop : path) {
        vector<short> orderedCons;
        hop.getConnectorsByIncreasingDistance(orderedCons);
        assert(!orderedCons.empty());
        
    }

   throw NotImplementedException();
    
    return !rotations.empty();
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
        computePathConnectorsAndDistances(motionRulesLinks, conFrom, distance);
    }

    return !distance.empty();
}

bool
API::computePathConnectorsAndDistances(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                                       short conFrom,
                                       std::map<short, int>& distance)
{
    if (motionRulesLinks.empty()) return false;
    
    // Build an adjacency matrix out of the motion rules link for easier graph traversal
    list<Catoms3DMotionRulesLink*> linkMatrix[12][12];
    std::array<list<short>, 12> adj;
    for (const auto& link : motionRulesLinks) {
        std::array<short, 2> linkCons = link->getConnectors();

        // cout << *link << endl;
        linkMatrix[linkCons[0]][linkCons[1]].push_back(link);
        // linkMatrix[linkCons[1]][linkCons[0]].push_back(link);

        adj[linkCons[0]].push_back(linkCons[1]);
        // adj[linkCons[1]].push_back(linkCons[0]);
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

                int depth = distance[parent[*itCon]] + 1;
                if (distance.find(*itCon) == distance.end()
                    || distance[*itCon] > depth) {
                    
                    distance[*itCon] = depth;
                    // cout << "inininin" << endl;
                } else {
                    // cout << "outoutoutout" << endl;
                }

                queue.push_back(*itCon);
            }
        }
    }

    return !distance.empty();
}


bool
API::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conFrom,
                        short conTo,
                        list<Catoms3DMotionRulesLink*>& path)
{
    if (motionRulesLinks.empty()) return false;

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
                    for (short currentCon = *itCon;
                         parent[currentCon] != 12;
                         currentCon = parent[currentCon])
                    {
                        // For now, we simply take the first available link between the two connectors
                        Catoms3DMotionRulesLink* nextMotion =
                            linkMatrix[parent[currentCon]][currentCon].front();
                        
                        path.push_front(nextMotion);
                    }
                    

                    return !path.empty();
                }

                queue.push_back(*itCon);
            }            
        }
    }

    // Could not find direct link from conFrom to conTo, return false
    return !path.empty();
}                                        

bool
API::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conFrom,
                        const vector<short>& consTo,
                        list<Catoms3DMotionRulesLink*>& shortestPath)
{
    if (motionRulesLinks.empty()
        || consTo.empty())
        return false;

    // Compute path to each connector and return the shortest
    for (short conTo : consTo) {
        list<Catoms3DMotionRulesLink*> candidate;
        API::findConnectorsPath(motionRulesLinks, conFrom, conTo, candidate);

        cout << conFrom << " to " << conTo << ":" << endl << "\t";
        for (Catoms3DMotionRulesLink* step : candidate) {
            cout << " " << *step << " ->";
        }
        cout << "|" << endl;
        
        if (!candidate.empty() &&
            (shortestPath.empty() || candidate.size() < shortestPath.size()) )
            shortestPath = candidate;
    }
    
    cout << "ShortestPath: " << endl << "\t";
    for (auto const& step : shortestPath) {
        cout << " " << *step << " ->";
    }
    cout << "|" << endl;

    return !shortestPath.empty(); // might be empty if no path found
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

                cout << "findAdjacentConnectorsAndDistances: " << c << " and "
                     << oppC << " are NOT ADJACENT" << endl;                
            } else {                
                adjacentConnectors.insert({oppC, hop.getDistance(c)});
            
                cout << "findAdjacentConnectorsAndDistances: " << c << " opp is -> "
                     << oppC << " (" << hop.getDistance(c)
                     << ")" << endl;
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

            adjacentConnectors.push_back(oppC);            
            mirrorConnector[oppC] = c;

            cout << "findAdjacentConnectors: " << c << " opp is -> " << oppC
                 << " (" << hop.getDistance(c) << ")" << endl;
            
        } 
    }

    return !adjacentConnectors.empty();
}



short
API::getConnectorForCell(Catoms3DBlock *catom, const Cell3DPosition cell)
{
    return catom->getConnectorId(cell);
}
