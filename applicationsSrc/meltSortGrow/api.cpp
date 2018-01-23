
#include "api.hpp"
#include "catoms3DWorld.h"

bool
API::addModuleToPath(Catoms3DBlock *catom, list<PathHop>& path)
{
    if (path.empty())
        return false;
    
    // List all connectors that could be filled in order to connect
    //  a neighbor module to the last hop
    PathHop lastHop = path.back();
    set<short> adjacentConnectors;
    findAdjacentConnectors(lastHop,
                           lastHop.getOrientation(), // actually should be P2P_NI
                           catom->orientationCode,
                           adjacentConnectors);

    if (adjacentConnectors.empty()) return false;
    // \todo

    throw NotImplementedException();
}

bool
API::buildRotationSequenceToTarget(Catoms3DBlock *pivot,
                                   short pivotCon,
                                   list<PathHop>& path,                                   
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
}

bool
API::getAllLinksToConnector(const Catoms3DBlock *pivot,
                            short conId,
                            vector<Catoms3DMotionRulesLink*>&links)
{
    getMotionRules()->getValidMotionList(pivot, (int)conId, links);
    
    return !links.empty();
}

bool
API::getAllLinks(const Catoms3DBlock *pivot,
                 vector<Catoms3DMotionRulesLink*>&links)
{
    for (short conId = 0; conId < 12; conId++) {
        // #TODO fixit
        // Pb: ne fournit pas les routes pour les interfaces déjà connectées
        getMotionRules()->getValidMotionList(pivot, (int)conId, links);
    }

    return !links.empty();
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
    for (const auto& link : motionRulesLinks) {
        std::array<short, 2> linkCons = link->getConnectors();

        cout << *link << endl;
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
    };

    list<short> queue;

    parent[conFrom] = 12;
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
                            linkMatrix[currentCon][parent[currentCon]].front();
                        
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

        if (!candidate.empty() &&
            (shortestPath.empty() || candidate.size() < shortestPath.size()) )
            shortestPath = candidate;
    }

    return !shortestPath.empty(); // might be empty if no path found
}

bool
API::findPathConnectors(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conTo,
                        set<short>& pathConnectors)
{
    for (auto const& mrl : motionRulesLinks) {
        if (mrl->getConToID() == conTo)
            pathConnectors.insert(mrl->getConFromID());
    }

    return !pathConnectors.empty(); // might be empty if no connectors
}

bool
API::findPathConnectors(const Catoms3DBlock *pivot,
                        short conTo,
                        set<short>& pathConnectors)
{
    vector<Catoms3DMotionRulesLink*> links;
    getAllLinks(pivot, links);

    return findPathConnectors(links, conTo, pathConnectors);
}

bool
API::findPathConnectors(const Catoms3DBlock *pivot,
                        const set<short>& consTo,
                        set<short>& pathConnectors)
{
    vector<Catoms3DMotionRulesLink*> links;
    getAllLinks(pivot, links);

    for (short conTo : consTo)
        findPathConnectors(links, conTo, pathConnectors);

    return !pathConnectors.empty();
}

bool
API::findAdjacentConnectors(const PathHop hop,
                            short pathOrientationCode,
                            short orientationCode,
                            set<short>& adjacentConnectors)
{
    throw NotImplementedException();
}


// NOT MSG SPECIFIC

bool
API::getConnectorsAdjacentToCell(Catoms3DBlock *catom,
                                 const Cell3DPosition cell,
                                 set<short>& pathConnectors)
{
    throw NotImplementedException();
}

short
API::getConnectorForCell(Catoms3DBlock *catom, const Cell3DPosition cell)
{
    return catom->getConnectorId(cell);
}
