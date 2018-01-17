
#include "api.hpp"

vector<Catoms3DMotionRulesLink*>
API::getAllLinksToConnector(const Catoms3DBlock *pivot,
                            short conId)
{
    vector<Catoms3DMotionRulesLink*> links;
    Catoms3DMotionRules().getValidMotionList(pivot, (int)conId, links);
    
    return links;
}

vector<Catoms3DMotionRulesLink*>
API::getAllLinks(const Catoms3DBlock *pivot)
{
    vector<Catoms3DMotionRulesLink*> links;
    for (short conId = 0; conId < 12; conId++) {
        Catoms3DMotionRules().getValidMotionList(pivot, (int)conId, links);
    }

    return links;
}

list<Catoms3DMotionRulesLink*>
API::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conFrom,
                        short conTo)
{
    if (motionRulesLinks.empty()) return list<Catoms3DMotionRulesLink*>();

    // Build an adjacency matrix out of the motion rules link for easier graph traversal
    list<Catoms3DMotionRulesLink*> linkMatrix[12][12];
    std::array<list<short>, 12> adj;
    for (const auto& link : motionRulesLinks) {
        std::array<short, 2> linkCons = link->getConnectors();

        linkMatrix[linkCons[0]][linkCons[1]].push_back(link);
        linkMatrix[linkCons[1]][linkCons[0]].push_back(link);

        adj[linkCons[0]].push_back(linkCons[1]);
        adj[linkCons[1]].push_back(linkCons[0]);
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
    list<Catoms3DMotionRulesLink*> path;
    
    while(!queue.empty()) {
        connector = queue.front();
        cout << connector << " ";
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

                        return path;
                    }
                }

                queue.push_back(*itCon);
            }            
        }
    }

    // Could not find direct link from conFrom to conTo, return empty list
    return path;
}


list<Catoms3DMotionRulesLink*>
API::findConnectorsPath(const vector<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conFrom,
                        set<short> consTo)
{
    if (motionRulesLinks.empty()
        || consTo.empty())
        return list<Catoms3DMotionRulesLink*>();

    // Compute path to each connector and return the shortest
    list<Catoms3DMotionRulesLink*> shortestPath;
    for (short conTo : consTo) {
        list<Catoms3DMotionRulesLink*> candidate =
            API::findConnectorsPath(motionRulesLinks, conFrom, conTo);

        if (shortestPath.empty()
            || candidate.size() < shortestPath.size())
            shortestPath = candidate;
    }

    return shortestPath; // might be empty if no path found
}

set<short>
API::findPathConnectors(const set<Catoms3DMotionRulesLink*>& motionRulesLinks,
                        short conTo)
{
    throw NotImplementedException();
}

set<short>
API::findPathConnectors(const Catoms3DBlock *pivot, short conTo)
{
    throw NotImplementedException();
}

set<short>
API::findPathConnectors(const Catoms3DBlock *pivot, set<short> consTo)
{
    throw NotImplementedException();
}

set<short>
API::findAdjacentConnectors(const set<short>& pathConnectors,
                            short pathOrientationCode,
                            short orientationCode)
{
    throw NotImplementedException();
}


// NOT MSG SPECIFIC

set<short>
API::getConnectorsAdjacentToCell(Catoms3DBlock *catom,
                                 const Cell3DPosition cell)
{
    throw NotImplementedException();
}

short
API::getConnectorForCell(Catoms3DBlock *catom, const Cell3DPosition cell)
{
    return catom->getConnectorId(cell);
}
