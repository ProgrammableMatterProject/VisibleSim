#include "bfs.h"
#include "base/buildingBlock.h"
#include <cstdlib>
#include "robots/catoms2D/catoms2DBlock.h"
#include "robots/catoms2D/catoms2DWorld.h"
#include "comm/network.h"

using namespace std;
using namespace Catoms2D;

Tree* Tree::bfs(int source, int dist[], int size) {
  Catoms2DWorld *world = Catoms2D::getWorld();
  // Mark all the vertices as not visited
  bool *visited = new bool[size];
  Tree *tree = new Tree(source);

  Catoms2DBlock *b = NULL;
  for(int i = 0; i < size; i++) {
    visited[i] = false;
  }

  dist[source] = 0;

  // Create a queue for BFS
  list<TreeNode *> queue;

  // Mark the current node as visited and enqueue it
  visited[source] = true;
  queue.push_back(tree->root);

  // 'i' will be used to get all adjacent vertices of a vertex
  list<int>::iterator i;

  TreeNode *s = NULL;
  while(!queue.empty())
    {
      // Dequeue a vertex from queue and print it
      s = queue.front();
      //cout << s << " ";
      queue.pop_front();

      b = world->getBlockById(s->id);

      for (int i = 0; i < 6; i++) {
    if (b->getInterface(i)->connectedInterface != NULL) {
      int id = b->getInterface(i)->connectedInterface->hostBlock->blockId;
      if (!visited[id]) {
        visited[id] = true;
        dist[id] = dist[s->id] + 1;
        s->children[i] = new TreeNode(id);
        s->children[i]->dist = dist[id];
        queue.push_back(s->children[i]);
      }
    }
      }
    }
  return tree;
}
