#ifndef CENTRALIZED_BFS
#define CENTRALIZED_BFS

#include <cstdlib>
#include <algorithm>

class TreeNode {
 public:
  int id;
  TreeNode* children[6];
  int branchLength[6];
  int height;
  int dist;
  int upperBound;
		
  TreeNode(int i) {
    id = i;
    height = -1;
    dist = 0;
    for (int j = 0; j < 6; j++) {
      children[j] = NULL;
      branchLength[j] = 0;
    }
  }

  bool isALeaf() {
    for (int i = 0; i < 6; i++) {
      if (children[i]) {
	return false;
      }
    }
    return true;
  }
	
  int getHeight() {
			
    if (height != -1) {
      return height;
    } else {
      height = 0;
    }
			
    if(isALeaf()){
      return 0;
    } else {
      //cout << "height computation" << endl;
      for (int i = 0; i < 6; i++) {
	if (children[i]) {
	  branchLength[i] = children[i]->getHeight()+1;
          height = std::max(height,branchLength[i]);
	}
      }
      return height;
    }
  }
		
  void setUpperBound(int h) {
     upperBound = std::max(h,height);
    //upperBound = max(upperBound,height);
			
    for (int i = 0; i < 6; i++) {
      if (children[i]) {
	int maxBranch = 0;
	for(int j = 0; j < 6; j++) {
	  if (i!=j && children[j]) {
             maxBranch = std::max(maxBranch, branchLength[j]);
	  }
	}
        children[i]->setUpperBound(std::max(maxBranch+1,h+1));
      }
    }
  }
};

class Tree {
 public:
  TreeNode *root;
		
  Tree(int r) {
    root = new TreeNode(r);
  }
		
  int getHeight() {
    return root->getHeight();
  }
		
  void setUpperBound() {
    root->setUpperBound(0);
  }
	
  static Tree* bfs(int source, int dist[], int size);
	
};

#endif
