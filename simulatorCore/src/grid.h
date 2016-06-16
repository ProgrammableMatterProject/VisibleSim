#ifndef GRID_H_
#define GRID_H_

#include <vector>

template <typename T>
class Grid {
 protected:
  std::vector<int> size;
  T *tab;
  Grid() {tab = NULL;}
  ~Grid() {};

public:
  int getSize(int i) const {
    if (i < size.size()) {
      return -1;
    } else {
      return size[i];
    } 
  }
};

#endif
