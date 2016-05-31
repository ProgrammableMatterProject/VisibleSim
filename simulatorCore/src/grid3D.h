#ifndef GRID3D_H_
#define GRID3D_H_

#include "grid.h"

template<typename T>
class Grid3D: public Grid<T> {
 protected: 
  int index(int x, int y, int z) {
    return x+(y+z*this->size[1])*this->size[0];
  }

 public:
 Grid3D(int x, int y, int z, T v): Grid<T>() {
    
    this->size.push_back(x);
    this->size.push_back(y);
    this->size.push_back(z);
    
    this->tab = new T[x*y*z];
    set(v);
  }
  
  ~Grid3D() {};
  
  T get(int x, int y, int z) {
    return this->tab[index(x,y,z)];
  }
  
  void set(T v) {
    int x = 0, y = 0, z = 0;
    for (int z = 0; z < this->size[2]; z++) {
      for (int y = 0; y < this->size[1]; y++) {
	for (int x = 0; x < this->size[0]; x++) {
	  this->tab[index(x,y,z)] = v;
	}
      }
    }
  }
  
  void set(int x, int y, int z, T v) {
    this->tab[index(x,y,z)] = v;
  }
  
};

#endif
