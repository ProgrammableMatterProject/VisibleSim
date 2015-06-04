#ifndef GRID_H
#define GRID_H

#include "coordinate.h"
#include "rectangle.h"

enum state_t {EMPTY = 0, FULL = 1};

class Grid {
 protected:
  int width; // x
  int height; // y

  Rectangle initial;
  Rectangle goal;

  state_t *grid;

  int indice(int x, int y) { return width * y + x;}

 public:
  /*Grid(int w, int h, state_t *g) {
    width = w;
    height = h;
    grid = g;
    }*/
  
  Grid(int w, int h) {
    width = w;
    height = h;
    
    initial = Rectangle(Coordinate(0,0), Coordinate(width/2-1, height-1));
    goal = Rectangle(Coordinate(width/2,height/2), Coordinate(width-1, height-1));
    grid = new state_t[width*height];
    for (int i = 0; i < width*height; i++) {
      grid[i] = EMPTY;
    }
  }
  
  ~Grid() { delete[] grid;}

  int getWidth() { return width; }
  int getHeight() { return height; }

  state_t get(int x, int y) {
    return grid[indice(x,y)];
  } 

  Rectangle& getInitialArea() {
    return initial;
  }
  
  Rectangle& getGoalArea() {
    return goal;
  }

  void set(int x, int y, state_t s) {
    grid[indice(x,y)] = s;
  }
};

#endif
