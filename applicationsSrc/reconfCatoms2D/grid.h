#ifndef GRID_H
#define GRID_H

enum state_t {FULL = 0,EMPTY};

class Grid {
 protected:
  int width; // x
  int height; // y
  state_t *grid;

  int indice(int x, int y) { return width * y + x;}

 public:
  Grid(int w, int h, state_t *g) {
    width = w;
    height = h;
    grid = g;
  }
  
  Grid(int w, int h) {
    width = w;
    height = h;
    grid = new state_t[w*h];    
  }
  
  ~Grid() {}

  int getWidth() { return width; }
  int getHeight() { return height; }

  state_t get(int x, int y) {
    return grid[indice(x,y)];
  } 
  
  void set(int x, int y, state_t s) {
    grid[indice(x,y)] = s;
  }
};

#endif
