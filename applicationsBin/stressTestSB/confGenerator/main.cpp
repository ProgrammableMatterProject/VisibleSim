#include <iostream>
#include <cstdlib>
#include <cassert>
#include <random>
#include <list>

using namespace std;

class Grid {
    int width;
    int height;
    bool *grid;

    inline void set(int ix, int iy, bool v=true) {
        assert(ix>=0 && ix<width && iy>=0 && iy<height);
        grid[ix+iy*width]=v;
    }
    inline bool get(int ix, int iy) {
        assert(ix>=0 && ix<width && iy>=0 && iy<height);
        return grid[ix+iy*width];
    }
public :
    Grid(int p_width,int p_height):width(p_width),height(p_height) {
        grid = new bool [width*height];
        // init to 0
        bool *ptr=grid;
        int n=width*height;
        while (n--) { *ptr++=false; }
    };
    ~Grid() {
        delete [] grid;
    }

    void add(int x,int y) {
        set(x,y);
    }
    void addFreeNeighbors(int x,int y,list<pair<int ,int>> &res) {
        if (x<width-1 and (not get(x+1,y)) and neighborCount(x+1,y)<=12) {
            set(x+1,y,true);
            res.push_back(make_pair(x+1,y));
        }
        // right
        if (x>0 and (not get(x-1,y)) and neighborCount(x-1,y)<=12) {
            set(x-1,y);
            res.push_back(make_pair(x-1,y));
        }
        // top
        if (y<height-1 and (not get(x,y+1)) and neighborCount(x,y+1)<=12) {
            set(x,y+1);
            res.push_back(make_pair(x,y+1));
        }
        // bottom
        if (y>0 and (not get(x,y-1)) and neighborCount(x,y-1)<=13) {
            set(x,y-1);
            res.push_back(make_pair(x,y-1));
        }

    }
    int neighborCount(int x,int y) {
        int n=0;
        if (x<width-1 and get(x+1,y)) n+=10;
        if (x>0 and get(x-1,y)) n+=10;
        if (y<height-1 and get(x,y+1)) n+=10;
        if (y>0 and get(x,y-1)) n+=10;
        if (x<width-1 and y<height-1 and get(x+1,y+1)) n++;
        if (x>0 and y<height-1 and get(x-1,y+1)) n++;
        if (x<width-1 and y>0 and get(x+1,y-1)) n++;
        if (x>0 and y>0 and get(x-1,y-1)) n++;
        return n;
    }

    void print() {
        cout << "<?xml version=\"1.0\" standalone=\"no\" ?>" << endl;
        cout << "<world gridSize=\"" << width << "," << height << ",1\">" << endl;
        cout << "\t<blockList color=\"0,255,0\" blockSize=\"25.0,25.0,11.0\">" << endl;
        for (int l=0; l<height; l++) {
            cout << "\t\t<blocksLine line=\"" << l << "\" values=\"";
            for (int c=0; c<width; c++) {
                cout << (int)get(c,l);
            }
            cout << "\"/>" << endl;
        }

        cout << "\t</blockList>" << endl;
        cout << "</world>" << endl;

        // count number of true
        bool *ptr=grid;
        int n=width*height;
        int nbok=0;
        while (n--) {
            nbok+=(int)(*ptr++);
        }
        cerr << "Nb modules=" << nbok << "/" << width*height << endl;
    }

};

int main(int argc, char **argv) {
    if (argc<4) {
        cerr << "usage: confGenerator width height nbSources [seed value]" << endl;
        return 1;
    }

    const int gridWidth=atoi(argv[1]),gridHeight=atoi(argv[2]),nbSources=atoi(argv[3]);
    cerr << "Generate grid " << gridWidth << " x " << gridHeight << ", #src=" << nbSources << endl;

    Grid grid(gridWidth,gridHeight);

    // create seeds
    list<pair<int ,int>> res;

    mt19937 rng;
    int randSeed=(argc==5)?atoi(argv[4]):1;
    int x,y;
    rng.seed(randSeed);
    for (int i=0; i<nbSources; i++) {
        x = rng()%gridWidth;
        y = rng()%gridHeight;
        cerr << "init: "<< x << "," << y << endl;
        grid.add(x,y);
        grid.addFreeNeighbors(x,y,res);
    }

    pair<int,int> pos;
    while (not res.empty()) {
        pos = res.front();
        res.pop_front();
        grid.addFreeNeighbors(pos.first,pos.second,res);
    }
    grid.print();


    return 0;
}
