#include <iostream>
#include <cstdlib>
#include <cassert>
#include <random>
#include <list>
#include <vector>
#include <algorithm>

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
        vector<pair<int,int>> solutions;
        if (x<width-1 and (not get(x+1,y)) and neighborCount(x+1,y)<=12) {
            set(x+1,y,true);
            solutions.emplace_back(x+1,y);
        }
        // right
        if (x>0 and (not get(x-1,y)) and neighborCount(x-1,y)<=12) {
            set(x-1,y);
            solutions.emplace_back(x-1,y);
        }
        // top
        if (y<height-1 and (not get(x,y+1)) and neighborCount(x,y+1)<=12) {
            set(x,y+1);
            solutions.emplace_back(x,y+1);
        }
        // bottom
        if (y>0 and (not get(x,y-1)) and neighborCount(x,y-1)<=13) {
            set(x,y-1);
            solutions.emplace_back(x,y-1);
        }
        // random shuffle
        random_shuffle ( solutions.begin(), solutions.end() );

        copy(solutions.begin(), solutions.end(),back_inserter(res));
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

    void print(int parts) {
        // count number of true
        bool *ptr=grid;
        int n=width*height;
        int nbok=0;
        while (n--) {
            nbok+=(int)(*ptr++);
        }
        cerr << "Nb modules=" << nbok << "/" << width*height << endl;

        cout << "<?xml version=\"1.0\" standalone=\"no\" ?>" << endl;
        cout << "<world gridSize=\"" << width << "," << height << ",1\" windowSize=\"1920,1080\" >" << endl;
        cout << "<camera target=\"" << width*12.5f << "," << height*12.5f << ",10\" directionSpherical=\"0,60," << width*40.0f << "\" angle=\"45\" near=\"" << width*20.0f << "\" far=\"" << width*50.0f << "\" />" << endl;
        cout << "\t<blockList color=\"0,255,0\" blockSize=\"25.0,25.0,11.0\">" << endl;
        int wp=width/parts;
        int hp=height/parts;
        int yp=hp/2;
        int l=0;
        for (int k=0; k<parts+1; k++) {
            while (l < yp) {
                cout << "\t\t<blocksLine line=\"" << l << "\" values=\"";
                for (int c = 0; c < width; c++) {
                    cout << (int) get(c, l);
                }
                cout << "\"/>" << endl;
                l++;
            }
            if (l<height) {
                int xp=wp/2;
                for (int c = 0; c < width; c++) {
                    if (get(c, l)) {
                        if (xp>0) {
                            cout << "\t\t<block position=\"" << c << "," << l << "," << "0\" />" << endl;
                        } else {
                            xp+=wp;
                            cout << "\t\t<block position=\"" << c << "," << l << "," << "0\" leader=\"1\" />" << endl;
                        }
                    }
                    xp--;
                }
            }
            l++;
            yp = min(yp+hp+1,height);
        }
        cout << "\t</blockList>" << endl;
        cout << "</world>" << endl;
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
    int randSeed=(argc>=5)?atoi(argv[4]):1;
    int parts=(argc==6)?atoi(argv[5]):1;
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
    grid.print(parts);


    return 0;
}
