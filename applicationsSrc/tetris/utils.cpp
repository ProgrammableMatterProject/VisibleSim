class CoordsData
{
public:
    int height;
    int width;
    unsigned int nbTree;

    CoordsData(int h, int w, unsigned int t)
    {
        height = h;
        width = w;
        nbTree = t;
    }
};

class TmnData
{
public:
    int nbupdate; //number of the update (to prevent infinite loops)
    int rotation;
    int position;
    int color;

    TmnData(int upd, int r, int p, int c)
    {
        nbupdate = upd;
        rotation = r;
        position = p;
        color = c;
    }
};

class ReinitData
{
public:
    int id;
    int tmn;
    int movement;

    ReinitData(int nbReinit,int t, int mvt)
    {
        id = nbReinit;
        tmn = t;
        movement = mvt;
    }
};

/*
stringstream strstm;
strstm << "Role is BL corner";
scheduler->trace(strstm.str(), module->blockId, GREEN);
*/