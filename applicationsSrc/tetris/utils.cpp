#include <vector>

class CoordsData
{
public:
    int stage;
    int height;
    int width;
    unsigned int nbTree;

    CoordsData(int st, int h, int w, unsigned int t)
    {
        stage = st;
        height = h;
        width = w;
        nbTree = t;
    }
};

class BackCoords
{
public:
    int stage;
    unsigned int spanTree;

    BackCoords(int st, unsigned int spT)
    {
        stage = st;
        spanTree = spT;
    }
};

class IntData
{
public:
    int stage;
    int data;

    IntData(int st, int i)
    {
        stage = st;
        data = i;
    }
};

class NewTmnData
{
public:
    int stage;
    int numbTmn;

    NewTmnData(int st, int nbTmn)
    {
        stage = st;
        numbTmn = nbTmn;
    }
};

class TmnData
{
public:
    int stage;
    int nbupdate; //number of the update (to prevent infinite loops)
    int rotation;
    int position;
    int color;
    int nbReinit;
    int nbFree;

    bool goingR = false;
    bool goingL = false;
    bool rotCW = false;
    bool rotCCW = false;

    TmnData(int st, int upd, int r, int p, int c, int reinit, int nbf)
    {
        stage = st;
        nbupdate = upd;
        rotation = r;
        position = p;
        color = c;
        nbReinit = reinit;
        nbFree = nbf;
    }
    TmnData(int st, int upd, int r, int p, int c, int reinit, int nbf, bool gRight, bool gLeft, bool rCK, bool rCCK)
    {
        stage = st;
        nbupdate = upd;
        rotation = r;
        position = p;
        color = c;
        nbReinit = reinit;
        nbFree = nbf;
        goingR = gRight;
        goingL = gLeft;
        rotCW = rCK;
        rotCCW = rCCK;
    }
};

class TmnBackData
{
public:
    int stage;
    int update;

    TmnBackData(int st, int upd)
    {
        stage = st;
        update = upd;
    }
};

class ReinitData
{
public:
    int stage;
    int id;
    int tmn;
    int movement;

    ReinitData(int st, int nbReinit, int t, int mvt)
    {
        stage = st;
        id = nbReinit;
        tmn = t;
        movement = mvt;
    }
};

class ReinitBackData
{
public:
    int stage;
    int nbReinit;

    ReinitBackData(int st, int nbRei)
    {
        stage = st;
        nbReinit = nbRei;
    }
};

class isFreeData
{
public:
    int stage;
    int id;
    int position;
    int direction;
    int answer;

    isFreeData(int st, int i, int p, int d)
    {
        stage = st;
        id = i;
        position = p;
        direction = d;
        answer = 0; // = NO_ANSWER
    }
    isFreeData(int st, int i, int p, int d, int a)
    {
        stage = st;
        id = i;
        position = p;
        direction = d;
        answer = a;
    }
};

class BackFreeData
{
public:
    int stage;
    bool answer;

    BackFreeData(int st, bool ans)
    {
        stage = st;
        answer = ans;
    }
};

class freeAnswer
{
public:
    int stage;
    int direction; //asked direction and position
    int position;

    freeAnswer(int st, int p, int d)
    {
        stage = st;
        direction = d;
        position = p;
    }
};

class farVerif
{
public:
    int stage;
    int id;
    int answer = 0; //=NO_ANSWER
    unsigned int current_dir;
    std::vector<int> directions;
    int rotation;

    farVerif(int st, int i, int c_dir, std::vector<int> dirs, int rot)
    {
        stage = st;
        id = i;
        current_dir = c_dir;
        directions = dirs;
        rotation = rot;
    }

    farVerif()
    {
    }
};

class TmnInfo
{
public:
    int stage;
    int tmn;
    int rotation;
    int position;
    int color;
    bool bckd;
    int bckdR;
    int bckdL;

    TmnInfo(int st, int tmn, int rot, int pos, int color, bool blocked, int blockedRight, int blockedLeft)
    {
        stage = st;
        this->tmn = tmn;
        this->rotation = rot;
        this->position = pos;
        this->color = color;
        bckd = blocked;
        bckdR = blockedRight;
        bckdL = blockedLeft;
    }
};

class BlockedData
{
public:
    int stage;
    int i;
    int n;

    BlockedData(int st, int id, int nb)
    {
        stage = st;
        i = id;
        n = nb;
    }
};

class CountBlockedData
{
public:
    int stage;
    int count;

    CountBlockedData(int st, int c)
    {
        stage = st;
        count = c;
    }
};

class Splitdata
{
public:
    int stage;
    int direction;

    Splitdata(int st, int dir)
    {
        stage = st;
        direction = dir;
    }
};

/*
stringstream strstm;
strstm << "Role is BL corner";
scheduler->trace(strstm.str(), module->blockId, GREEN);
*/