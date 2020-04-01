#include "smartBlocksCapabilities.h"
#include <algorithm>

const bool truthTable[9][5]={ {1,0,0,0,0}, {0,0,0,0,0}, {0,0,0,0,0}, {0,1,0,0,0}, {0,0,0,0,1}, {1,0,0,0,0}, {0,0,0,1,0}, {0,0,0,1,1}, {1,1,1,1,1} };
const bool truthTable0[9][5]={{1,0,0,0,0}, {0,0,0,0,0}, {0,0,0,0,0}, {0,1,0,0,0}, {0,0,0,0,0}, {1,0,0,0,0}, {0,1,0,0,0}, {0,1,0,0,0}, {1,1,1,1,0} };
const short gainTable[9][4]={ {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,0,0,0}, {0,1,0,0}, {1,0,0,0}, {0,0,0,0}, {0,0,0,0} };
const short rotDir[4][4]={ {0,1,2,3},{1,2,3,0},{2,3,0,1},{3,0,1,2} };

Capability::Capability(const string& sid) {

    name = sid;
    OUTPUT << "New capability : " << name << endl;
    linkNextPos=linkPrevPos=NULL;
    singleMotionDir=NULL;
    isHead = false;
    isEnd = false;
    prevDir.set(0,0);
}

Capability::Capability(const string& sid,const Capability*ref,int n):name(sid) {
    OUTPUT << "new capability  : " << name << endl;

    linkNextPos=linkPrevPos=NULL;
    isHead = ref->isHead;
    isEnd = ref->isEnd;
    isAngle = ref->isAngle;
    singleMotionDir=NULL;
    switch (n) {
        case 1 : case 2 : case 3 : { // rotation around Z axis
            ValidationMatrix tmp,tmp2;
            PointCel tmppt;

            validationMatrix.fromRotationZ(ref->validationMatrix,n);
            if (ref->linkPrevPos) {
                linkPrevPos = new PointCel(ref->linkPrevPos->rotateZ(n));
            } else {
                linkPrevPos = NULL;
            }
            if (ref->linkNextPos) {
                linkNextPos = new PointCel(ref->linkNextPos->rotateZ(n));
            } else {
                linkNextPos = NULL;
            }
            prevDir = ref->prevDir.rotateZ(n);
            if (ref->singleMotionDir) {
                singleMotionDir = new PointCel(ref->singleMotionDir->rotateZ(n));
            } else {
                singleMotionDir = NULL;
            }
            // copy the tabMotion vector with rotation
            vector <Motion*>::const_iterator cm = ref->tabMotions.begin();
            Motion *nm;
            while (cm!=ref->tabMotions.end()) {
                nm = new Motion(*cm);
                nm->vect = (*cm)->vect.rotateZ(n);
                // objectPath
                vector <short>::const_iterator cs = (*cm)->PathToBlock.begin();
                nm->PathToBlock.clear();
                while (cs!=(*cm)->PathToBlock.end()) {
                    nm->PathToBlock.push_back(rotDir[n][*cs]);
                    cs++;
                }
                // unlockpath
                cs = (*cm)->UnlockPath.begin();
                nm->UnlockPath.clear();
                while (cs!=(*cm)->UnlockPath.end()) {
                    nm->UnlockPath.push_back(rotDir[n][*cs]);
                    cs++;
                }

                tabMotions.push_back(nm);
                cm++;
            }

            // copy the tabUnlockPath with rotation
            vector <short>::const_iterator cs = ref->tabUnlockPath.begin();
            while (cs!=ref->tabUnlockPath.end()) {
                tabUnlockPath.push_back(rotDir[n][*cs]);
                cs++;
            }
        }
        break;
    }
}

Capability::~Capability() {
    vector <Motion *>::const_iterator ci = tabMotions.begin();
    while (ci!=tabMotions.end()) {
        delete (*ci);
        ci++;
    }
    tabMotions.clear();
    delete linkPrevPos;
    delete linkNextPos;
}

// write capability parameters into a flux
ostream& operator<<(ostream& f,const Capability &c) {
    f << "Capability:" << c.name << endl;
    f << "States condition: " << endl;
    f << c.validationMatrix;
    if (c.linkNextPos!=NULL) f << "next: " << *(c.linkNextPos) << endl;
    if (c.linkPrevPos!=NULL) f << "prev: " << *(c.linkPrevPos) << endl;
    if (c.singleMotionDir!=NULL) f << "sMD: " << *(c.singleMotionDir) << endl;
  return f;
}

/*************************************BPi*************************************/
/* Capabilities                                                              */
SmartBlocksCapabilities::SmartBlocksCapabilities(TiXmlNode *node) {

    TiXmlElement *capaElem = node->FirstChildElement("capability");
    Capability *currentCapa=NULL;
    TiXmlElement *matrixElem;
    PointCel pos;
    OUTPUT << "begin capabilities" << endl;
    while (capaElem) {
        currentCapa = new Capability(capaElem->Attribute("name"));

        const char *str_c = capaElem->Attribute("head");
        if (!str_c) {
            currentCapa->isHead = false;
        } else {
            currentCapa->isHead = (bool)(atoi(str_c));
        }

        str_c = capaElem->Attribute("end");
        if (!str_c) {
            currentCapa->isEnd = false;
        } else {
            currentCapa->isEnd = (bool)(atoi(str_c));
        }

        matrixElem = capaElem->FirstChildElement("states");
        if (!matrixElem) { cerr << "erreur <states>" << endl; return; };
        currentCapa->validationMatrix.fromString(matrixElem->GetText());

        matrixElem = capaElem->FirstChildElement("this");
        if (!matrixElem) {
            cerr << "erreur <this>" << endl;
            return;
        } else {
            str_c = matrixElem->Attribute("linkPrevPos");
            if (str_c) {
                currentCapa->linkPrevPos=new PointCel(str_c);
            } else {
                currentCapa->linkPrevPos=NULL;
            }
            str_c = matrixElem->Attribute("linkPrevDir");
            if (str_c) {
                currentCapa->prevDir.set(str_c);
                currentCapa->prevDir.x+=1;
                currentCapa->prevDir.y+=1;
//				cout << "prevDir =" << currentCapa->prevDir << endl;
            }
            str_c = matrixElem->Attribute("singleMotionDir");
            if (str_c) {
                currentCapa->singleMotionDir = new PointCel(str_c);
                currentCapa->singleMotionDir->x+=1;
                currentCapa->singleMotionDir->y+=1;
            } else {
                currentCapa->singleMotionDir = NULL;
            }
            str_c = matrixElem->Attribute("linkNextPos");
            if (str_c) {
                currentCapa->linkNextPos=new PointCel(str_c);
            } else {
                currentCapa->linkNextPos=NULL;
            }
            str_c = matrixElem->Attribute("isAngle");
            if (str_c) {
                currentCapa->isAngle=(bool)atoi(str_c);
            } else {
                currentCapa->isAngle=false;
            }
            str_c = matrixElem->Attribute("unlockPath");
            if (str_c) {
                int i=0,n=strlen(str_c);
                while (i<n) {
                    switch (str_c[i]) {
                        case 'E' :
                            currentCapa->tabUnlockPath.push_back(1); //East
                        break;
                        case 'W' :
                            currentCapa->tabUnlockPath.push_back(3); // West
                        break;
                        case 'N' :
                            currentCapa->tabUnlockPath.push_back(0); // North
                        break;
                        case 'S' :
                            currentCapa->tabUnlockPath.push_back(2); // South
                        break;
                    }
                    i++;
                }
            }
        }
        matrixElem = capaElem->FirstChildElement("motions");
        if (!matrixElem) {
            cerr << "erreur <motions>" << endl;
            return;
        } else {
            TiXmlElement *motionElem = matrixElem->FirstChildElement("motion");
            Motion *currentMotion=NULL;
            while (motionElem) {
                currentMotion = new Motion();
                str_c = motionElem->Attribute("objPath");
                if (str_c) {
                    int i=0,n=strlen(str_c);
                    while (i<n) {
                        switch (str_c[i]) {
                        case 'X' : break;
                        case 'E' :
                            currentMotion->PathToBlock.push_back(1); //East
                        break;
                        case 'W' :
                            currentMotion->PathToBlock.push_back(3); // West
                        break;
                        case 'N' :
                            currentMotion->PathToBlock.push_back(0); // North
                        break;
                        case 'S' :
                            currentMotion->PathToBlock.push_back(2); // South
                        break;
                        }
                        i++;
                    }
                }

                str_c = motionElem->Attribute("time");
                if (str_c) {
                    currentMotion->time = atof(str_c);
                } else {
                    currentMotion->time = 0.0;
                }
                str_c = motionElem->Attribute("vect");
                if (str_c) {
                    currentMotion->vect.fromString(string(str_c),PointCel(-1,-1));
                }

                str_c = motionElem->Attribute("unlockPath");
                if (str_c) {
                    int i=0,n=strlen(str_c);
                    while (i<n) {
                        switch (str_c[i]) {
                            case 'E' :
                            currentMotion->UnlockPath.push_back(1); //East
                            break;
                            case 'W' :
                            currentMotion->UnlockPath.push_back(3); // West
                            break;
                            case 'N' :
                            currentMotion->UnlockPath.push_back(0); // North
                            break;
                            case 'S' :
                            currentMotion->UnlockPath.push_back(2); // South
                            break;
                        }
                        i++;
                    }
                }
                currentCapa->tabMotions.push_back(currentMotion);
                motionElem = motionElem->NextSiblingElement("motion");
            }
        }

        tabCapabilities.push_back(currentCapa);

        // create rotated clones
        char strNum[32];
        OUTPUT << *currentCapa << endl;
        for (int i=0; i<3; i++) {
            sprintf(strNum,"%s.%0d",currentCapa->name.c_str(),i+1);
            tabCapabilities.push_back(new Capability(strNum,currentCapa,i+1));
            OUTPUT << *(tabCapabilities.back()) << endl;
        }
        capaElem = capaElem->NextSiblingElement("capability");
    }
}

SmartBlocksCapabilities::~SmartBlocksCapabilities() {
    vector<Capability*>::const_iterator it = tabCapabilities.begin();
    while (it!=tabCapabilities.end()) {
        delete (*it);
        it++;
    }
    tabCapabilities.clear();
}

bool compare(Validation* a,Validation* b) {
    return (a->gain<b->gain);
}

int dir(PointCel *v) {
    if (v->x==1) {
        return 1;
    }
    if (v->x==-1) {
        return 3;
    }
    if (v->y==1) {
        return 0;
    }
    if (v->y==-1) {
        return 2;
    }
    return -1;
}

vector <Validation*> *SmartBlocksCapabilities::validateDirection(const PresenceMatrix &pm,const PresenceMatrix &ltm,PointCel tab[4]) {
    vector<Capability*>::const_iterator it = tabCapabilities.begin();
    vector<Validation*> *tabValid = new vector<Validation*>();
    int g;
    bool zl,th,te;
    //OUTPUT << pm;
    //OUTPUT << ltm;
    while (it!=tabCapabilities.end()) {
        if ((*it)->validationMatrix.validate(pm,truthTable) ) {
// ----------------------------------------------------------------
// 2 cases : center cell is single or not
            if (pm.get(0,0)==singleCell && (*it)->singleMotionDir) {
//                OUTPUT << "valid :" << (*it)->name << endl;
//                OUTPUT << "tab[4]=" << tab[4] << ", singleMotionDir = " << *(*it)->singleMotionDir << endl;
                if (tab[4]==*(*it)->singleMotionDir) {
                    Validation *v = new Validation(*it,50,false);
                    tabValid->push_back(v);
                }
            } else {
                int d = (*it)->linkPrevPos?dir((*it)->linkPrevPos):-1;
//                    OUTPUT << "valid :" << (*it)->name << endl;
//                    if (d!=-1) OUTPUT << "tab["<<d<<"]=" << tab[d] << endl;
//                    OUTPUT << "prevDir=" << (*it)->prevDir << endl;
                if ((*it)->prevDir.isZero() || d==-1 || tab[d].isZero() || (*it)->prevDir==tab[d]) {
                    if (ltm.get(0,0)==fullCell && (*it)->linkPrevPos &&
                        (ltm.get(*(*it)->linkPrevPos)==emptyCell)) {
                    } else {
                        if (d==-1 || tab[d].isZero()) {
                            g=0;
                        } else {
                            g=10;
                        }
                        if ((*it)->isEnd) {
                            g+=2*(ltm.get(0,0)==emptyCell);
                            g+=2*((*it)->linkPrevPos && ltm.get(*(*it)->linkPrevPos)==fullCell);
                            if ((*it)->isHead) g=0;
                            //OUTPUT << "end g=" << g << " " << (*it)->name <<endl;
                        } else {
                            th = (*it)->linkPrevPos && ltm.get(*(*it)->linkPrevPos)==fullCell && pm.get(*(*it)->linkPrevPos)==emptyCell;
                            te = (*it)->linkNextPos && ltm.get(*(*it)->linkNextPos)==emptyCell;
                            g+=3*(!(*it)->isHead && !(*it)->isEnd && (th||te) && !(th&&te));
                            //g+=(!(*it)->isHead && !(*it)->isEnd && (th||te) );
                            g+=3*(!(*it)->isHead && !(*it)->isEnd && (ltm.get(*(*it)->linkPrevPos)==fullCell || ltm.get(*(*it)->linkNextPos)==emptyCell));
                            g+=2*((*it)->isHead && th);
                            g+=((*it)->isHead);
     //                       OUTPUT << th << "," << te << ":";
                        }
    //                    OUTPUT << "g=" << g << endl;
                        if (g>0 || ((*it)->isEnd && (*it)->isHead)) {
                        // zl : indique si le bloc est à distance 0 de Cf
                            zl = pm.get(0,0)!=singleCell && ltm.get(0,0)==emptyCell && (*it)->linkPrevPos && ltm.get(*(*it)->linkPrevPos)==fullCell;
     /*                       OUTPUT << "empty(0,0)" << ltm.get(0,0) <<endl;
                            if ((*it)->linkPrevPos) {
                                OUTPUT << "full" << *((*it)->linkPrevPos) << ":" << ltm.get(*(*it)->linkPrevPos) << endl;
                            } else  {
                                OUTPUT << "full no" << endl;
                            }*/
                            // si on est 0 distance alors on a un bonus de 100
                            Validation *v = new Validation(*it,g+100*zl,zl);
                            tabValid->push_back(v);
                        }
                    }
                }
            }
        }
        it++;
    }
    if (tabValid->empty()) {
        delete tabValid;
        return NULL;
    }
    std::sort(tabValid->begin(),tabValid->end(),compare);
    return tabValid;
}

/*
vector <Validation*> *SmartBlocksCapabilities::validateMulti(const PresenceMatrix &pm,const PresenceMatrix &ltm) {
    vector<Capability*>::const_iterator it = tabCapabilities.begin();
    vector<Validation*> *tabValid = new vector<Validation*>();
    int g;
    bool zl,th,te;
    //OUTPUT << pm;
    while (it!=tabCapabilities.end()) {
        if ((*it)->validationMatrix.validate(pm,truthTable)) {
            //OUTPUT << "-1 :" << (*it)->name << endl;
            if (ltm.get(0,0)==fullCell && (*it)->linkPrevPos && ltm.get(*(*it)->linkPrevPos)==emptyCell) {
            } else {
                g=0;
                if ((*it)->isEnd) {
                    g=(ltm.get(0,0)==emptyCell);
                    g+=((*it)->linkPrevPos && ltm.get(*(*it)->linkPrevPos)==fullCell);
                    if ((*it)->isHead) g=0;
                    //OUTPUT << "end g=" << g << " " << (*it)->name <<endl;
                } else {
                    th = (*it)->linkPrevPos && ltm.get(*(*it)->linkPrevPos)==fullCell && pm.get(*(*it)->linkPrevPos)==emptyCell;
                    te = (*it)->linkNextPos && ltm.get(*(*it)->linkNextPos)==emptyCell;
                    g=0;
                    g+=3*(!(*it)->isHead && !(*it)->isEnd && (th||te) && !(!th&&!te));
                    //g+=(!(*it)->isHead && !(*it)->isEnd && (th||te) );
                    g+=3*(!(*it)->isHead && !(*it)->isEnd && (ltm.get(*(*it)->linkPrevPos)==fullCell || ltm.get(*(*it)->linkNextPos)==emptyCell));
                    g+=2*((*it)->isHead && th);
                    g+=((*it)->isHead);
                }
                if (g>0 || ((*it)->isEnd && (*it)->isHead)) {
                    zl = ltm.get(0,0)==emptyCell && (*it)->linkPrevPos && ltm.get(*(*it)->linkPrevPos)==fullCell;
                    Validation *v = new Validation(*it,g,zl);
                    tabValid->push_back(v);
                } else  {
                    //OUTPUT << g << " :" << (*it)->name << endl;
                }
            }
        }
        it++;
    }
    if (tabValid->empty()) {
        delete tabValid;
        return NULL;
    }
    std::sort(tabValid->begin(),tabValid->end(),compare);
    return tabValid;
}
*/

vector <Validation*> *SmartBlocksCapabilities::validateToTarget(const PresenceMatrix &pm,const PointCel &dirToO) {
    vector<Capability*>::const_iterator it = tabCapabilities.begin();
    vector<Validation*> *tabValid = new vector<Validation*>();

    while (it!=tabCapabilities.end()) {
//OUTPUT << (*it)->name << "\n" << (*it)->validationMatrix;
        if ((*it)->validationMatrix.validate(pm,truthTable0)) {
//OUTPUT << "valid :" << *(*it)->linkNextPos << endl;
            if (((*it)->linkNextPos->x==0 || (*it)->linkNextPos->x == dirToO.x) && ((*it)->linkNextPos->y == 0 || (*it)->linkNextPos->y == dirToO.y)) {
                Validation *v = new Validation(*it,0,0);
                tabValid->push_back(v);
            }
        }
        it++;
    }
    if (tabValid->empty()) {
        delete tabValid;
        return NULL;
    }
    //std::sort(tabValid->begin(),tabValid->end(),compare);
    return tabValid;
}


/**************************************BPi************************************/
/* ValidationMatrix                                                               */
ValidationMatrix::ValidationMatrix() {
    memset(grid,0,sizeof(states)*9);
}

/*emptyState=0, fullState=3, filledState=5, emptiedState=6, transitState=7, jockerState=8*/
void ValidationMatrix::fromString(const char *str) {
    int ix=0,iy=2,i=ix+iy*3;
    const char *ptrStr=str;
    bool move;

    while (*ptrStr!=0) {
        move=true;
        switch (*ptrStr) {
        case '0' :
            grid[i]=emptyState;
            break;
        case '1' :
            grid[i]=wellPlaceState;
            break;
        case '3' :
            grid[i]=fullState;
            break;
        case '5' :
            grid[i]=filledState;
            break;
        case '4' :
            grid[i]=singleState;
            break;
        case '6' :
            grid[i]=emptiedState;
            break;
        case '7' :
            grid[i]=transitState;
            break;
        case '8' :
            grid[i]=jockerState;
            break;
        default :
            move=false;
        }
        if (move) {
            ix++;
            if (ix>2) {
                ix=0;
                iy--;
            }
            i=ix+iy*3;
        }
        ptrStr++;
    }
}

void ValidationMatrix::fromRotationZ(const ValidationMatrix& mat,int n) {
    int transf[4][9]={
        {0,1,2,3,4,5,6,7,8},
        {2,5,8,1,4,7,0,3,6},
        {8,7,6,5,4,3,2,1,0},
        {6,3,0,7,4,1,8,5,2}};
    int *matTransf = transf[n%4];
    for (int i=0; i<9; i++) {
        grid[i] = mat.grid[matTransf[i]];
    }

}

states ValidationMatrix::get(const PointCel& pt) {
    return grid[pt.x+pt.y*3+4];
}

void ValidationMatrix::set(const PointCel& pt,states v) {
    states *ptr = grid+pt.x+pt.y*3+4;
    *ptr=v;
}

// write ValidationMatrix values into a flux
ostream& operator<<(ostream& f,const ValidationMatrix &sm) {
    f << sm.grid[6] << " " << sm.grid[7] << " " << sm.grid[8] << endl;
    f << sm.grid[3] << " " << sm.grid[4] << " " << sm.grid[5] << endl;
    f << sm.grid[0] << " " << sm.grid[1] << " " << sm.grid[2] << endl;
  return f;
}

bool ValidationMatrix::validate(const PresenceMatrix &pm,const bool tt[9][5]) {
    int n=9;
    const states *vmg=grid;
    const presence *pmg=pm.grid;
    while (n--) {
        //OUTPUT << "TruthTable[" << *vmg << "][" << *pmg << "]" << "=" << tt[*vmg][*pmg] << endl;
        if (!tt[*vmg][*pmg]) {
            return false;
        }
        vmg++;
        pmg++;
    }
    return true;
}

bool ValidationMatrix::validateWithTarget(const PresenceMatrix &pm,const PresenceMatrix &ltm,int &gain) {
    int n=9;
    const states *vmg=grid;
    const presence *pmg=pm.grid;
    const presence *ltmg=ltm.grid;
    gain=0;
    while (n--) {
        if (!truthTable[*vmg][*pmg]) {
            gain=-1;
            return false;
        }
        gain+=gainTable[*vmg][*ltmg];
        vmg++;
        pmg++;
        ltmg++;
    }
    return true;
}

/**************************************BPi************************************/
/* PointCel                                                                */
PointCel::PointCel() {
    x=y=SHRT_MIN;
}

PointCel::PointCel(short vx,short vy):x(vx),y(vy) {
};

PointCel::PointCel(const char *str) {
    fromString(str);
}

PointCel::PointCel(const char *str,const PointCel &ref) {
    fromString(str,ref);
}


/*void PointCel::fromString(const string &str) {
    int dx,dy;
    cout << str << endl;
#ifdef WIN32
    sscanf_s(str.c_str(),"%d,%d,%d",&dx,&dy,&dz);
#else
    sscanf(str.c_str(),"%d,%d",&dx,&dy);
#endif
cout << dx << "," << dy << endl;
    x=dx-1;
    y=dy-1;
}*/

void PointCel::fromString(const char *str) {
    int dx,dy;
#ifdef WIN32
    sscanf_s(str.c_str(),"%d,%d,%d",&dx,&dy,&dz);
#else
    sscanf(str,"%d,%d",&dx,&dy);
#endif
    x=dx-1;
    y=dy-1;
}

void PointCel::fromString(const string &str,const PointCel &ref) {
    int dx,dy;
    sscanf(str.c_str(),"%d,%d",&dx,&dy);
    x=dx-ref.x-1;
    y=dy-ref.y-1;
}

// write PointCel values into a flux
ostream& operator<<(ostream& f,const PointCel &pt) {
    f << "(" << pt.x << "," << pt.y << ")";
  return f;
}

PointCel PointCel::rotateZ(int a) const {
    int tabMat[4][4]={
        {1,0,0,1},
        {0,1,-1,0},
        {-1,0,0,-1},
        {0,-1,1,0}};
    PointCel res;
    int *mat = tabMat[a%4];
    res.x = x*mat[0]+y*mat[1];
    res.y = x*mat[2]+y*mat[3];
    return res;
}

void PointCel::set(short vx,short vy) {
    x=vx;
    y=vy;
}

void PointCel::set(const char *str) {
    fromString(str);
}

bool operator !=(const PointCel p1,const PointCel p2) {
    return (p1.x!=p2.x || p1.y!=p2.y);
}

bool operator ==(const PointCel p1,const PointCel p2) {
    return (p1.x==p2.x && p1.y==p2.y);
}

/**************************************BPi************************************/
/* Motion                                                                    */
Motion::Motion() {
}

Motion::Motion(const Motion *src) {
    vector <short>::const_iterator cs = src->PathToBlock.begin();
    while (cs!=src->PathToBlock.end()) {
        PathToBlock.push_back(*cs);
        cs++;
    }

    cs = src->UnlockPath.begin();
    while (cs!=src->UnlockPath.end()) {
        UnlockPath.push_back(*cs);
        cs++;
    }
    time = src->time;
    vect = src->vect;
}

/**************************************BPi************************************/
/* PresenceMatrix                                                            */
PresenceMatrix::PresenceMatrix() {
    memset(grid,emptyCell,sizeof(presence)*9);
}

void PresenceMatrix::add(presence v,const PointCel &posAdd,const PointCel &origine) {
    PointCel p;
    p.x = posAdd.x-origine.x;
    if (p.x>=-1 && p.x<=1) {
        p.y = posAdd.y-origine.y;
        if (p.y>=-1 && p.y<=1) {
            *(grid+p.x+p.y*3+4)=v;
        }
    }
}

presence PresenceMatrix::get(const PointCel& pt) const {
    return grid[pt.x+pt.y*3+4];
}

presence PresenceMatrix::get(short x,short y) const {
    return grid[x+y*3+4];
}

void PresenceMatrix::set(const PointCel& pt,presence v) {
    presence *ptr = grid+pt.x+pt.y*3+4;
    *ptr=v;
}

bool PresenceMatrix::isBorder() {
    int i=9;
    presence *ptr=grid;

    while (i-- && *ptr!=emptyCell) { ptr++; };
    return (i!=-1);
}

// write PresenceMatrix values into a flux
 ostream& operator<<(ostream& f,const PresenceMatrix &sm) {
    f << sm.grid[6] << " " << sm.grid[7] << " " << sm.grid[8] << endl;
    f << sm.grid[3] << " " << sm.grid[4] << " " << sm.grid[5] << endl;
    f << sm.grid[0] << " " << sm.grid[1] << " " << sm.grid[2] << endl;
  return f;
}

/**************************************BPi************************************/
/* Validation                                                                */
Validation::Validation(Capability *c,short g,bool izd) {
    capa = c;
    gain = g;
    isZeroDistance = izd;
}
