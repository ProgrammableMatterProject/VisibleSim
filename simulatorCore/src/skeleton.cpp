#include "skeleton.h"
#include "math.h"

Skeleton::~Skeleton() {
    Skeleton *s;
    while (!children.empty()) {
        s = children.back();
        delete s;
        children.pop_back();
    }
}

void Skeleton::add(Skeleton *child) {
    children.push_back(child);
}

double Skeleton::potentiel(const Vecteur &pos) {
    double d2 = distance2(pos);

    double sum=0;
    if (d2>0) sum=exp(blobbiness*(d2/radius2-1));
    vector <Skeleton*>::const_iterator ci=children.begin();
    while (ci!=children.end()) {
        sum+=(*ci)->potentiel(pos);
        ci++;
    }
    return sum;
}

double SkelPoint::distance2(const Vecteur &pos) {
    return (pos-ptA).norme2();
}

double SkelLine::distance2(const Vecteur &pos) {
    Vecteur AM = pos-ptA;
    double AMAB = AM*AB;
    if (AMAB<0) return AM.norme2();
    AMAB=(AMAB*AMAB)/AB2;
    if (AMAB>AB2) return (pos-ptB).norme2();
    return AM.norme2()-AMAB;
}
