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

    double sum=(d2<0)?0.0:exp(blobbiness*(d2/radius2-1));
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

/* TODO */
double SkelLine::distance2(const Vecteur &pos) {
    return -1.0;
}
