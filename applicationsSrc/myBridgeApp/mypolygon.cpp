#include <sstream>
#include "mypolygon.h"
#include "myBridgeAppCode.hpp"
#include <algorithm>

//#include <QDebug>

MyPolygon::MyPolygon(int p_Nmax):Nmax(p_Nmax),N(0) {
    tabPts = new Vector2D[Nmax+1]; // allocation of Nmax+1 element
}

MyPolygon::~MyPolygon() {
    // free the memory!
    //delete [] tabPts;
}

bool MyPolygon::addPoint(const Vector2D &p) {
    if (N==Nmax) return false;
    // add a new element and duplicate the first element in the last place [N]
    tabPts[N++] = p;
    tabPts[N] = tabPts[0];
    return true;
}

//void MyPolygon::draw(QPainter &painter) {
    //QPointF points[N];
    //painter.setBrush(Qt::blue);
    // Copy tabPts into points
    //for (int i=0; i< N; i++) {
        //points[i] = QPointF(tabPts[i].x, tabPts[i].y);
        //painter.drawEllipse(points[i],5,5);
    //}

//    painter.setBrush(Qt::yellow);
//    if(this->selected) painter.setBrush(Qt::green);
//    painter.drawPolygon(points, N);
//}

bool MyPolygon::isConvex() const {
    int i=0;

    while (i<N && isOnTheLeft(tabPts[(i+2)%N],i)) {
        i++;
    }

    return (i==N);
}

bool MyPolygon::isOnTheLeft(const Vector2D &p, int i) const {
    Vector2D AB = tabPts[i+1]-tabPts[i],
    AP = p-tabPts[i];

    return (AB.x*AP.y - AB.y*AP.x)>=0;
}


// return true if P is on the left of AB
bool MyPolygon::isOnTheLeft(const Vector2D &P, const Vector2D &A, const Vector2D &B) const {
    Vector2D AB=B-A;
    Vector2D AP=P-A;
    return (AB.x*AP.y - AB.y*AP.x)>=0;
}

bool MyPolygon::isInside(const Vector2D &p) const {
    // first check points
    int i=0;
    while (i<N-1 && p!=tabPts[i]) {
        i++;
    }
    if (i<N-1) return true;

    // check interior
    i=0;
    while (i<N && isOnTheLeft(p,i)) {
        i++;
    }

    return (i==N);
}



bool MyPolygon::insertPoint(const Vector2D &p) {
    //qDebug() << "Point " << p.x << "," << p.y;

    if(N==Nmax) return false;
    if (isInside(p)) {
//        qDebug() << "cas 0";
        return false;
    }

    if (isOnTheLeft(p,0)) {
//        qDebug() << "cas 1";
        int i=1;
        while (i<N && isOnTheLeft(p,i)) {
            i++;
        }
        if (i==N) return false; // normally it must find an edge out
        int j=i+1;
        while (j<N && !isOnTheLeft(p,j)) {
            j++;
        }
//        qDebug() << "insert " << p.x << ',' << p.y << " between" << i << "and " << j << "N=" << N;
        // use an intermediary array
        vector<Vector2D> tmp;
        for (int it=0; it<=i; ++it) {
            tmp.push_back(tabPts[it]);
        }
        tmp.push_back(p);
        for (int it=j; it<N; ++it) {
            tmp.push_back(tabPts[it]);
        }
        // copy into tabPts
        int it=0;
        for (auto &v:tmp) {
            tabPts[it++]=v;
        }
        tabPts[it]=tabPts[0];
        N=it;
    } else {
//        qDebug() << "cas 2";
        int j=1;
        while (j<N && !isOnTheLeft(p,j)) {
            j++;
        }
        if (j==N) return false; // normally it must find an edge out
        int i=(j+1)%N;
        while (i<N && isOnTheLeft(p,i)) {
            i++;
        }
        //       qDebug() << "insert " << p.x << ',' << p.y << " between" << i << "and " << j << "N=" << N;
        // use an intermediary array
        vector<Vector2D> tmp;
        for (int it=j; it<=i; ++it) {
            tmp.push_back(tabPts[it]);
        }
        tmp.push_back(p);
        // copy into tabPts
        int it=0;
        for (auto &v:tmp) {
            tabPts[it++]=v;
        }
        tabPts[it]=tabPts[0];
        N=it;
    }

    // simplify
    Vector2D *p0,*p1,*p2;
    bool aligned;
    do {
        // cas particulier du premier point aligné
        p0 = &tabPts[N-1];
        p1 = &tabPts[0];
        p2 = &tabPts[1];
        aligned= ((p0->x==p1->x && p0->x==p2->x) ||
                  (p0->y==p1->y && p0->y==p2->y) ||
                  ((p2->x-p0->x!=0)&&(p2->y-p0->y!=0)&&(p1->x-p0->x)/(p2->x-p0->x)==(p1->y-p0->y)/(p2->y-p0->y)));
        if (aligned) {
            for (int j=0; j<N; j++) {
                tabPts[j]=tabPts[j+1];
            }
            N--;
            tabPts[N]=tabPts[0];
//            qDebug() << "Remove 0";
        }
        int i=1;
        aligned=false;
        while (i<N && !aligned) {
            p0 = &tabPts[i-1];
            p1 = &tabPts[i];
            p2 = &tabPts[i+1];
            aligned= ((p1->x==p0->x && p1->x==p2->x) ||
                      (p1->y==p0->y && p1->y==p2->y) ||
                      ((p2->x-p0->x!=0)&&(p2->y-p0->y!=0)&&(p1->x-p0->x)/(p2->x-p0->x)==(p1->y-p0->y)/(p2->y-p0->y)));
            i++;
        }
        if (aligned) {
            i--;
//            qDebug() << "Remove " << i << ": " << tabPts[i].x << "," << tabPts[i].y;
            for (int j=i; j<N; j++) {
                tabPts[j]=tabPts[j+1];
            }
            N--;
        }
    } while (aligned);
    return true;
}
std::string MyPolygon::affichage() {
    std::stringstream ss;
    ss <<"Nmax: "<<Nmax<< ", N: "<< N << "\n";
    for(int i=1;i<=N;i++)
        ss  << i <<": (" <<tabPts[i].x << "," << tabPts[i].y <<")\n";

    return ss.str();
}

/*
void MyPolygon::triangulate() {
    QVector<int> L; // temporary vector of the list of indices not treated by the triangulation
    for (int i=0; i<N; i++) {
        L.push_back(i);
    }

    int i=0;
    while(L.size()>2) {
        // first triangle tested (0,1,2)
        Triangle T(&tabPts[L[i]],&tabPts[L[(i+1)%N]],&tabPts[L[(i+2)%N]]);
        auto itL=L.begin();

        while (itL!=L.end() && !T.isInside(tabPts[*itL])) {
            itL++;
        }
        if (itL==L.end()) { // if there is not point inside T
            triangles.push_back(T); // add T in the list of triangles
            L.removeAll((i+1)%N); // remove the middle point from the list
        } else {
            i=(i+1)%N;

        }
    }
    qDebug() << "Triangulation creates: " << triangles.size() << " triangles";
}
*/
double DistancePointLigne(const Vector2D& point, const Vector2D& ligne_start, const Vector2D& ligne_end) {
    Vector2D vecteurLigne = { ligne_end.x - ligne_start.x, ligne_end.y - ligne_start.y };
    Vector2D vecteurPointStart = { point.x - ligne_start.x, point.y - ligne_start.y };

    double produitScalaire = vecteurPointStart.x * vecteurLigne.x + vecteurPointStart.y * vecteurLigne.y;
    double normeLigneCarree = vecteurLigne.x * vecteurLigne.x + vecteurLigne.y * vecteurLigne.y;

    Vector2D projection = { static_cast<float>((produitScalaire / normeLigneCarree) * vecteurLigne.x), static_cast<float>((produitScalaire / normeLigneCarree) * vecteurLigne.y) };

    double distance = std::sqrt((point.x - ligne_start.x - projection.x) * (point.x - ligne_start.x - projection.x) +
                                (point.y - ligne_start.y - projection.y) * (point.y - ligne_start.y - projection.y));

    return distance;
}
double MyPolygon::DistancePointPolygone(const Vector2D& point, const std::vector<Vector2D>& polygone) {
    double distanceMin = std::numeric_limits<double>::infinity();

    for (size_t i = 0; i < polygone.size(); ++i) {
        const Vector2D& A = polygone[i];
        const Vector2D& B = polygone[(i + 1) % polygone.size()];  // Prend le sommet suivant, gère la boucle du polygone

        double distance = DistancePointLigne(point, A, B);
        distanceMin = std::min(distanceMin, distance);
    }

    return distanceMin;
}
double CrossProduct(const Vector2D& A, const Vector2D& B, const Vector2D& C) {
    return (B.x - A.x) * (C.y - A.y) - (B.y - A.y) * (C.x - A.x);
}
std::vector<Vector2D> MyPolygon::JarvisConvexHull(std::vector<Vector2D>& points) {
    if (points.size() < 3) {
        return points;
    }

    std::vector<Vector2D> hull;

    // Find the leftmost point
    int leftmost = 0;
    for (int i = 1; i < points.size(); ++i) {
        if (points[i].x < points[leftmost].x) {
            leftmost = i;
        }
    }

    int current = leftmost;
    do {
        hull.push_back(points[current]);
        int next = (current + 1) % points.size();

        for (int i = 0; i < points.size(); ++i) {
            if (CrossProduct(points[current], points[next], points[i]) > 0) {
                next = i;
            }
        }

        current = next;
    } while (current != leftmost);

    return hull;
}


