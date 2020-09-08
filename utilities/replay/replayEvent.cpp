/*!
 * @file replayEvent.cpp
 * @brief Contains motion events classes for moving blocks in the replayer
 * @author Mattéo Daluz
 */

#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"
#include <map>
#include "../../simulatorCore/src/base/glBlock.h"
#include "../../simulatorCore/src/grid/lattice.h"
#include "replayPlayer.h"
using namespace std;

ReplayMotionEvent::ReplayMotionEvent()
{

}

Matrix Catoms3DRotationEvent::getMatrixFromTime(u8 time)
{
    //TODO Corriger : Ne fonctionne pas
    Matrix m;
    bool firstRotation;

    if(time < beginDate+duration/2)
        firstRotation = true;
    else
        firstRotation = false;
    int step = (time-beginDate)*2*20/duration; //TODO UNSURE


    if (firstRotation) {
        double currentAngle=angle*step/20;
        Matrix mr;
        mr.setRotation(currentAngle,axe1);

        Matrix matTCA,matTDC,matTAD;
        matTCA.setTranslation(-A0C0);
        matTDC.setTranslation(-A0D0+A0C0);
        matTAD.setTranslation(A0D0);
        m = matTAD*(mr*(matTDC*(mr*matTCA)));
        m = initialMatrix * m;

    } else {
        double currentAngle=-angle*step/20;
        // TRT-1R
        Matrix mr;
        mr.setRotation(currentAngle,axe2);

        Matrix matTCA,matTDC,matTAD;
        matTCA.setTranslation(-A1C1);
        matTDC.setTranslation(-A1D1+A1C1);
        matTAD.setTranslation(A1D1);
        m = matTAD*(mr*(matTDC*(mr*matTCA)));
        m = finalMatrix * m;
    }
    return m;
}

void Catoms3DRotationEvent::init(Catoms3D::Catoms3DGlBlock *mobileBlock,
        Catoms3D::Catoms3DGlBlock* fixedBlock)
{
    //TODO TEST NEEDED WITH EXPORT FILE
    if(type == 3)
    {
        radius = 0.4530052159;
        angle = atan(sqrt(2.0)/2.0)*180.0/M_PI;
    }
    else if(type ==4)
    {
        radius = 0.453081839321973;
        angle = 45.0;
    }


    static const double c_2 = 1.0/(3+sqrt(2));
    Matrix MA = mobileBlock->mat;
    Matrix MB = fixedBlock->mat;
    initialMatrix = MA;
    Matrix MA_1;

    // we calculate AB translation in A referentiel
    MA.inverse(MA_1);
    Matrix m = MA_1*MB;
    Vector3D AB = m*Vector3D(0,0,0,1);

    Matrix matTAB,matTBA;
    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);

    double r=AB.norme()/2.0;
    double shift = (angle>0)?c_2*r:-c_2*r;
    Vector3D V = AB^axe1;
    V.normer_interne();

    A0D0 = (0.5+0.5*radius)*AB+shift*V;
    A0C0 = (0.5-0.5*radius)*AB+shift*V;

    Matrix mr;
    mr.setRotation(angle,axe1);
    finalMatrix = matTAB*(mr*(matTBA*mr));

    m  = MA*finalMatrix;
    m.inverse(MA_1);
    m = MA_1*MB;
    AB = m*Vector3D(0,0,0,1);

    finalMatrix.inverse(MA_1);


    matTAB.setTranslation(AB);
    matTBA.setTranslation(-AB);
    mr.setRotation(angle,axe2);
    m = matTAB*(mr*(matTBA*mr));
    finalMatrix = finalMatrix*m;

    m = MA*finalMatrix;
    m.inverse(MA_1);
    m = MA_1*MB;
    AB = m*Vector3D(0,0,0,1);

    shift = (angle>0)?-c_2*r:c_2*r;
    V = AB^axe2;
    V.normer_interne();

    A1D1 = (0.5+0.5*radius)*AB+shift*V;
    A1C1 = (0.5-0.5*radius)*AB+shift*V;


}