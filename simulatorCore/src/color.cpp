#include "color.h"

Color::Color() {
    memset(rgba,0,4*sizeof(GLfloat));
}

Color::Color(float r,float g,float b,float a) {
    rgba[0]=r;
    rgba[1]=g;
    rgba[2]=b;
    rgba[3]=a;
}

void Color::set(GLfloat r,GLfloat g, GLfloat b, GLfloat a) {
    rgba[0]=r;
    rgba[1]=g;
    rgba[2]=b;
    rgba[3]=a;
}

// écriture d'une couleur dans un flux
ostream& operator<<(ostream& f,const Color&p)
{ f << "(" << p.rgba[0] << "," << p.rgba[1] << "," << p.rgba[2] << "," << p.rgba[3] << ")";
  return f;
}

