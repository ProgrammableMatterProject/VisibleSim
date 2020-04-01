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

Color::Color(unsigned short r, unsigned short g, unsigned short b, unsigned short a,
             bool integers) {
    rgba[0] = r / 255.0f;
    rgba[1] = g / 255.0f;
    rgba[2] = b / 255.0f;
    rgba[3] = a / 255.0f;
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
