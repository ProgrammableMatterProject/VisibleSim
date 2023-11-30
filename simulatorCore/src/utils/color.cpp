#include "color.h"
#include "exceptions.h" //@TODO BP REMOVE

Color::Color(float r,float g,float b) {
    set(r,g,b);
}

Color::Color(int r, int g, int b) {
    set(r,g,b);
}

void Color::set(GLfloat r,GLfloat g, GLfloat b) {
    _rgb[0]=uint8_t(clamp(int(r*255.0),0,255));
    _rgb[1]=uint8_t(clamp(int(g*255.0),0,255));
    _rgb[2]=uint8_t(clamp(int(b*255.0),0,255));
}

void Color::set(int r,int g, int b) {
    _rgb[0]=uint8_t(clamp(r,0,255));
    _rgb[1]=uint8_t(clamp(g,0,255));
    _rgb[2]=uint8_t(clamp(b,0,255));
}

// écriture d'une couleur dans un flux
ostream& operator<<(ostream& f,const Color&p)
{ f << "(" << to_string(p._rgb[0]) << "," << to_string(p._rgb[1]) << "," << to_string(p._rgb[2]) << ")";
  return f;
}

void Color::serialize(std::ofstream &bStream) {
    throw BaseSimulator::NotImplementedException(); // @TODO BP
}

void Color::serialize_cleartext(std::ofstream &dbStream) {
    throw BaseSimulator::NotImplementedException(); // @TODO BP
}

void Color::glMaterial(GLenum face, GLenum pname, float alpha) const {
    GLfloat v[4];
    v[0] = float(_rgb[0])/255.0f;
    v[1] = float(_rgb[1])/255.0f;
    v[2] = float(_rgb[2])/255.0f;
    v[3] = alpha;
    glMaterialfv(face,pname,v);
}