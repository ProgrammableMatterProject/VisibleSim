#ifndef VECTOR2D_H
#define VECTOR2D_H


class Vector2D
{
public:
    float x,y;

    Vector2D();
    Vector2D(float p_x,float p_y):x(p_x),y(p_y) {};
    Vector2D operator+(const Vector2D &op) const { return Vector2D(x+op.x,y+op.y); }
    Vector2D operator-(const Vector2D &op) const { return Vector2D(x-op.x,y-op.y); }
    bool operator!= (const Vector2D &op) const { return x!=op.x || y!=op.y; }

};

#endif // VECTOR2D_H
