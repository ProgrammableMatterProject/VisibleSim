#include "datomsMotionEngine.h"
#include "../../grid/lattice.h"

//! \namespace Datoms
namespace Datoms {

    DatomsMotionEngine::DatomsMotionEngine() {
        const Vector3D up(0, 0, 1),xy(1, 1, 0), _xy(-1, 1, 0);
        float r = 5.0,
              c = 2*r/(3*M_SQRT2-1),
              c_2 = c/2, // c/2=1,541953143
              e = r*(2-M_SQRT2)/(3*M_SQRT2-1),
        cx = 0.56374 * r, // ??
        cy = e/M_SQRT2, // e/sqrt(2)
        cz = c*(0.5+M_SQRT1_2)+e, // c(0.5+sqrt(2)/2)+e
        dy = c_2+e/M_SQRT2, // c/2+e/sqrt(2)
        dx = dy + c/M_SQRT2, // dy+c/sqrt(2)
        dz = c_2+e; // c/2+e

// allocation of pistons
        pistons.push_back(DatomsMotionPiston(Vector3D(r, r, 0), Piston012A));
        pistons.back().setAxis(0, 0,Vector3D(r, -c_2, -c_2,1), up); //#0
        pistons.back().setAxis(1, 1,Vector3D(-c_2, r, c_2,1), -up); //#1
        pistons.back().setAxis(2, 2,Vector3D(cx, cy, cz,1), _xy); //#2 XXX
        pistons.back().setAxis(3, 10,Vector3D(cx, cy, -cz,1), -_xy); //#10
        pistons.push_back(DatomsMotionPiston(Vector3D(-r, r, 0), Piston136B));
        pistons.back().setAxis(0, 1,Vector3D(c_2, r, -c_2,1), up); //#1
        pistons.back().setAxis(1, 3,Vector3D(-cx, cy, cz,1), -xy); //#3
        pistons.back().setAxis(2, 6,Vector3D(-r, -c_2, c_2,1), -up); //#6
        pistons.back().setAxis(3, 11,Vector3D(-cx, cy, -cz,1), xy); //#11
        pistons.push_back(DatomsMotionPiston(Vector3D(-r, -r, 0), Piston4678));
        pistons.back().setAxis(0, 4,Vector3D(-cx, -cy, cz,1), -_xy); //#4
        pistons.back().setAxis(1, 6,Vector3D(-r, c_2, -c_2,1), up); //#6
        pistons.back().setAxis(2, 7,Vector3D(c_2, -r, c_2,1), -up); //#7
        pistons.back().setAxis(3, 8,Vector3D(-cx, -cy, -cz,1), _xy); //#8
        pistons.push_back(DatomsMotionPiston(Vector3D(r, -r, 0), Piston0579));
        pistons.back().setAxis(0, 0,Vector3D(r, c_2, c_2,1), -up); //#0
        pistons.back().setAxis(1, 5,Vector3D(cx, -cy, cz,1), xy); //#5
        pistons.back().setAxis(2, 7,Vector3D(-c_2, -r, c_2,1), up); //#7
        pistons.back().setAxis(3, 9,Vector3D(cx, -cy, -cz,1), -xy); //#9
        pistons.push_back(DatomsMotionPiston(Vector3D(0, 0, M_SQRT2 * r), Piston2345));
        pistons.back().setAxis(0, 2,Vector3D(dx, dy, dz,1), -_xy); //#2
        pistons.back().setAxis(1, 3,Vector3D(-dy, dx, dz,1), xy); //#3
        pistons.back().setAxis(2, 4,Vector3D(-dx, -dy, dz,1), _xy); //#4
        pistons.back().setAxis(3, 5,Vector3D(dy, -dx, dz,1), -xy); //#5
        pistons.push_back(DatomsMotionPiston(Vector3D(0, 0, -M_SQRT2 * r), Piston89AB));
        pistons.back().setAxis(0, 8,Vector3D(-dx, -dy, -dz,1), -_xy); //#8
        pistons.back().setAxis(1, 9,Vector3D(dy, -dx, -dz,1), xy); //#9
        pistons.back().setAxis(2, 10,Vector3D(dx, dy, -dz,1), _xy); //#10
        pistons.back().setAxis(3, 11,Vector3D(-dy, dx, -dz,1), -xy); //#11

        addSame(0, 0, {{Piston012A, Piston0579},
                       {{1, 7},     {2, 5}, {10, 9}}});
        addSame(0, 0, {{Piston0579, Piston012A},
                       {{7, 1},     {5, 2}, {9, 10}}});
        addSame(0, 1, {{Piston012A, Piston136B},
                       {{1, 6},     {2, 11}, {10, 3}}});
        addSame(0, 1, {{Piston0579, Piston012A},
                       {{7, 0},     {5, 10}, {9, 2}}});
        addSame(0, 2, {{Piston012A, Piston012A},
                       {{1, 10},    {2, 0}, {10, 1}}});
        addSame(0, 2, {{Piston0579, Piston2345},
                       {{7, 4},     {5, 5}, {9, 3}}});
        addSame(0, 3, {{Piston012A, Piston2345},
                       {{1, 5},     {2, 4}, {10, 2}}});
        addSame(0, 3, {{Piston0579, Piston136B},
                       {{7, 11},    {5, 6}, {9, 1}}});
        addSame(0, 4, {{Piston012A, Piston4678},
                       {{1, 8},     {2, 6}, {10, 7}}});
        addSame(0, 4, {{Piston0579, Piston2345},
                       {{7, 2},     {5, 3}, {9, 5}}});
        addSame(0, 5, {{Piston012A, Piston2345},
                       {{1, 3},     {2, 2}, {10, 4}}});
        addSame(0, 5, {{Piston0579, Piston0579},
                       {{7, 9},     {5, 0}, {9, 7}}});
        addSame(0, 6, {{Piston012A, Piston136B},
                       {{1, 1},     {2, 3}, {10, 11}}});
        addSame(0, 6, {{Piston0579, Piston4678},
                       {{7, 7},     {5, 4}, {9, 8}}});
        addSame(0, 7, {{Piston012A, Piston0579},
                       {{1, 0},     {10, 5}, {2, 9}}});
        addSame(0, 7, {{Piston0579, Piston4678},
                       {{7, 6},     {9, 4}, {5, 8}}});
        addSame(0, 8, {{Piston012A, Piston4678},
                       {{1, 4},     {2, 7}, {10, 6}}});
        addSame(0, 8, {{Piston0579, Piston89AB},
                       {{7, 10},     {5, 9}, {9, 11}}});
        addSame(0, 9, {{Piston012A, Piston89AB},
                       {{1, 11},     {2, 8}, {10, 10}}});
        addSame(0, 9, {{Piston0579, Piston0579},
                       {{7, 5},     {5, 7}, {9, 0}}});
        addSame(0, 10, {{Piston012A, Piston012A},
                       {{1, 2},     {2, 1}, {10, 0}}});
        addSame(0, 10, {{Piston0579, Piston89AB},
                       {{7, 8},     {5, 11}, {9, 9}}});
        addSame(0, 11, {{Piston012A, Piston89AB},
                        {{1, 9},     {2, 10}, {10, 8}}});
        addSame(0, 11, {{Piston0579, Piston136B},
                        {{7, 3},     {5, 1}, {9, 6}}});
        addSame(1, 1, {{Piston012A, Piston136B},
                        {{0, 6},     {10, 11}, {2, 3}}});
        addSame(1, 1, {{Piston136B, Piston012A},
                        {{6, 0},     {11, 10}, {3, 2}}});
        addSame(1, 2, {{Piston012A, Piston012A},
                       {{0, 10},     {10, 0}, {2, 1}}});
        addSame(1, 2, {{Piston136B, Piston2345},
                       {{6, 4},     {11, 5}, {3, 3}}});
        addSame(1, 3, {{Piston012A, Piston2345},
                       {{0, 5},     {10, 4}, {2, 2}}});
        addSame(1, 3, {{Piston136B, Piston136B},
                       {{6, 11},     {11, 6}, {3, 1}}});
        addSame(1, 4, {{Piston012A, Piston4678},
                       {{0, 8},     {10, 6}, {2, 7}}});
        addSame(1, 4, {{Piston136B, Piston2345},
                       {{6, 2},     {11, 3}, {3, 5}}});
        addSame(1, 5, {{Piston012A, Piston2345},
                       {{0, 3},     {10, 2}, {2, 4}}});
        addSame(1, 5, {{Piston136B, Piston0579},
                       {{6, 9},     {11, 0}, {3, 7}}});
        addSame(1, 6, {{Piston012A, Piston136B},
                       {{0, 1},     {2, 11}, {10, 3}}});
        addSame(1, 6, {{Piston136B, Piston4678},
                       {{6, 7},     {3, 8}, {11, 4}}});
        addSame(1, 7, {{Piston012A, Piston0579},
                       {{0, 0},     {10, 9}, {2, 5}}});
        addSame(1, 7, {{Piston136B, Piston4678},
                       {{6, 6},     {11, 8}, {3, 4}}});
        addSame(1, 8, {{Piston012A, Piston4678},
                       {{0, 4},     {10, 7}, {2, 6}}});
        addSame(1, 8, {{Piston136B, Piston89AB},
                       {{6, 10},     {11, 9}, {3, 11}}});
        addSame(1, 9, {{Piston012A, Piston89AB},
                       {{0, 11},     {10, 8}, {2, 10}}});
        addSame(1, 9, {{Piston136B, Piston0579},
                       {{6, 5},     {11, 7}, {3, 0}}});
        addSame(1, 10, {{Piston012A, Piston012A},
                       {{0, 2},     {10, 1}, {2, 0}}});
        addSame(1, 10, {{Piston136B, Piston89AB},
                       {{6, 8},     {11, 11}, {3, 9}}});
        addSame(1, 11, {{Piston012A, Piston89AB},
                        {{0, 9},     {10, 10}, {2, 8}}});
        addSame(1, 11, {{Piston136B, Piston136B},
                        {{6, 3},     {11, 1}, {3, 6}}});
        addSame(2, 2, {{Piston2345, Piston012A},
                        {{4, 10},     {5, 0}, {3, 1}}});
        addSame(2, 2, {{Piston012A, Piston2345},
                        {{10, 4},     {0, 5}, {1, 3}}});
        addSame(2, 3, {{Piston2345, Piston2345},
                       {{4, 5},     {5, 4}, {3, 2}}});
        addSame(2, 3, {{Piston012A, Piston136B},
                       {{10, 11},     {0, 6}, {1, 1}}});
        addSame(2, 4, {{Piston2345, Piston4678},
                       {{4, 8},     {5, 6}, {3, 7}}});
        addSame(2, 4, {{Piston012A, Piston2345},
                       {{10, 2},     {0, 3}, {1, 5}}});
        addSame(2, 5, {{Piston2345, Piston2345},
                       {{4, 3},     {5, 2}, {3, 4}}});
        addSame(2, 5, {{Piston012A, Piston0579},
                       {{10, 9},     {0, 0}, {1, 7}}});
        addSame(2, 6, {{Piston2345, Piston136B},
                       {{4, 1},     {5, 3}, {3, 11}}});
        addSame(2, 6, {{Piston012A, Piston4678},
                       {{10, 7},     {0, 4}, {1, 8}}});
        addSame(2, 7, {{Piston2345, Piston0579},
                       {{4, 0},     {5, 9}, {3, 5}}});
        addSame(2, 7, {{Piston012A, Piston4678},
                       {{10, 6},     {0, 8}, {1, 4}}});
        addSame(2, 8, {{Piston2345, Piston4678},
                       {{4, 4},     {5, 7}, {3, 6}}});
        addSame(2, 8, {{Piston012A, Piston89AB},
                       {{10, 10},     {0, 9}, {1, 11}}});
        addSame(2, 9, {{Piston2345, Piston89AB},
                       {{4, 11},     {5, 8}, {3, 10}}});
        addSame(2, 9, {{Piston012A, Piston0579},
                       {{10, 5},     {0, 7}, {1, 0}}});
        addSame(2, 10, {{Piston2345, Piston012A},
                       {{4, 2},     {5, 1}, {3, 0}}});
        addSame(2, 10, {{Piston012A, Piston89AB},
                       {{10, 8},     {0, 11}, {1, 9}}});
        addSame(2, 11, {{Piston2345, Piston89AB},
                        {{4, 9},     {5, 10}, {3, 8}}});
        addSame(2, 11, {{Piston012A, Piston136B},
                        {{10, 3},     {0, 1}, {1, 6}}});
        addSame(3, 3, {{Piston136B, Piston2345},
                        {{11, 5},     {6, 4}, {1, 2}}});
        addSame(3, 3, {{Piston2345, Piston136B},
                        {{5, 11},     {4, 6}, {2, 1}}});
        addSame(3, 4, {{Piston136B, Piston4678},
                       {{11, 8},     {6, 6}, {1, 7}}});
        addSame(3, 4, {{Piston2345, Piston2345},
                       {{5, 2},     {4, 3}, {2, 5}}});
        addSame(3, 5, {{Piston136B, Piston2345},
                       {{11, 3},     {6, 2}, {1, 4}}});
        addSame(3, 5, {{Piston2345, Piston0579},
                       {{5, 9},     {4, 0}, {2, 7}}});
        addSame(3, 6, {{Piston136B, Piston136B},
                       {{11, 1},     {6, 3}, {1, 11}}});
        addSame(3, 6, {{Piston2345, Piston4678},
                       {{5, 7},     {4, 4}, {2, 8}}});
        addSame(3, 7, {{Piston136B, Piston0579},
                       {{11, 0},     {6, 9}, {1, 5}}});
        addSame(3, 7, {{Piston2345, Piston4678},
                       {{5, 6},     {4, 8}, {2, 4}}});
        addSame(3, 8, {{Piston136B, Piston4678},
                       {{11, 4},     {6, 7}, {1, 6}}});
        addSame(3, 8, {{Piston2345, Piston89AB},
                       {{5, 10},     {4, 9}, {2, 11}}});
        addSame(3, 9, {{Piston136B, Piston89AB},
                       {{11, 11},     {6, 8}, {1, 10}}});
        addSame(3, 9, {{Piston2345, Piston0579},
                       {{5, 5},     {4, 7}, {2, 0}}});
        addSame(3, 10, {{Piston136B, Piston012A},
                       {{11, 2},     {6, 1}, {1, 0}}});
        addSame(3, 10, {{Piston2345, Piston89AB},
                       {{5, 8},     {4, 11}, {2, 9}}});
        addSame(3, 11, {{Piston136B, Piston89AB},
                        {{11, 9},     {6, 10}, {1, 8}}});
        addSame(3, 11, {{Piston2345, Piston136B},
                        {{5, 3},     {4, 1}, {2, 6}}});
        addSame(4, 4, {{Piston2345, Piston4678},
                        {{2, 8},     {3, 6}, {5, 7}}});
        addSame(4, 4, {{Piston4678, Piston2345},
                        {{8, 2},     {6, 3}, {7, 5}}});
        addSame(4, 5, {{Piston2345, Piston2345},
                       {{2, 3},     {3, 2}, {5, 4}}});
        addSame(4, 5, {{Piston4678, Piston0579},
                       {{8, 9},     {6, 0}, {7, 7}}});
        addSame(4, 6, {{Piston2345, Piston136B},
                       {{2, 1},     {3, 3}, {5, 11}}});
        addSame(4, 6, {{Piston4678, Piston4678},
                       {{8, 7},     {6, 4}, {7, 8}}});
        addSame(4, 7, {{Piston2345, Piston0579},
                       {{2, 0},     {3, 9}, {5, 5}}});
        addSame(4, 7, {{Piston4678, Piston4678},
                       {{8, 6},     {6, 8}, {7, 4}}});
        addSame(4, 8, {{Piston2345, Piston4678},
                       {{2, 4},     {3, 7}, {5, 6}}});
        addSame(4, 8, {{Piston4678, Piston89AB},
                       {{8, 10},     {6, 9}, {7, 11}}});
        addSame(4, 9, {{Piston2345, Piston89AB},
                       {{2, 11},     {3, 8}, {5, 10}}});
        addSame(4, 9, {{Piston4678, Piston0579},
                       {{8, 5},     {6, 7}, {7, 0}}});
        addSame(4, 10, {{Piston2345, Piston012A},
                       {{2, 2},     {3, 1}, {5, 0}}});
        addSame(4, 10, {{Piston4678, Piston89AB},
                       {{8, 8},     {6, 11}, {7, 9}}});
        addSame(4, 11, {{Piston2345, Piston89AB},
                        {{2, 9},     {3, 10}, {5, 8}}});
        addSame(4, 11, {{Piston4678, Piston136B},
                        {{8, 3},     {6, 1}, {7, 6}}});
        addSame(5, 5, {{Piston0579, Piston2345},
                        {{9, 3},     {0, 2}, {7, 4}}});
        addSame(5, 5, {{Piston2345, Piston0579},
                        {{3, 9},     {2, 0}, {4, 7}}});
        addSame(5, 6, {{Piston0579, Piston136B},
                       {{9, 1},     {0, 3}, {7, 11}}});
        addSame(5, 6, {{Piston2345, Piston4678},
                       {{3, 7},     {2, 4}, {4, 8}}});
        addSame(5, 7, {{Piston0579, Piston0579},
                       {{9, 0},     {0, 9}, {7, 5}}});
        addSame(5, 7, {{Piston2345, Piston4678},
                       {{3, 6},     {2, 8}, {4, 4}}});
        addSame(5, 8, {{Piston0579, Piston4678},
                       {{9, 4},     {0, 7}, {7, 6}}});
        addSame(5, 8, {{Piston2345, Piston89AB},
                       {{3, 10},     {2, 9}, {4, 11}}});
        addSame(5, 9, {{Piston0579, Piston89AB},
                       {{9, 11},     {0, 8}, {7, 10}}});
        addSame(5, 9, {{Piston2345, Piston0579},
                       {{3, 5},     {2, 7}, {4, 0}}});
        addSame(5, 10, {{Piston0579, Piston012A},
                       {{9, 2},     {0, 1}, {7, 0}}});
        addSame(5, 10, {{Piston2345, Piston89AB},
                       {{3, 8},     {2, 11}, {4, 9}}});
        addSame(5, 11, {{Piston0579, Piston89AB},
                        {{9, 9},     {0, 10}, {7, 8}}});
        addSame(5, 11, {{Piston2345, Piston136B},
                        {{3, 3},     {2, 1}, {4, 6}}});
        addSame(6, 6, {{Piston4678, Piston136B},
                        {{7, 1},     {4, 3}, {8, 11}}});
        addSame(6, 6, {{Piston136B, Piston4678},
                        {{1, 7},     {3, 4}, {11, 8}}});
        addSame(6, 7, {{Piston4678, Piston0579},
                       {{7, 0},     {4, 9}, {8, 5}}});
        addSame(6, 7, {{Piston136B, Piston4678},
                       {{1, 6},     {3, 8}, {11, 4}}});
        addSame(6, 8, {{Piston4678, Piston4678},
                       {{7, 4},     {4, 7}, {8, 6}}});
        addSame(6, 8, {{Piston136B, Piston89AB},
                       {{1, 10},     {3, 9}, {11, 11}}});
        addSame(6, 9, {{Piston4678, Piston89AB},
                       {{7, 11},     {4, 8}, {8, 10}}});
        addSame(6, 9, {{Piston136B, Piston0579},
                       {{1, 5},     {3, 7}, {11, 0}}});
        addSame(6, 10, {{Piston4678, Piston012A},
                       {{7, 2},     {4, 1}, {8, 0}}});
        addSame(6, 10, {{Piston136B, Piston89AB},
                       {{1, 8},     {3, 11}, {11, 9}}});
        addSame(6, 11, {{Piston4678, Piston89AB},
                        {{7, 9},     {4, 10}, {8, 8}}});
        addSame(6, 11, {{Piston136B, Piston136B},
                        {{1, 3},     {3, 1}, {11, 6}}});
        addSame(7, 7, {{Piston4678, Piston0579},
                        {{6, 0},     {8, 9}, {4, 5}}});
        addSame(7, 7, {{Piston0579, Piston4678},
                        {{0, 6},     {9, 8}, {5, 4}}});
        addSame(7, 8, {{Piston4678, Piston4678},
                       {{6, 4},     {8, 7}, {4, 6}}});
        addSame(7, 8, {{Piston0579, Piston89AB},
                       {{0, 10},     {9, 9}, {5, 11}}});
        addSame(7, 9, {{Piston4678, Piston89AB},
                       {{6, 11},     {8, 8}, {4, 10}}});
        addSame(7, 9, {{Piston0579, Piston0579},
                       {{0, 5},     {9, 7}, {5, 0}}});
        addSame(7, 10, {{Piston4678, Piston012A},
                       {{6, 2},     {8, 1}, {4, 0}}});
        addSame(7, 10, {{Piston0579, Piston89AB},
                       {{0, 8},     {9, 11}, {5, 9}}});
        addSame(7, 11, {{Piston4678, Piston89AB},
                        {{6, 9},     {8, 10}, {4, 8}}});
        addSame(7, 11, {{Piston0579, Piston136B},
                        {{0, 3},     {9, 1}, {5, 6}}});
        addSame(8, 8, {{Piston89AB, Piston4678},
                        {{10, 4},     {9, 7}, {11, 6}}});
        addSame(8, 8, {{Piston4678, Piston89AB},
                        {{4, 10},     {7, 9}, {6, 11}}});
        addSame(8, 9, {{Piston89AB, Piston89AB},
                       {{10, 11},     {9, 8}, {11, 10}}});
        addSame(8, 9, {{Piston4678, Piston0579},
                       {{4, 5},     {7, 7}, {6, 0}}});
        addSame(8, 10, {{Piston89AB, Piston012A},
                       {{10, 2},     {9, 1}, {11, 0}}});
        addSame(8, 10, {{Piston4678, Piston89AB},
                       {{4, 8},     {7, 11}, {6, 9}}});
        addSame(8, 11, {{Piston89AB, Piston89AB},
                        {{10, 9},     {9, 10}, {11, 8}}});
        addSame(8, 11, {{Piston4678, Piston136B},
                        {{4, 3},     {7, 1}, {6, 6}}});
        addSame(9, 9, {{Piston0579, Piston89AB},
                        {{5, 11},     {7, 8}, {0, 10}}});
        addSame(9, 9, {{Piston89AB, Piston0579},
                        {{11, 5},     {8, 7}, {10, 0}}});
        addSame(9, 10, {{Piston0579, Piston012A},
                       {{5, 2},     {7, 1}, {0, 0}}});
        addSame(9, 10, {{Piston89AB, Piston89AB},
                       {{11, 8},     {8, 11}, {10, 9}}});
        addSame(9, 11, {{Piston0579, Piston89AB},
                        {{5, 9},     {7, 10}, {0, 8}}});
        addSame(9, 11, {{Piston89AB, Piston136B},
                        {{11, 3},     {8, 1}, {10, 6}}});
        addSame(10, 10, {{Piston89AB, Piston012A},
                        {{8, 2},     {11, 1}, {9, 0}}});
        addSame(10, 10, {{Piston012A, Piston89AB},
                        {{2, 8},     {1, 11}, {0, 9}}});
        addSame(10, 11, {{Piston89AB, Piston89AB},
                         {{8, 9},     {11, 10}, {9, 8}}});
        addSame(10, 11, {{Piston012A, Piston136B},
                         {{2, 3},     {1, 1}, {0, 6}}});
        addSame(11, 11, {{Piston136B, Piston89AB},
                         {{3, 9},     {1, 10}, {6, 8}}});
        addSame(11, 11, {{Piston89AB, Piston136B},
                         {{9, 3},     {10, 1}, {8, 6}}});
    }

    /*bool DatomsDestinations::isUsable(uint8_t mobile,uint8_t pivot,const DatomsBlock *m) const{
        if (mobile!=mobileConId || pivot!=pivotConId) return false;


        return true;
    }*/

    vector<uint8_t> DatomsDestinations::getPistonConnectors() const {
        vector<uint8_t> res;
        for (auto &cpl:dests.second) {
            res.push_back(cpl.first);
        }
        return res;
    }


}

