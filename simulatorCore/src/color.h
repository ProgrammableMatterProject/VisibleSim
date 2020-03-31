#ifndef COLOR_H_
#define COLOR_H_

#include <iostream>
#include <fstream>
#include <cmath>
#ifndef _WIN
#include <memory.h>
#endif

#include "shaders.h"

class Color {
public :
    GLfloat rgba[4];
    Color();
    Color(float r,float g,float b,float a=1.0);

    /**
     * Special constructor to build a Color object using standard rgb integer values
     * @param integers dummy parameter to remove ambiguity between constructors
     * @param r
     * @param g
     * @param b
     * @param a intensity
     */
    Color(unsigned short r, unsigned short g,unsigned short b, unsigned short a,bool integers);

    Color(const Color& c):rgba{ c.rgba[0], c.rgba[1], c.rgba[2], c.rgba[3] } {};

    void set(float r,float g,float b,float a=1.0);
//    Color(unsigned char r,unsigned char g,unsigned char b,unsigned char a=255) { color[0]=r/255.0; color[1]=g/255.0; color[2]=b/255.0; color[3]=a/255.0; };
    inline void glColor() { glColor4fv(rgba); };
    inline const GLfloat operator[](const int i) const { return rgba[i]; };
    inline bool operator==(const Color &c) const { return (rgba[0] == c.rgba[0] && rgba[1] == c.rgba[1] && rgba[2] == c.rgba[2] && rgba[3] == c.rgba[3]); };
    inline bool operator!=(const Color &c) const { return !(*this==c); };
    friend ostream& operator<<(ostream& f,const Color &c);
};

inline static const Color WHITE(1.0f,1.0f,1.0f);
inline static const Color RED(1.0f,0.0f,0.0f);
inline static const Color GREEN(0.0f,1.0f,0.0f);
inline static const Color LIGHTGREEN(0.75f,1.0f,0.25f);
inline static const Color BLUE(0.0f,0.0f,1.0f);
inline static const Color YELLOW(1.0f,1.0f,0.0f);
inline static const Color CYAN(0.0f,1.0f,1.0f);
inline static const Color MAGENTA(1.0f,0.0f,1.0f);
inline static const Color LIGHTBLUE(173/255.0,216/255.0,230/255.0);
inline static const Color GOLD(1.0,215/255.0,0);
inline static const Color PINK(1.0,192/255.0,203/255.0);
inline static const Color GREY(0.5,0.5,0.5);
inline static const Color LIGHTGREY(0.75,0.75,0.75);
inline static const Color DARKGREY(0.25,0.25,0.25);
inline static const Color ORANGE(1.0,0.64706,0.0);
inline static const Color DARKORANGE(1.0,0.549,0.0);
inline static const Color BLACK(0.0,0.0,0.0);
inline static const Color BROWN(102, 51, 0, 255, true);
inline static const Color DARKGREEN(6, 240, 46, 255, true);

static const GLfloat tabColors[12][4] = {{1.0,0.0,0.0,1.0},{1.0,0.647058824,0.0,1.0},{1.0,1.0,0.0,1.0},
                                         {0.0,1.0,0.0,1.0},{0.0,0.0,1.0,1.0},
                                         {0.274509804,0.509803922,0.705882353,1.0},
                                         {0.815686275,0.125490196,0.564705882,1.0},{0.5,0.5,0.5,1.0},
                                         {0.980392157,0.5,0.456,1.0},{0.549019608,0.5,0.5,1.0},
                                         {0.980392157,0.843137255,0.0,1.0},
                                         {0.094117647,0.545098039,0.094117647,1.0}};

#define NB_COLORS 9
inline static const Color Colors[NB_COLORS] = {RED, ORANGE, YELLOW, GREEN, CYAN, BLUE, WHITE, MAGENTA, PINK};

class TermColor {
public:
    inline static const string Black = "\033[0;30m";
    inline static const string Red = "\033[0;31m";
    inline static const string Green = "\033[0;32m";
    inline static const string Yellow = "\033[0;33m";
    inline static const string Blue = "\033[0;34m";
    inline static const string Magenta = "\033[0;35m";
    inline static const string Cyan = "\033[0;36m";
    inline static const string White = "\033[0;37m";

    //!< Bright Black
    inline static const string BBlack = "\033[1;30m";
    //!< Bright Red
    inline static const string BRed = "\033[1;31m";
    //!< Bright Green
    inline static const string BGreen = "\033[1;32m";
    //!< Bright Yellow
    inline static const string BYellow = "\033[1;33m";
    //!< Bright Blue
    inline static const string BBlue = "\033[1;34m";
    //!< Bright Magenta
    inline static const string BMagenta = "\033[1;35m";
    //!< Bright Cyan
    inline static const string BCyan = "\033[1;36m";
    //!< Bright White
    inline static const string BWhite = "\033[1;37m";

    //!< Background Black
    inline static const string BG_Black = "\033[0;40m";
    inline static const string BG_Red = "\033[0;41m";
    inline static const string BG_Green = "\033[0;42m";
    inline static const string BG_Yellow = "\033[0;43m";
    inline static const string BG_Blue = "\033[0;44m";
    inline static const string BG_Magenta = "\033[0;45m";
    inline static const string BG_Cyan = "\033[0;46m";
    inline static const string BG_White = "\033[0;47m";

    inline static const string BG_BBlack = "\033[1;40m";
    inline static const string BG_BRed = "\033[1;41m";
    inline static const string BG_BGreen = "\033[1;42m";
    inline static const string BG_BYellow = "\033[1;43m";
    inline static const string BG_BBlue = "\033[1;44m";
    inline static const string BG_BMagenta = "\033[1;45m";
    inline static const string BG_BCyan = "\033[1;46m";
    inline static const string BG_BWhite = "\033[1;47m";

    //!< Reset Tag
    inline static const string Reset = "\033[0m";

    //<! Color Output Configuration
    inline static const string SchedulerColor = BGreen;
    inline static const string LifecycleColor = BYellow;
    inline static const string ErrorColor = BRed;
};

#endif // COLOR_H_
