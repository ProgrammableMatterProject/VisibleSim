/*
 * csgParser.h
 *
 *  Created on: 4 september 2015
 *      Author: Thadeu
 */

#ifndef CSGPARSER_H_
#define CSGPARSER_H_
#include <iostream>
#include <fstream>
#include <sstream>
#include <muParser.h>

using namespace std;

enum class CSG_T : unsigned char
{
    Difference = 0, Union = 1, Intersection, Translate, Scale, Rotate, Color, Cube, Cylinder, Sphere, END
};

class CSGParser
{
public:
    static const size_t keywordsCount;
    static const string keywords[];
    static char* parseCsg(string str); //TODO unique_ptr
private:
    void writeType(stringstream &buffer, CSG_T t);
    void writeChar(stringstream &buffer, char c);
    void writeFloat(stringstream &buffer, float f);
    double readExpression(string expr);
    size_t readParameters(string line, size_t initialPos, double &param1, double &param2, double &param3);
    size_t readParameters(string line, size_t initialPos, double &param1);
    size_t readKeyword(string line, size_t initialPos, size_t &keywordId);
};

#endif /* CSGPARSER_H_ */


