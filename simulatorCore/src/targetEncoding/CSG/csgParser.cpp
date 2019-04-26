#include "csgParser.h"

const size_t CSGParser::keywordsCount = 12;
const string CSGParser::keywords[] = {"union","intersection","difference","translate","rotate","scale","cube","cylinder","sphere","color","{","}"};

void CSGParser::writeType(stringstream &buffer, CSG_T t) {
    buffer.write((char *)&t, sizeof(CSG_T));
}

void CSGParser::writeChar(stringstream &buffer, char c) {
    buffer.write((char *)&c, sizeof(char));
}

void CSGParser::writeFloat(stringstream &buffer, float f) {
    buffer.write((char *)&f, sizeof(float));
}

double CSGParser::readExpression(string expr) {
    try {
        mu::Parser p;
        p.SetExpr(expr);
        return p.Eval();
    } catch (mu::Parser::exception_type &e) {
        cout << e.GetMsg() << endl;
    }
    return 0;
}

size_t CSGParser::readParameters(string line, size_t initialPos, double &param1, double &param2, double &param3) {
    // get parameter from one character after ( until )
    size_t beginKeyword = line.find_first_of("(", initialPos);
    size_t endKeyword = line.find_first_of(")", initialPos);
    string parameters = line.substr(beginKeyword+1, endKeyword-beginKeyword);

    // get rid of [
    size_t square_bracket = parameters.find_first_of("[");
    if (square_bracket != string::npos && square_bracket < parameters.find_first_of(")"))
        parameters = parameters.substr(square_bracket+1);

    string expr = parameters.substr(0, parameters.find_first_of(','));
    param1 = readExpression(expr);
    parameters = parameters.substr(parameters.find_first_of(',')+1);
    expr = parameters.substr(0, parameters.find_first_of(','));
    param2 = readExpression(expr);
    parameters = parameters.substr(parameters.find_first_of(',')+1);
    expr = parameters.substr(0, parameters.find_first_of(",)]"));
    param3 = readExpression(expr);
    return endKeyword;
}

size_t CSGParser::readParameters(string line, size_t initialPos, double &param1) {
    // get parameter from one character after ( until )
    size_t beginKeyword = line.find_first_of("(", initialPos);
    size_t endKeyword = line.find_first_of(")", initialPos);
    string parameters = line.substr(beginKeyword+1, endKeyword-beginKeyword);

    // get rid of [
    size_t square_bracket = parameters.find_first_of("[");
    if (square_bracket != string::npos && square_bracket < parameters.find_first_of(")"))
        parameters = parameters.substr(square_bracket+1);

    string expr = parameters.substr(0, parameters.find_first_of(",)]"));
    param1 = readExpression(expr);
    return endKeyword;
}

size_t CSGParser::readKeyword(string line, size_t initialPos, size_t &keywordId) {
    size_t beginKeyword = line.find_first_not_of(" );\t\n", initialPos);
    if (beginKeyword == string::npos)
        return string::npos;
    size_t endKeyword = line.find_first_of(" (\t", beginKeyword);
    if (line[beginKeyword] == '{') {
        keywordId = 10;
        return beginKeyword;
    }
    if (line[beginKeyword] == '}') {
        keywordId = 11;
        return beginKeyword;
    }
    string keyword = line.substr(beginKeyword, endKeyword-beginKeyword);

    keywordId = -1;
    for (size_t i = 0; i < keywordsCount; i++)
        if (keyword.find(keywords[i]) != string::npos)
            keywordId = i;

    return endKeyword;
}

char* CSGParser::parseCsg(string str) {
	stringstream csgFile(str);
	stringstream csgFileBin;

	string line;
	size_t pos, keywordId;

	CSGParser parser;
	while(getline(csgFile, line)) {
		pos = 0;
		while((pos = parser.readKeyword(line, pos, keywordId)) != string::npos) {
			switch(keywordId) {
				case 0: { // union
					pos = line.find_first_of(")", pos);
#ifdef DEBUG_CSG
	cout << "union() " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Union);
				} break;
				case 1: { // intersection
					pos = line.find_first_of(")", pos);
#ifdef DEBUG_CSG
	cout << "intersection() " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Intersection);
				} break;
				case 2: { //difference
					pos = line.find_first_of(")", pos);
#ifdef DEBUG_CSG
	cout << "difference() " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Difference);
				} break;
				case 3: { //ŧranslate
					double p1, p2, p3;
					pos = parser.readParameters(line, pos, p1, p2, p3);
#ifdef DEBUG_CSG
	cout << "translate([" << p1 << "," << p2 << "," << p3 << "]) " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Translate);
					parser.writeFloat(csgFileBin, p1);
					parser.writeFloat(csgFileBin, p2);
					parser.writeFloat(csgFileBin, p3);
				} break;
				case 4: { //rotate
					double p1, p2, p3;
					pos = parser.readParameters(line, pos, p1, p2, p3);
#ifdef DEBUG_CSG
	cout << "rotate([" << p1 << "," << p2 << "," << p3 << "]) " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Rotate);
					parser.writeFloat(csgFileBin, p1);
					parser.writeFloat(csgFileBin, p2);
					parser.writeFloat(csgFileBin, p3);
				} break;
				case 5: { //scale
					double p1, p2, p3;
					pos = parser.readParameters(line, pos, p1, p2, p3);
#ifdef DEBUG_CSG
	cout << "scale([" << p1 << "," << p2 << "," << p3 << "]) " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Scale);
					parser.writeFloat(csgFileBin, p1);
					parser.writeFloat(csgFileBin, p2);
					parser.writeFloat(csgFileBin, p3);
				} break;
				case 6: { //cube
					double p1, p2, p3;
					pos = parser.readParameters(line, pos, p1, p2, p3);
#ifdef DEBUG_CSG
	cout << "cube([" << p1 << "," << p2 << "," << p3 << "],true); " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Cube);
					parser.writeFloat(csgFileBin, p1);
					parser.writeFloat(csgFileBin, p2);
					parser.writeFloat(csgFileBin, p3);
				} break;
				case 7: { //cylinder
					double p1, p2, p3;
					pos = parser.readParameters(line, pos, p1, p2, p3);
					parser.writeType(csgFileBin, CSG_T::Cylinder);
					parser.writeFloat(csgFileBin, p1);
					parser.writeFloat(csgFileBin, p2);
#ifdef DEBUG_CSG
	cout << "cylinder(" << p1 << "," << p2 << "," << p3 << ",true); " << endl;
#endif
					if (p2 != p3) cout << "ERROR Cylinder - " << p2 << ' ' << p3 << endl;
				} break;
				case 8: { //sphere
					double p1;
					pos = parser.readParameters(line, pos, p1);
#ifdef DEBUG_CSG
	cout << "sphere(" << p1 << "); " << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Sphere);
					parser.writeFloat(csgFileBin, p1);
				} break;
				case 9: { // color
					double p1, p2, p3;
					pos = parser.readParameters(line, pos, p1, p2, p3);
#ifdef DEBUG_CSG
	cout << "color([" << p1 << ',' << p2 << ',' << p3 << "])" << endl;
#endif
					parser.writeType(csgFileBin, CSG_T::Color);
					parser.writeChar(csgFileBin, (unsigned char)(p1*255));
					parser.writeChar(csgFileBin, (unsigned char)(p2*255));
					parser.writeChar(csgFileBin, (unsigned char)(p3*255));
				} break;
				case 10: { // {
					pos++;
#ifdef DEBUG_CSG
	cout << "{" << endl;
#endif
				} break;
				case 11: { // }
					pos++;
#ifdef DEBUG_CSG
	cout << "}\n";
#endif
					parser.writeType(csgFileBin, CSG_T::END);
				} break;
				default:{
					pos++;
				}
			}
		}
	}

	string strBin = csgFileBin.str();
	char* csgBin = new char[strBin.size()];
	memcpy(csgBin, strBin.c_str(), strBin.size());
	return csgBin;
}
