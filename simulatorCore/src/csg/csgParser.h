/*
 * csgParser.h
 *
 *  Created on: 4 september 2015
 *      Authors: Thadeu & bpiranda
 */

#ifndef CSGPARSER_H_
#define CSGPARSER_H_
#include <iostream>
#include <fstream>
#include <sstream>
#include <muParser.h>
#include <map>
#include "../math/vector3D.h"
#include "csg.h"

class CSGParserModule {
public :
    std::string moduleBody;
    std::vector<pair<string,double>> moduleParams;
    explicit CSGParserModule(const std::string &p_mb):moduleBody(p_mb) {};
    void addParamId(const std::string &p_id) {
        cout << "add:" << p_id << endl;
        moduleParams.push_back(pair<string,double>(p_id,0.0));
    }
    void addVariable(size_t num,double value,mu::Parser &mu_p) {
        cout << "affect:" << moduleParams[num].first << "=" << value << endl;
        moduleParams[num].second=value;
        mu_p.DefineVar(moduleParams[num].first, &(moduleParams[num].second));
    };
    void removeVariables(mu::Parser &mu_p) {
        for (auto mp:moduleParams) {
            mu_p.RemoveVar(mp.first);
        }
    }
};

class CSGParser {
public:
    CSGParser();
    ~CSGParser() {
        auto it=modules.begin();
        while (it!=modules.end()) {
            delete it->second;
            it++;
        }
        modules.clear();
    }
    CSGNode *parseCSG(const std::string &str) {
        std::size_t pos=0;
        return parseCSG(str,pos);
    }
    static const size_t keywordsCount;
    static const string keywords[];

private:
    std::map<string,double> variables;
    std::map<string,CSGParserModule*> modules;
    mu::Parser mu_p;

    CSGNode *parseCSG(const std::string &str,std::size_t &pos);
    void updateVariable(const std::string &instr);
    double readExpression(const std::string &expr);
    static std::size_t extractParametersString(const std::string &line, std::size_t pos, std::string &str);
    static std::size_t extractChildrenString(const std::string &line, std::size_t pos, std::string &str);
    std::size_t readCylinderParameters(const std::string &line, double &height, double &radiusBase, double &radiusTop, bool &center);
    std::size_t readCubeParameters(const std::string &line, Vector3D &v, bool &center);
    std::size_t readTorusParameters(const std::string &line, double &radius1, double &radius2);
    std::size_t readVector(const std::string &line, std::size_t pos1, std::size_t pos2, Vector3D &v);
    std::size_t readKeyword(const std::string &line, std::size_t initialPos, std::size_t &keywordId);
    std::size_t createModule(const std::string &line, std::size_t initialPos);
    std::size_t createLoop(const string &line, size_t initialPos,string &iteratorName,double &firstVal,double &lastVal,double &step,string &loopCode);
};

#endif /* CSGPARSER_H_ */
