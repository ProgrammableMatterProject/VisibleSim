/**
 * @file   ReplayPlayer.cpp
 * @author Matteo Daluz
 * @date   Tue Jun  9 11:14:00 2020
 *
 * @brief Simulation replay application for simulation reconstruction
 *
 *
 */

#include <algorithm>
#include "replayPlayer.h"

#include "../../simulatorCore/src/replay/replayTags.h"

#include "../../simulatorCore/src/utils/utils.h"
#include "../../simulatorCore/src/base/simulator.h"
#include <fstream>

using namespace ReplayTags;
namespace Replay {

ReplayPlayer::ReplayPlayer(int argc, char *argv[]):cmdLine(argc,argv,NULL) {

    cout << "instanciating ReplayPlayer" << endl;

    cout << "Loading export file .. " << flush;
    string replayFileName = cmdLine.getReplayFilename();
    exportFile = new ifstream(replayFileName,ifstream::in | ifstream::binary);
    cout << "Done" << endl;

    parseHeader();
    parseInitialConfiguration();
    exportFile->close();
}

void ReplayPlayer::createPlayer(int argc, char *argv[]) {
    cout << "creating Replay"<< endl;
    replayPlayer =  new ReplayPlayer(argc, argv);

}

void ReplayPlayer::parseHeader()
{
    cout <<"Parsing header .."<<flush;

    simulationType = new char[sizeof(u4)] ;
    exportFile->seekg(0,ios::beg);
    exportFile->read((char*)simulationType,sizeof(u4));
    exportFile->read((char*)&robotType,sizeof(u1));
    exportFile->read((char*)&gridSizeX,sizeof(u2));
    exportFile->read((char*)&gridSizeY,sizeof(u2));
    exportFile->read((char*)&gridSizeZ,sizeof(u2));

    cout <<"Done"<<endl;

    cout << "simulation Type : "<< simulationType << endl;

    string strRobotType;
    switch(robotType)
    {
        case MODULE_TYPE_BB:
            strRobotType = "BlinkyBlocks";
            break;
        case MODULE_TYPE_C2D:
            strRobotType = "Catom2D";
            break;
        case MODULE_TYPE_C3D:
            strRobotType = "Catom3D";
            break;
        case MODULE_TYPE_DATOM:
            strRobotType = "Datoms";
            break;
        case MODULE_TYPE_HEXANODE:
            strRobotType = "Hexanode";
            break;
        case MODULE_TYPE_NODE2D:
            strRobotType = "Node2D";
            break;
        case MODULE_TYPE_OKTEEN:
            strRobotType = "Okteens";
            break;
        case MODULE_TYPE_SLIDINGCUBE:
            strRobotType = "SlindingCubes";
            break;
        case MODULE_TYPE_SMARTBLOCKS:
            strRobotType = "SmartBlocks";
            break;
        default:
            strRobotType = "Erreur";
    }

    cout << "Robot Type : "<< strRobotType << endl;
    cout << "Grid Size : "<< gridSizeX
         << " " << gridSizeY
         << " " << gridSizeZ <<endl;
}

void ReplayPlayer::parseInitialConfiguration()
{
    cout << "Parsing initial configuration .." <<flush;
    exportFile->seekg(headerSize);
    int blockCount = 0;
    exportFile->read((char*)&blockCount,sizeof(u4));
    KeyFrameBlock* keyFrame = new KeyFrameBlock[blockCount];
    for(int i=0;i<blockCount;i++)
    {
        exportFile->read((char*)&keyFrame[i].id,sizeof(u4));
        exportFile->read((char*)&keyFrame[i].x,sizeof(u2));
        exportFile->read((char*)&keyFrame[i].y,sizeof(u2));
        exportFile->read((char*)&keyFrame[i].z,sizeof(u2));
        exportFile->read((char*)&keyFrame[i].rotation,sizeof(u1));
        exportFile->read((char*)&keyFrame[i].r,sizeof(u1));
        exportFile->read((char*)&keyFrame[i].g,sizeof(u1));
        exportFile->read((char*)&keyFrame[i].b,sizeof(u1));

    }
    cout <<"Done"<<endl;

    //print results
    cout << "There are "<<blockCount<<" in the initial configuration"<<endl;
    for(int i=0;i<blockCount;i++)
    {
        cout << "id : "<<keyFrame[i].id
            <<" | Pos : "<<(int)keyFrame[i].x<<" "<<(int)keyFrame[i].y<<" "<<(int)keyFrame[i].z
            <<" | rotation : "<<(int)keyFrame[i].rotation
            <<" | RGB : "<<(int)keyFrame[i].r<<" "<<(int)keyFrame[i].g<<" "<<(int)keyFrame[i].b<<endl;
    }
}

}
