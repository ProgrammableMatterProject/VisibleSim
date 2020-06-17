/**
 * @file   ReplayPlayer.h
 * @author Matteo Daluz
 * @date   Tue Jun  9 11:13:33 2020
 *
 * @brief  Simulation replay application for simulation reconstruction
 *
 *
 */

#pragma once

#include <fstream>
#include <map>
#include "../../simulatorCore/src/utils/tDefs.h"
#include "../../simulatorCore/src/utils/color.h"
#include "../../simulatorCore/src/utils/exceptions.h"
#include "../../simulatorCore/src/base/buildingBlock.h"
#include "../../simulatorCore/src/grid/cell3DPosition.h"
#include "../../simulatorCore/src/utils/commandLine.h"
#include "../../simulatorCore/src/replay/replayTags.h"

using namespace std;
using namespace ReplayTags;
namespace Replay {

class ReplayPlayer;

extern ReplayPlayer *replayPlayer;
    /**
 * Simulation replay player that reads an export file for simulation reconstruction
 * @note To be used as a singleton instance
 */
class ReplayPlayer {
    static inline ReplayPlayer* replayPlayer = nullptr; //!< the singleton instance

    ifstream* exportFile = nullptr;     //!< binary export file
    ofstream* debugFile = nullptr;      //!< corresponding clear text export file for debugging
private:
    const streampos headerSize = 11*sizeof(u1);

    //Simulation general parameters
    char * simulationType ;
    u1 robotType = 0;
    int gridSizeX = 0, gridSizeY = 0, gridSizeZ = 0;

    CommandLine cmdLine;
public:


    ReplayPlayer(int argc, char *argv[]);
    virtual ~ReplayPlayer() {}


    static void createPlayer(int argc, char*argv[]);

    void parseHeader();

    void parseInitialConfiguration();


};



inline void createPlayer(int argc, char*argv[]){
    ReplayPlayer::createPlayer(argc, argv);
}

}
