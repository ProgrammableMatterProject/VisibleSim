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
#include "../../simulatorCore/src/gui/openglViewer.h"
#include "../../simulatorCore/src/robots/smartBlocks/smartBlocksWorld.h"

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
private:
    const streampos headerSize = 19*sizeof(u1);

    //Simulation general parameters
    World *world = nullptr;
    char * simulationType ;
    u1 robotType = 0;
    int gridSizeX = 0, gridSizeY = 0, gridSizeZ = 0;

    u8 keyframeIndexPosition = 0;
    u8 keyframeCount = 0;

    CommandLine cmdLine;
public:


    ReplayPlayer(int argc, char *argv[]);
    virtual ~ReplayPlayer() {}

    /**
     * calls ReplayPlayer constructor method
     */
    static void createPlayer(int argc, char*argv[]);

    /**
     * Read export file's header and store global variables
     */
    void parseHeader();

    /**
     * [deprecated] Read first KeyFrame
     */
    void parseInitialConfiguration();

    /**
     * Read KeyFrame index
     */
    void parseKeyframeIndex();

    /**
     * Read Keyframe Index to return the position in the file of the last KeyFrame before the time given in parameter
     * @param time
     * @return
     */
    u8 findKeyframeWithTime(u8 time);

    /**
     * Parse the keyframe which beginning position is given in parameter
     * @param position
     */
    void parseKeyframe(u8 position);
};



inline void createPlayer(int argc, char*argv[]){
    ReplayPlayer::createPlayer(argc, argv);
}

}
