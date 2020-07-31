/**
 * @file   ReplayPlayer.cpp
 * @author Matteo Daluz
 * @brief Simulation replay application for simulation reconstruction
 * contains parsing methods and world generation
 */

#pragma once

#include <fstream>
#include <map>
#include "../../simulatorCore/src/utils/tDefs.h"
#include "../../simulatorCore/src/utils/exceptions.h"
#include "../../simulatorCore/src/grid/cell3DPosition.h"
#include "../../simulatorCore/src/utils/commandLine.h"
#include "../../simulatorCore/src/replay/replayTags.h"
#include "replayGlutContext.h"
#include "replayWorld.h"
#include "../../simulatorCore/src/grid/lattice.h"

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

    ReplayWorld *world = nullptr;
    //Simulation general parameters
    char * simulationType ;
    u1 robotType = 0;


    u8 keyframeIndexPosition = 0;
    u8 keyframeCount = 0;
    u8 keyframeEndTime = 0;
    u8 maxMotionDuration = 0;
    u8 lastFrameEndParsePosition = 0;

    CommandLine cmdLine;

//    float replayDuration = 25.0f;
//    float currentTime = 0.0f;

public:

    int gridSizeX = 0, gridSizeY = 0, gridSizeZ = 0;
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
     * Read Keyframe Index to return the position in the file of the last KeyFrame before the time given in parameter
     * @param time
     * @return
     */
    u8 findKeyframeWithTime(u8 time);

    /**
     * Read Keyframe Index to return the position in the file of the first KeyFrame after the time given in parameter
     * @param time
     * @return
     */
    u8 findNextKeyframe(u8 time);

    /**
     * Starts parsing of the frame at time @time
     * @param time
     */
    void parseFrame(u8 time);


    /**
     * Parse the keyframe which beginning position is given in parameter
     * @param position
     * @return Position of the end of the Keyframe
     */
    u8 parseKeyframe(u8 position);

    /**
     * Parse Keyframe index to show keyframes on the timeline
     * Updates ReplayGlutContext::keyframesTime
     */
    void parseKeyframeForTimeline();
    /**
     * Parse the events that starts at the position given in parameter until the time given or the end position
     * @param position : Start position of the events of current keyframe
     * @param time : Time of the selected frame
     * @param end : End position of current keyframe
     */
    void parseEvents(u8 position,u8 time, u8 end);

    void parseEventColor(u8 position, u4 blockId);

    void parseEventMotion(u8 position, u4 blockId, u8 time, u8 readTime);

    void parseEventMotionCatoms3D(u8 position, u4 blockId, u8 time, u8 readTime);

    void parseEventPosition(u8 position, u4 blockId);

    void parseEventDisplay(u8 position, u4 blockId);



    /**
     * Parse the total duration of the exported file
     * @return duration
     */
    u8 parseDuration();

    u8 getKeyframeEndTime();

    u8 getLastFrameEndParsePosition();

};



inline void createPlayer(int argc, char*argv[]){
    ReplayPlayer::createPlayer(argc, argv);
}

}
