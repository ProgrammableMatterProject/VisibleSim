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
#include "../utils/tDefs.h"
#include "../utils/color.h"
#include "../utils/exceptions.h"
#include "../base/buildingBlock.h"
#include "../grid/cell3DPosition.h"
#include "../utils/commandLine.h"


using namespace std;
namespace Replay {

    /**
 * Simulation replay player that reads an export file for simulation reconstruction
 * @note To be used as a singleton instance
 */
class ReplayPlayer {
    static inline ReplayPlayer* singleton = nullptr; //!< the singleton instance

    ofstream* exportFile = nullptr;     //!< binary export file
    ofstream* debugFile = nullptr;      //!< corresponding clear text export file for debugging

public:


    ReplayPlayer();
    virtual ~ReplayPlayer() {}
    CommandLine cmdLine;

    /**
     * Singleton getter
     * @return the singleton instance
     */
    static ReplayPlayer* getInstance() {
        if (not singleton)
            singleton = new ReplayPlayer();

        return singleton;
    }

    static void createPlayer(int argc, char*argv[]);

};



inline void createPlayer(int argc, char*argv[]){
    ReplayPlayer::createPlayer(argc, argv);
}

}