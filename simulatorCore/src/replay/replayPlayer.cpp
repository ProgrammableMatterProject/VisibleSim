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

#include "replayTags.h"

#include "../utils/utils.h"
#include "../base/simulator.h"

using namespace ReplayTags;
namespace Replay {

ReplayPlayer::ReplayPlayer(int argc, char *argv[]):cmdLine(argc,argv) {

    cout << TermColor::BWhite
         << "(replay) exporting simulation data to file: " << TermColor::Reset
         << fnbin << endl;
}

void ReplayPlayer::createPlayer(int argc, char *argv[]) {
    cout << "creating Replay"<< endl;
    replayPlayer =  new ReplayPlayer(argc, argv;

}

}
