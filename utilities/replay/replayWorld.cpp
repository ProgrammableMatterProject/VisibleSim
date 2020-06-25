#include <iostream>
#include "replayWorld.h"
#include "replay.hpp"
#include "../../simulatorCore/src/gui/objLoader.h"

using namespace std;
using namespace Replay;

ReplayWorld::ReplayWorld(int argc, char *argv[])
{
    cout << "Debug Replay World "<<endl;
    objBlock = new ObjLoader::ObjLoader("../../simulatorCore/resources/textures/smartBlocksTextures",
                                        "smartBlockSimple.obj");
    //GlutContext::ReplayGlutContext::mainLoop();
}

ReplayWorld::~ReplayWorld()
{

}
