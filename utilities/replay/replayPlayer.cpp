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

#include "replayWorld.h"
#include <fstream>
#include "../../simulatorCore/src/robots/smartBlocks/smartBlocksGlBlock.h"
#include "replayEvent.h"
#include "replayGlutContext.h"

using namespace ReplayTags;
using namespace GlutContext;
namespace Replay {

    ReplayPlayer::ReplayPlayer(int argc, char *argv[]) : cmdLine(argc, argv, NULL) {

        cout << "instanciating ReplayPlayer" << endl;

        cout << "Loading export file .. " << flush;
        string replayFileName = cmdLine.getReplayFilename();
        exportFile = new ifstream(replayFileName, ifstream::in | ifstream::binary);
        cout << "Done" << endl;

        parseHeader();
        parseKeyframeIndex();
        //parseInitialConfiguration();

//        parseKeyframe(findKeyframeWithTime(0));//0 seconds
//        parseKeyframe(findKeyframeWithTime(10000000));//10 seconds

        cout << "Initialising GlutContext.." << flush;
        GlutContext::ReplayGlutContext::init(argc, argv);

        GlutContext::ReplayGlutContext::camera->initFromGridSize(Vector3D(25*gridSizeX,25*gridSizeY,0));

        cout << "Done" << endl;

        cout << "Initializing World .." << flush;
        world = new ReplayWorld(argc,argv,parseDuration(),25.0f);
        world->player = this;
        cout << "Done" << endl;

        cout << "Setting up World .." << flush;
        GlutContext::ReplayGlutContext::setWorld(world);
        cout << "Done" << endl;

        GlutContext::ReplayGlutContext::mainLoop();
         // exportFile->close(); // jamais atteint -> à placer dans quit
    }

    void ReplayPlayer::createPlayer(int argc, char *argv[]) {
        cout << "creating Replay" << endl;
        replayPlayer = new ReplayPlayer(argc, argv);
    }

    void ReplayPlayer::parseHeader() {
        cout << "Parsing header .." << flush;

        simulationType = new char[sizeof(u4)];
        exportFile->seekg(0, ios::beg);
        exportFile->read((char *) simulationType, sizeof(u4));
        exportFile->read((char *) &robotType, sizeof(u1));
        exportFile->read((char *) &gridSizeX, sizeof(u2));
        exportFile->read((char *) &gridSizeY, sizeof(u2));
        exportFile->read((char *) &gridSizeZ, sizeof(u2));
        exportFile->read((char *) &keyframeIndexPosition, sizeof(u8));
        cout << "Done" << endl;

        cout << "simulation Type : " << simulationType << endl;

        string strRobotType;
        switch (robotType) {
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
                strRobotType = "SlidingCubes";
                break;
            case MODULE_TYPE_SMARTBLOCKS:
                strRobotType = "SmartBlocks";
                break;
            default:
                strRobotType = "Error";
        }
        cout << "Robot Type : " << strRobotType << endl;
        cout << "Grid Size : " << gridSizeX
             << " " << gridSizeY
             << " " << gridSizeZ << endl;
    }

    void ReplayPlayer::parseInitialConfiguration() {
        cout << "Parsing initial configuration .." << flush;
        exportFile->seekg(headerSize);
        int blockCount = 0;
        exportFile->read((char *) &blockCount, sizeof(u4));
        KeyframeBlock *keyframe = new KeyframeBlock[blockCount];
        for (int i = 0; i < blockCount; i++) {
            exportFile->read((char *) &keyframe[i].id, sizeof(u4));
            exportFile->read((char *) &keyframe[i].x, sizeof(u2));
            exportFile->read((char *) &keyframe[i].y, sizeof(u2));
            exportFile->read((char *) &keyframe[i].z, sizeof(u2));
            exportFile->read((char *) &keyframe[i].rotation, sizeof(u1));
            exportFile->read((char *) &keyframe[i].r, sizeof(u1));
            exportFile->read((char *) &keyframe[i].g, sizeof(u1));
            exportFile->read((char *) &keyframe[i].b, sizeof(u1));

        }
        cout << "Done" << endl;

        //print results
        cout << "There are " << blockCount << " blocks in the initial configuration" << endl;
        for (int i = 0; i < blockCount; i++) {
            cout << "id : " << keyframe[i].id
                 << " | Pos : " << (int) keyframe[i].x << " " << (int) keyframe[i].y << " " << (int) keyframe[i].z
                 << " | rotation : " << (int) keyframe[i].rotation
                 << " | RGB : " << (int) keyframe[i].r << " " << (int) keyframe[i].g << " " << (int) keyframe[i].b
                 << endl;
        }
    }

    void ReplayPlayer::parseKeyframeIndex() {
        cout << "Parsing keyframes index .." << flush;
        exportFile->seekg(keyframeIndexPosition);
        exportFile->read((char *) &keyframeCount, sizeof(u8));

        Keyframe *keyframes = new Keyframe[keyframeCount];
        for (u8 i = 0; i < keyframeCount; i++) {
            exportFile->read((char *) &keyframes[i].time, sizeof(u8));
            exportFile->read((char *) &keyframes[i].position, sizeof(u8));
        }
        cout << "Done" << endl;

        cout << "There are " << keyframeCount << " Key frames" << endl;
    }

/**
 * return the last keyframe before time in parameter
 * @param time (in microseconds)
 */
    u8 ReplayPlayer::findKeyframeWithTime(u8 time) {
        exportFile->seekg(keyframeIndexPosition);
        exportFile->read((char *) &keyframeCount, sizeof(u8));
        Keyframe *keyframes = new Keyframe[keyframeCount];
        for (u8 i = 0; i < keyframeCount; i++) {
            exportFile->read((char *) &keyframes[i].time, sizeof(u8));
            exportFile->read((char *) &keyframes[i].position, sizeof(u8));

            // Optimisation possible (jeu du plus ou moins) si nécessaire
            if (keyframes[i].time > time) {
                return keyframes[i - 1].position;
            }
        }
        return keyframes[keyframeCount - 1].position;
    }

    u8 ReplayPlayer::findNextKeyframe(u8 time) {
        exportFile->seekg(keyframeIndexPosition);
        exportFile->read((char *) &keyframeCount, sizeof(u8));
        Keyframe *keyframes = new Keyframe[keyframeCount];
        for (u8 i = 0; i < keyframeCount; i++) {
            exportFile->read((char *) &keyframes[i].time, sizeof(u8));
            exportFile->read((char *) &keyframes[i].position, sizeof(u8));

            // Optimisation possible (jeu du plus ou moins) si nécessaire
            if (keyframes[i].time > time) {
                keyframeEndTime = keyframes[i].time;
                return keyframes[i].position;
            }
        }
        return keyframeIndexPosition;
    }
    void ReplayPlayer::parseFrame(u8 time) {
        u8 keyframePosition = findKeyframeWithTime(time);
        u8 position = parseKeyframe(keyframePosition);
        u8 endPosition = findNextKeyframe(time);
        parseEvents(position,time,endPosition);
    }

    u8 ReplayPlayer::parseKeyframe(u8 position) {
        cout << "Parsing keyframe .." << flush;
        exportFile->seekg(position);
        int blockCount = 0;
        exportFile->read((char *) &blockCount, sizeof(u4));
        KeyframeBlock *keyframe = new KeyframeBlock[blockCount];
        for (int i = 0; i < blockCount; i++) {
            exportFile->read((char *) &keyframe[i].id, sizeof(u4));
            exportFile->read((char *) &keyframe[i].x, sizeof(u2));
            exportFile->read((char *) &keyframe[i].y, sizeof(u2));
            exportFile->read((char *) &keyframe[i].z, sizeof(u2));
            exportFile->read((char *) &keyframe[i].rotation, sizeof(u1));
            exportFile->read((char *) &keyframe[i].r, sizeof(u1));
            exportFile->read((char *) &keyframe[i].g, sizeof(u1));
            exportFile->read((char *) &keyframe[i].b, sizeof(u1));
            if(robotType == MODULE_TYPE_SMARTBLOCKS)
            {
                exportFile->read((char *) &keyframe[i].displayedValue, sizeof(u2));
            }
        }
        cout << "Done" << endl;

        //print results
        cout << "There are " << blockCount << " blocks in the keyframe" << endl;
        for (int i = 0; i < blockCount; i++) {
            cout << "id : " << keyframe[i].id
                 << " | Pos : " << (int) keyframe[i].x << " " << (int) keyframe[i].y << " " << (int) keyframe[i].z
                 << " | rotation : " << (int) keyframe[i].rotation
                 << " | RGB : " << (int) keyframe[i].r << " " << (int) keyframe[i].g << " " << (int) keyframe[i].b
                 << endl;
        }

        cout <<"Done"<<endl;

        //print results
        cout << "There are "<<blockCount<<" blocks in the keyframe"<<endl;
        for(int i=0;i<blockCount;i++)
        {
    //        cout << "id : "<<keyframe[i].id
    //             <<" | Pos : "<<(int)keyframe[i].x<<" "<<(int)keyframe[i].y<<" "<<(int)keyframe[i].z
    //             <<" | rotation : "<<(int)keyframe[i].rotation
    //             <<" | RGB : "<<(int)keyframe[i].r<<" "<<(int)keyframe[i].g<<" "<<(int)keyframe[i].b<<endl;
            world->addBlock(keyframe[i].id,keyframe[i]);
        }
        return (u8)exportFile->tellg();
    }

    void ReplayPlayer::parseKeyframeForTimeline()
    {
        exportFile->seekg(keyframeIndexPosition);
        exportFile->read((char *) &keyframeCount, sizeof(u8));

        Keyframe *keyframes = new Keyframe[keyframeCount];
        for (u8 i = 0; i < keyframeCount; i++) {
            exportFile->read((char *) &keyframes[i].time, sizeof(u8));
            ReplayGlutContext::keyframesTime.push_back(keyframes[i].time*pow(10,-6));
            exportFile->read((char *) &keyframes[i].position, sizeof(u8));
        }
    }

    void ReplayPlayer::parseEvents(u8 position,u8 time, u8 end) {
        cout << "Parsing events .." << flush;
        exportFile->seekg(position);
        u8 readTime = 0;
        u1 eventType;
        u4 blockId;
        while(true)
        {
            lastFrameEndParsePosition = exportFile->tellg();
            if(exportFile->tellg()>end) {break;}
            exportFile->read((char *) &readTime, sizeof(u8));
            if(readTime>time){break;}
            exportFile->read((char *) &eventType, sizeof(u1));
            exportFile->read((char *) &blockId, sizeof(u4));
            switch(eventType){
                case EVENT_COLOR_UPDATE:
                    parseEventColor(exportFile->tellg(), blockId);
                    break;
                case EVENT_DISPLAY_UPDATE:
                    parseEventDisplay(exportFile->tellg(),blockId);
                    break;
                case EVENT_POSITION_UPDATE:
                    parseEventPosition(exportFile->tellg(), blockId);
                    break;
                case EVENT_ADD_MODULE:
                    break;
                case EVENT_REMOVE_MODULE:
                    break;
                case EVENT_MOTION:
                    parseEventMotion(exportFile->tellg(),blockId,time,readTime);
                    break;
                case EVENT_CONSOLE_TRACE:
                    break;
            }
        }
        cout << "Done" << endl;

    }

    void ReplayPlayer::parseEventDisplay(u8 position, u4 blockId)
    {
        exportFile->seekg(position);
        uint16_t displayedValue;
        exportFile->read((char *) &displayedValue, sizeof(u2));
        world->updateDisplayedValue(blockId,displayedValue);
    }

    void ReplayPlayer::parseEventPosition(u8 position, u4 blockId)
    {
        exportFile->seekg(position);
        KeyframeBlock block;
        Vector3D newPosition;
        exportFile->read((char *) &block.x, sizeof(u2));
        exportFile->read((char *) &block.y, sizeof(u2));
        exportFile->read((char *) &block.z, sizeof(u2));
        exportFile->read((char *) &block.rotation, sizeof(u1));

        world->updatePosition(blockId,block);
    }
    void ReplayPlayer::parseEventMotion(u8 position, u4 blockId, u8 time, u8 readTime)
    {
        //Tracing block 36
        if(blockId==36)
        {
            cout << "Parsing Motion Event for Block : "<<blockId<<endl;
        }

        exportFile->seekg(position);
        KeyframeBlock block;
        u8 endTime;
        exportFile->read((char *) &endTime, sizeof(u8));
        exportFile->read((char *) &block.x, sizeof(u2));
        exportFile->read((char *) &block.y, sizeof(u2));
        exportFile->read((char *) &block.z, sizeof(u2));
        maxMotionDuration = max(endTime, maxMotionDuration);
        if(time>=readTime+2000)
        {
            if(time<=readTime+endTime)
            {
                ReplayEvent newEvent;
                Cell3DPosition pos, initPos;
                pos.pt[0] = block.x;
                pos.pt[1] = block.y;
                pos.pt[2] = block.z;
                newEvent.beginDate = readTime;
                newEvent.duration = endTime;
                newEvent.destinationPosition = pos;
                initPos.pt[0] = world->getPosition(blockId).pt[0];
                initPos.pt[1] = world->getPosition(blockId).pt[1];
                initPos.pt[2] = world->getPosition(blockId).pt[2];
                newEvent.initialPosition = initPos;
                world->eventBuffer.insert(make_pair(blockId,newEvent));
                world->updatePositionMotion(blockId,block, time, readTime, initPos);
            }
            else
            {
                world->updatePosition(blockId,block);
            }
        }


    }
    void ReplayPlayer::parseEventColor(u8 position, u4 blockId)
    {
        exportFile->seekg(position);
        KeyframeBlock block;
        Color col;
        exportFile->read((char *) &block.r, sizeof(u1));
        exportFile->read((char *) &block.g, sizeof(u1));
        exportFile->read((char *) &block.b, sizeof(u1));
        col.rgba[0] = (GLfloat) block.r/255.0f;
        col.rgba[1] = (GLfloat) block.g/255.0f;
        col.rgba[2] = (GLfloat) block.b/255.0f;
        col.rgba[3] = 1.0f;
        world->updateColor(blockId,col);
    }
    u8 ReplayPlayer::parseDuration()
    {
        u8 duration;
        exportFile->seekg(0,exportFile->end);
        exportFile->seekg(exportFile->tellg()-8);
        cout <<"OMGG"<< exportFile->tellg()<<endl;
        exportFile->read((char*)&duration,sizeof(u8));
        cout <<"DEBUG : "<<duration<<endl;
        return duration;

    }

    u8 ReplayPlayer::getKeyframeEndTime()
    {
        return keyframeEndTime;
    }
    u8 ReplayPlayer::getLastFrameEndParsePosition()
    {
        return lastFrameEndParsePosition;
    }
}


