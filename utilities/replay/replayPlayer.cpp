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

using namespace ReplayTags;
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
        cout << "Done" << endl;


        cout <<"Initializing World .."<<flush;
        world = new ReplayWorld(argc,argv,parseDuration());
        world->player = this;
        cout <<"Done"<<endl;

        cout << "Setting up World .." << flush;
        GlutContext::ReplayGlutContext::setWorld(world);
        cout << "Done" << endl;

        GlutContext::ReplayGlutContext::mainLoop();
         // exportFile->close(); // jamais atteint -> placer dans quit
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
                strRobotType = "SlindingCubes";
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

            //TODO changer les coordonnées avec gridToWorld
            Vector3D position;
            Color col;
            position.pt[3] = 1;
            position.pt[0] = keyframe[i].x*25;
            position.pt[1] = keyframe[i].y*25;
            position.pt[2] = keyframe[i].z*25;
            col.rgba[0] = keyframe[i].r/255.0f;
            col.rgba[1] = keyframe[i].g/255.0f;
            col.rgba[2]= keyframe[i].b/255.0f;
            col.rgba[3]= 1.0;
    //        cout << "id : "<<keyframe[i].id
    //             <<" | Pos : "<<(int)keyframe[i].x<<" "<<(int)keyframe[i].y<<" "<<(int)keyframe[i].z
    //             <<" | rotation : "<<(int)keyframe[i].rotation
    //             <<" | RGB : "<<(int)keyframe[i].r<<" "<<(int)keyframe[i].g<<" "<<(int)keyframe[i].b<<endl;
            world->addBlock(keyframe[i].id,position,col);
        }
        return (u8)exportFile->tellg();
    }

    void ReplayPlayer::parseEvents(u8 position,u8 time, u8 end) {
        cout << "Parsing events .." << flush;
        exportFile->seekg(position);
        u8 readTime = 0;
        u1 eventType;
        u4 blockId;
        KeyframeBlock block;
        Color col;
        while(true)
        {
            if(exportFile->tellg()>end) {break;}
            exportFile->read((char *) &readTime, sizeof(u8));
            if(readTime>time){break;}
            exportFile->read((char *) &eventType, sizeof(u1));
            exportFile->read((char *) &blockId, sizeof(u4));
            switch(eventType){
                case EVENT_COLOR_UPDATE:
                    cout <<"EVENEMENT DE COULEUR"<<endl;
                    cout <<"EVENEMENT DE DEBUG 1: "<<col.rgba[0]<<endl;
                    exportFile->read((char *) &block.r, sizeof(u1));
                    exportFile->read((char *) &block.g, sizeof(u1));
                    exportFile->read((char *) &block.b, sizeof(u1));
                    cout <<"EVENEMENT DE DEBUG 2: "<<col.rgba[0]<<endl;
                    col.rgba[0] = (GLfloat) block.r/255.0f;
                    col.rgba[1] = (GLfloat) block.g/255.0f;
                    col.rgba[2] = (GLfloat) block.b/255.0f;
                    col.rgba[3] = 1.0f;
                    cout <<"EVENEMENT DE DEBUG 3: "<<col.rgba[0]<<endl;
                    world->updateColor(blockId,col);
                    break;
                case EVENT_DISPLAY_UPDATE:
                    cout <<"EVENEMENT DE DISP"<<endl;
                    exportFile->seekg(exportFile->tellg()+2);
                    break;
                case EVENT_POSITION_UPDATE:
                    exportFile->seekg(exportFile->tellg()+7);
                    break;
                case EVENT_ADD_MODULE:
                    break;
                case EVENT_REMOVE_MODULE:
                    break;
                case EVENT_MOTION:
                    cout <<"EVENEMENT DE MOTION"<<endl;
                    exportFile->seekg(exportFile->tellg()+14);
                    break;
                case EVENT_CONSOLE_TRACE:
                    break;
            }
        }
        cout << "Done" << endl;

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
}
