/*
 * robot02BlockCode.h
 *
 *  Created on: 12 janvier 2014
 *      Author: Benoît
 */

#ifndef ROBOT02BLOCKCODE_H_
#define ROBOT02BLOCKCODE_H_

#include "robots/slidingCubes/slidingCubesBlockCode.h"
#include "robots/slidingCubes/slidingCubesSimulator.h"

#include "robots/slidingCubes/slidingCubesBlock.h"

#define COLOR_MESSAGE	1001
#define SEARCH_MASTER_MESSAGE	1002
#define RETURN_MASTER_MESSAGE	1003

const int COM_DELAY=100000;

class SearchMasterMessage : public Message {
public :
    int blockId;
    Color blockColor;
    SearchMasterMessage(int i,Color c):blockId(i),blockColor(c) { type=SEARCH_MASTER_MESSAGE; };
    ~SearchMasterMessage() {};
};

class ReturnMasterMessage : public Message {
public :
    int blockId;
    Color blockColor;
    ReturnMasterMessage(int i,Color c):blockId(i),blockColor(c) { type=RETURN_MASTER_MESSAGE; };
    ~ReturnMasterMessage() {};
};

class ColorMessage : public Message {
public :
    Color color;
    ColorMessage(Color c):color(c) { type=COLOR_MESSAGE; };
    ~ColorMessage() {};
};

typedef std::shared_ptr<ColorMessage> ColorMessage_ptr;
typedef std::shared_ptr<SearchMasterMessage> SearchMasterMessage_ptr;
typedef std::shared_ptr<ReturnMasterMessage> ReturnMasterMessage_ptr;

using namespace std;
using namespace SlidingCubes;

class Robot02BlockCode : public SlidingCubes::SlidingCubesBlockCode {
    int masterId;
    Color masterColor;
    bool searchDone;
    int nbreOfWaitedAnswers;
    P2PNetworkInterface *block2answer;
    bool colored;
public:
    Scheduler *scheduler;
    SlidingCubes::SlidingCubesBlock *block;

    Robot02BlockCode (SlidingCubes::SlidingCubesBlock *host);
    ~Robot02BlockCode ();

    void startup() override;
    void processLocalEvent(EventPtr pev) override;

    static BlockCode *buildNewBlockCode(BuildingBlock *host) {
        return (new Robot02BlockCode((SlidingCubes::SlidingCubesBlock*)host));
    }

    void sendMasterMessageToAllNeighbors(P2PNetworkInterface *except);
    void sendReturnMessageTo(P2PNetworkInterface *p2p);
    void sendColorMessageToAllNeighbors(P2PNetworkInterface *except);
};

#endif /* ROBOT2BLOCKCODE_H_ */
