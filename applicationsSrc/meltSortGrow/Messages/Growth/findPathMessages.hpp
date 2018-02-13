/**
 * @file   findPathMessages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Tue Feb 13 10:10:28 2018
 * 
 * @brief  Messages used for finding a path to the next target position to be filled during the growth phase.
 */

#ifndef FIND_PATH_MESSAGES_H_
#define FIND_PATH_MESSAGES_H_

#include "../../meltSortGrowBlockCode.hpp"
#include "../meltSortGrowMessage.hpp"
#include "../../pathHop.hpp"

class FindPathMessage : public MeltSortGrowMessage {
    Cell3DPosition goalPosition; /// Position we are trying to find an adjacent module to
public:
    FindPathMessage(Cell3DPosition pos) : goalPosition(pos) {
        type = MSG_GROW_FINDPATH;
    };
    
    virtual ~FindPathMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindPathMessage(*this); }
    virtual string getName() { return "findPath"; }
};

/** 
 * @brief This messages is propagated from a module adjacent to the target position to reach, all the way to the end of the tail - i.e., the module that will move. A path is incrementally built along the way. 
 * @attention We need to make sure that there is always a connector path from that module to the target position (maybe this can only be enforce by a construction ordering)
 */
class FindPathFoundMessage : public MeltSortGrowMessage {
    vector<PathHop> path; 
public:
    FindPathFoundMessage(vector<PathHop> _path) : path(_path) {
        type = MSG_GROW_FINDPATH_FOUND;
    };
    virtual ~FindPathFoundMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindPathFoundMessage(*this); }
    virtual string getName() { return "FindPathFound"; }
};

class FindPathIgnoreMessage : public MeltSortGrowMessage {
public:
    FindPathIgnoreMessage() {
        type = MSG_GROW_FINDPATH_IGNORE;
    };
    
    virtual ~FindPathIgnoreMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindPathIgnoreMessage(*this); }
    virtual string getName() { return "FindPathIgnore"; }
};


class FindPathNotFoundMessage : public MeltSortGrowMessage {
public:
    FindPathNotFoundMessage() {
        type = MSG_GROW_FINDPATH_NOTFOUND;
    };
    
    virtual ~FindPathNotFoundMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindPathNotFoundMessage(*this); }
    virtual string getName() { return "FindPathNotFound"; }
};


class GrowNextModuleMessage : public MeltSortGrowMessage {
public:
    GrowNextModuleMessage() { type = MSG_GROW_NEXTMODULE; };
    
    virtual ~GrowNextModuleMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new GrowNextModuleMessage(*this); }
    virtual string getName() { return "GrowNextModule"; }
};

#endif /* FIND_PATH_MESSAGES_H_ */
