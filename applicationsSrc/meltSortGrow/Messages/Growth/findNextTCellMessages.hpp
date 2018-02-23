/**
 * @file   findNextTCellMessages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Thu Feb 22 10:03:14 2018
 * 
 * @brief  
 * 
 * 
 */

#ifndef FIND_NEXT_TCELL_MESSAGES_H_
#define FIND_NEXT_TCELL_MESSAGES_H_

#include "../../meltSortGrowBlockCode.hpp"
#include "../meltSortGrowMessage.hpp"
#include "../../pathHop.hpp"

class FindNextTCellMessage : public MeltSortGrowMessage {
    list<PathHop> path;
    Cell3DPosition goalPosition;
public:
    FindNextTCellMessage(list<PathHop> _path, Cell3DPosition& _pos)
        : path(_path), goalPosition(_pos) {
        type = MSG_GROW_BUILDPATH;
    };
    
    virtual ~FindNextTCellMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindNextTCellMessage(*this); }
    virtual string getName() { return "FindNextTCell"; }
};

class FindNextTCellSuccessMessage : public MeltSortGrowMessage {
    list<PathHop> path;
public:
    FindNextTCellSuccessMessage(list<PathHop> _path)
        : path(_path) {
        type = MSG_GROW_BUILDPATH_SUCCESS;
    };
    virtual ~FindNextTCellSuccessMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindNextTCellSuccessMessage(*this); }
    virtual string getName() { return "FindNextTCellSuccess"; }
};

class FindNextTCellIgnoreMessage : public MeltSortGrowMessage {
public:
    FindNextTCellIgnoreMessage() {
        type = MSG_GROW_BUILDPATH_IGNORE;
    };
    virtual ~FindNextTCellIgnoreMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindNextTCellIgnoreMessage(*this); }
    virtual string getName() { return "FindNextTCellIgnore"; }
};

class FindNextTCellFailureMessage : public MeltSortGrowMessage {
public:
    FindNextTCellFailureMessage() {
        type = MSG_GROW_BUILDPATH_FAILURE;
    };
    virtual ~FindNextTCellFailureMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new FindNextTCellFailureMessage(*this); }
    virtual string getName() { return "FindNextTCellFailure"; }
};

#endif /* FIND_NEXT_TCELL_MESSAGES_H_ */
