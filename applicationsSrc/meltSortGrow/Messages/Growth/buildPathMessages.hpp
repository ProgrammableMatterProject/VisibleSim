/**
 * @file   buildPathMessages.hpp
 * @author pthalamy <pthalamy@p3520-pthalamy-linux>
 * @date   Wed Feb 21 10:44:45 2018
 * 
 * @brief  
 */

#ifndef BUILD_PATH_MESSAGES_H_
#define BUILD_PATH_MESSAGES_H_

#include "../../meltSortGrowBlockCode.hpp"
#include "../meltSortGrowMessage.hpp"
#include "../../pathHop.hpp"

class BuildPathMessage : public MeltSortGrowMessage {
    list<PathHop> path;
    // Cell3DPosition goalPosition;
public:
    BuildPathMessage(list<PathHop> _path)// , Cell3DPosition& _pos)
        : path(_path){// , goalPosition(_pos) {
        type = MSG_GROW_BUILDPATH;
    };
    
    virtual ~BuildPathMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new BuildPathMessage(*this); }
    virtual string getName() { return "FindConPath"; }
};

class BuildPathSuccessMessage : public MeltSortGrowMessage {
    list<PathHop> path;
    Cell3DPosition goalPosition;
public:
    BuildPathSuccessMessage(list<PathHop> _path, Cell3DPosition& _goalPosition)
        : path(_path), goalPosition(_goalPosition) {
        type = MSG_GROW_BUILDPATH_SUCCESS;
    };
    virtual ~BuildPathSuccessMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new BuildPathSuccessMessage(*this); }
    virtual string getName() { return "BuildPathSuccess"; }
};

class BuildPathIgnoreMessage : public MeltSortGrowMessage {
public:
    BuildPathIgnoreMessage() {
        type = MSG_GROW_BUILDPATH_IGNORE;
    };
    virtual ~BuildPathIgnoreMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new BuildPathIgnoreMessage(*this); }
    virtual string getName() { return "BuildPathIgnore"; }
};

class BuildPathFailureMessage : public MeltSortGrowMessage {
public:
    BuildPathFailureMessage() {
        type = MSG_GROW_BUILDPATH_FAILURE;
    };
    virtual ~BuildPathFailureMessage() {};

    virtual void handle(BaseSimulator::BlockCode*);
    virtual Message* clone() { return new BuildPathFailureMessage(*this); }
    virtual string getName() { return "BuildPathFailure"; }
};

#endif /* BUILD_PATH_MESSAGES_H_ */
