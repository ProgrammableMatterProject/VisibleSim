#ifndef DIRECTIONS_H_
#define DIRECTIONS_H_

enum SIDE_COMPLETED { LEFT, RIGHT };
enum SIDE_DIRECTION { TO_LEFT, TO_RIGHT };
enum LINE_DIRECTION { TO_NEXT = 1, TO_PREVIOUS = -1};
enum DIRECTION {DIRECTION_UP, DIRECTION_DOWN, DIRECTION_LEFT, DIRECTION_RIGHT};

class SyncRoute {
public:
    DIRECTION direction;
    bool rightSeedVisited;
    bool leftSeedVisited;
    bool nextSeedVisited;
    bool parentVisited;

    SyncRoute() {
        rightSeedVisited = false;
        leftSeedVisited = false;
        nextSeedVisited = false;
        parentVisited = false;
    }
};
#endif /* DIRECTIONS_H_ */
