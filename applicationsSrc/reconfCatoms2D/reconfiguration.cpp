#include "reconfiguration.h"

Reconfiguration::Reconfiguration() {
  state = NOT_WELL_PLACED;
}

Reconfiguration::Reconfiguration(Reconfiguration const &r) {
  state = r.state;
}

Reconfiguration::~Reconfiguration() { }

void Reconfiguration::setState(state_t s) {
  state = s;
}

void Reconfiguration::tryToMove() {

}
