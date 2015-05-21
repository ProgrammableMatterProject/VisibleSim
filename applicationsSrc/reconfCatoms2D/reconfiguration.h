#ifndef RECONFIGURATION_H
#define RECONFIGURATION_H

class Reconfiguration {
 public:
  enum state_t {WELL_PLACED = 0, NOT_WELL_PLACED};
  
  state_t state;
  
  Reconfiguration();
  Reconfiguration(Reconfiguration const &r);
  ~Reconfiguration();

  void setState(state_t s);
  void tryToMove();
};

#endif
