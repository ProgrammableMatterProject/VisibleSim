#ifndef TARGA_H_
#define TARGA_H_

#include <string>

class Targa {
  std::string input;
  unsigned char *buffer;
  int width;
  int height;
  
 public:
  Targa(std::string i);
  Targa(const Targa &t);
  ~Targa();
  
  void read();
  std::ostream& exportToVisibleSim(std::ostream &output);
  // export to VisibleSim format
  //friend std::ostream &operator<<(std::ostream &output, const Targa &t);
};


#endif
