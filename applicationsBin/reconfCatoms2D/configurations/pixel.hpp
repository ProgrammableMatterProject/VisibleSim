#ifndef PIXEL_H_
#define PIXEL_H_

#include <iostream>
#include <cstdint>

typedef uint8_t byte;

class Pixel {
public:
  byte red;
  byte green;
  byte blue;
  byte alpha;

  Pixel();
  Pixel(byte r, byte g, byte b, byte a);
  Pixel(const Pixel &p);
  ~Pixel();
  friend std::ostream &operator<<(std::ostream &output, const Pixel &p);
};

#endif
