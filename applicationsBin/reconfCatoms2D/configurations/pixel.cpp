#include "pixel.hpp"

using namespace std;

Pixel::Pixel() {
  red = 0;
  green = 0;
  blue = 0;
  alpha = 0;
}

Pixel::Pixel(byte r, byte g, byte b, byte a) {
  red = r;
  green = g;
  blue = b;
  alpha = a;
}

Pixel::Pixel(const Pixel &p) {
  red = p.red;
  green = p.green;
  blue = p.blue;
  alpha = p.alpha;
}

Pixel::~Pixel() {}

ostream& operator<<(std::ostream &output, const Pixel &p) { 
  //output << "F : " << p.red << " I : " << D.inches;
  output << (int)p.red << "," <<
    (int)p.green << "," <<
    (int)p.blue; // << "," << (int) p.alpha;

  return output;            
}
