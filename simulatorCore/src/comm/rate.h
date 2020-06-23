#ifndef RATE_H_
#define RATE_H_

#include "../utils/random.h"

namespace BaseSimulator {

     class Rate {
     public:
          Rate();
          Rate(const Rate &r);
          virtual ~Rate();

          virtual double get() = 0;
     };

     class StaticRate : public Rate {
     protected:
          double value;
     public:
          StaticRate();
          StaticRate(double v);
          StaticRate(const StaticRate &sr);
          ~StaticRate();

          double get() override;
     };

     class RandomRate : public Rate {
     protected:
          doubleRNG generator;
     public:
          RandomRate();
          RandomRate(doubleRNG &g);
          RandomRate(const RandomRate &rr);
          ~RandomRate();

          double get() override;
     };

}
#endif
