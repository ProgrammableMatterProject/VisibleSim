#include <iostream>
#include "rate.h"

using namespace std;
using namespace BaseSimulator::utils;

namespace BaseSimulator {

Rate::Rate() {}

Rate::Rate(const Rate &r) {}

Rate::~Rate() {}

StaticRate::StaticRate() : Rate(){
  value = 0.0;
}

StaticRate::StaticRate(double v) : Rate() {
  value = v;
}

StaticRate::StaticRate(const StaticRate &sr) : Rate(sr) {
  value = sr.value;
}

StaticRate::~StaticRate() {}

double StaticRate::get() {
  return value;
}

RandomRate::RandomRate() {
  generator = Random::getUniformDoubleRNG(0,0,0);
}

RandomRate::RandomRate(doubleRNG &g) {
  generator = g;
}

RandomRate::RandomRate(const RandomRate &rr) : Rate(rr) {
  generator = rr.generator;
}

RandomRate::~RandomRate() {

}

double RandomRate::get() {
  return generator();
}

}
