#include <vector>
#include <climits>

#include "c2srMsg.h"

using namespace std;

// frequency vector
vector<unsigned int> C2SRMsg::hopCountStats;

void C2SRMsg::incrHopCountStats(unsigned int index) {
  // allocate if not available!
  if (index >= hopCountStats.size()) {
    hopCountStats.resize(index+1,0);
    assert(hopCountStats[index] == 0);
  }
  
  if (index > 1) {
    hopCountStats[index-1]--;
  }
  
  hopCountStats[index]++;
}

void C2SRMsg::printHopCountStats() {
  // check MAX_DOUBLE ?
  double min = numeric_limits<double>::max(),
    max = numeric_limits<double>::min();
  
  long double mean = 0, sd = 0, size = 0;

  // mean computation
  for(unsigned int i = 0; i < hopCountStats.size(); i++) {

    if (hopCountStats[i] > 0 && i < min) {
      min = i;
    }
    if (hopCountStats[i] > 0 && i > max) {
      max = i;
    }
    
    mean += i*hopCountStats[i];
    size += hopCountStats[i];
  }

 //cerr << "size: " << size << endl;
  mean /= size;

  // standard-deviation computation
  for(unsigned int i = 0; i < hopCountStats.size(); i++) {
    long double v = i,
      n = hopCountStats[i];
    sd += n * (v - mean) * (v - mean);
  }
  sd /= size;
  sd = sqrt(sd);

  cerr << "=== RECONFIGURATION MSG STATISTICS ===" << endl;
  cerr << "HopCount: " << min << " " << mean << " " << max << " " << sd << endl;
}
