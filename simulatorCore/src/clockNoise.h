
#ifndef CLOCK_NOISE_H_
#define CLOCK_NOISE_H

#include <cstdint>
#include <vector>
#include <string>

using namespace std;

namespace BaseSimulator {

typedef double clockNoise_t;

/**
 * \brief class to simulate clock noise
 */
class ClockNoise {
public:
  /**
   * @brief ClockNoise default constructor.
   */
  ClockNoise();
  /**
   * @brief ClockNoise destructor.
   */
  virtual ~ClockNoise();
  /**
   * @brief Returns the noise for the simulation time in parameter
   * @para simTime simulation time 
   * @return noise
   */
  virtual clockNoise_t getNoise(uint64_t simTime) = 0;
};
 
/**
 * \brief class to simulate a gaussian clock noise N(mean,sd)
 */
class GClockNoise : public ClockNoise {
 private:
  unsigned int seed;//!< Seed for random noise generation
  double mean;//!< Mean of the Gaussian distribution
  double sd;//!< Standard-deviation of the Gaussian distribution

 public:
  /**
   * @brief ClockNoise constructor.
   * @para _seed seed for random noise generation
   * @para _mean mean of the Gaussian distribution
   * @para _sd standard-deviation of the Gaussian distribution
   */
  GClockNoise(unsigned int _seed, double _mean, double _sd);
  /**
   * @brief GClockNoise destructor.
   */
  ~GClockNoise();

  clockNoise_t getNoise(uint64_t simTime);
};
 
/**
 * \brief class to replay clock noise from a list of data files
 */
class DClockNoise : public ClockNoise {
  typedef pair<uint64_t, clockNoise_t> referencePt_t;
  typedef vector<referencePt_t> noiseSignal_t; 
 private:
  static vector<noiseSignal_t> noiseSignals;//!< Noise signals
  unsigned int id;//!< Index of the noise in noiseSignals

  /**
   * @brief Returns the noise for the time in parameter.
   * @para time simulation time for which we want to compute the noise.
   * @para p1 reference point (simulation time: t_1, noise: n_1), t_1 >= time  
   * @para p2 reference point (t_2,n_2), t_2 <= time
   */
  clockNoise_t getNoise(uint64_t time, referencePt_t p1, referencePt_t p2);
  
 public:
  /**
   * @brief ClockNoise constructor.
   * @para seed seed use to randomly select which noise signal to use
   */
  DClockNoise(unsigned int seed);
  /**
   * @brief GClockNoise destructor.
   */
  ~DClockNoise();
  
  clockNoise_t getNoise(uint64_t simTime);

  /**
   * @brief Print the noise signal (simulation time, noise value)
   */
  void print();
  
  /**
   * @brief Load noise data from the set of files in parameters.
   * @para files vector of string path to files that contain noise signals
   */
  static void loadData(vector<string> &files);
};

}

#endif
