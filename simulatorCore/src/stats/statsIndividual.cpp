/*! @file statsIndividual.cpp
 * @brief Provides a mean to store useful number statistics about modules at runtime
 * @author Andre Naz
 * @date 10/08/2016
 */

#include <cmath>

#include "statsIndividual.h"
#include "../base/buildingBlock.h"
#include "../base/world.h"

using namespace std;

namespace BaseSimulator {
namespace utils {

bool StatsIndividual::enable = false;

StatsIndividual::StatsIndividual(StatsIndividual const&si) {
 sentMessages = si.sentMessages;
 receivedMessages = si.receivedMessages;

 outgoingMessageQueueSize = si.outgoingMessageQueueSize;
 incommingMessageQueueSize = si.incommingMessageQueueSize;
 messageQueueSize = si.messageQueueSize;
 maxMessageQueueSize = si.maxMessageQueueSize;
 maxOutgoingMessageQueueSize = si.maxOutgoingMessageQueueSize;
 maxIncommingMessageQueueSize = si.maxIncommingMessageQueueSize;

 motions = si.motions;
}

void StatsIndividual::incSentMessageCount(StatsIndividual *s) {
  if (s) {
    s->sentMessages++;
  }
}

void StatsIndividual::incReceivedMessageCount(StatsIndividual *s) {
  if (s) {
    s->receivedMessages++;
  }
}

void StatsIndividual::incOutgoingMessageQueueSize(StatsIndividual *s) {
  if (s) {
    s->outgoingMessageQueueSize++;
    s->updateQueueSizeStats();
  }
}

void StatsIndividual::decOutgoingMessageQueueSize(StatsIndividual *s) {
  if (s) {
    s->outgoingMessageQueueSize--;
    s->updateQueueSizeStats();
  }
}

void StatsIndividual::incIncommingMessageQueueSize(StatsIndividual *s) {
  if (s) {
    s->incommingMessageQueueSize++;
    s->updateQueueSizeStats();
  }
}

void StatsIndividual::decIncommingMessageQueueSize(StatsIndividual *s) {
  if (s) {
    s->incommingMessageQueueSize--;
    s->updateQueueSizeStats();
  }
}

void StatsIndividual::updateQueueSizeStats() {
  messageQueueSize = outgoingMessageQueueSize + incommingMessageQueueSize;

  maxOutgoingMessageQueueSize = max(maxOutgoingMessageQueueSize,outgoingMessageQueueSize);
  maxIncommingMessageQueueSize = max(maxIncommingMessageQueueSize,incommingMessageQueueSize);
  maxMessageQueueSize = max(maxMessageQueueSize,messageQueueSize);
}

void StatsIndividual::incMotionCount(StatsIndividual *s) {
  if (s) {
    s->motions++;
  }
}

#define MIN_INDEX 0
#define SUM_INDEX 1
#define MAX_INDEX 2

void StatsIndividual::compute1(uint64_t cs[3], uint64_t v) {
    cs[MIN_INDEX] = min(cs[MIN_INDEX],v);
    cs[SUM_INDEX] += v;
    cs[MAX_INDEX] = max(cs[MAX_INDEX],v);
}

long double StatsIndividual::compute2(uint64_t s[3], int n) {
  return ((long double) s[SUM_INDEX]) / ((long double) n);
}

long double StatsIndividual::compute3(long double m, uint64_t v) {
  long double vld = (long double) v;
  return (vld - m) * (vld - m);
}

string StatsIndividual::formatStat(string n, uint64_t s[3], long double m, long double sd, string f) {
    return n + ": " + to_string(s[MIN_INDEX]) + " " + to_string(m) + " " + to_string(s[MAX_INDEX]) + " " + to_string(sd) + f;
}

string StatsIndividual::getStats() {
  // min mean max sd
  string s;

  uint64_t sm[3] = {UINT64_MAX,0,0};
  long double smm = 0;
  long double smsd = 0;

  uint64_t rm[3] = {UINT64_MAX,0,0};
  long double rmm = 0;
  long double rmsd = 0;

  uint64_t mmqs[3] = {UINT64_MAX,0,0};
  long double mmqsm = 0;
  long double mmqssd = 0;

  uint64_t momqs[3] = {UINT64_MAX,0,0};
  long double momqsm = 0;
  long double momqssd = 0;

  uint64_t mimqs[3] = {UINT64_MAX,0,0};
  long double mimqsm = 0;
  long double mimqssd = 0;

  uint64_t m[3] = {UINT64_MAX,0,0};
  long double mm = 0;
  long double msd = 0;

  // Stats computation, over all modules
  int size = getWorld()->getSize();
  map<bID,BuildingBlock*> &modules = getWorld()->getMap();
  map<bID,BuildingBlock*>::iterator it;

  // Min, sum and max computation
  for (it = modules.begin(); it != modules.end(); ++it) {
    StatsIndividual *st = it->second->stats;
    compute1(sm,st->sentMessages);
    compute1(rm,st->receivedMessages);
    compute1(mmqs,st->maxMessageQueueSize);
    compute1(momqs,st->maxOutgoingMessageQueueSize);
    compute1(mimqs,st->maxIncommingMessageQueueSize);
    compute1(m,st->motions);
  }

  // Mean
  smm = compute2(sm,size);
  rmm = compute2(rm,size);
  mmqsm = compute2(mmqs,size);
  momqsm = compute2(momqs,size);
  mimqsm = compute2(mimqs,size);
  mm = compute2(m,size);

  // Standard-Deviation computation
  // First, variance computation:
  for (it = modules.begin(); it != modules.end(); ++it) {
    StatsIndividual *st = it->second->stats;
    smsd += compute3(smm,st->sentMessages);
    rmsd += compute3(rmm,st->receivedMessages);
    mmqssd += compute3(mmqsm,st->maxMessageQueueSize);
    momqssd += compute3(momqsm,st->maxOutgoingMessageQueueSize);
    mimqssd += compute3(mimqsm,st->maxIncommingMessageQueueSize);
    msd += compute3(mm,st->motions);
  }

  // Standard-deviation from variance: divide the variance by size
  // and take the square-root of the previous result
  smsd /= size;
  rmsd /= size;
  mmqssd /= size;
  momqssd /= size;
  mimqssd /= size;
  msd /= size;

  smsd = sqrt(smsd);
  rmsd = sqrt(rmsd);
  mmqssd = sqrt(mmqssd);
  momqssd = sqrt(momqssd);
  mimqssd = sqrt(mimqssd);
  msd = sqrt(msd);

  // Result string formatting
  s += "=== STATISTICS PER MODULE ===\n";
  s += "Format: \"parameter: min mean max standard-deviation\"\n";
  s += formatStat("Sent messages",sm,smm,smsd,"\n");
  s += formatStat("Received messages",rm,rmm,rmsd,"\n");
  s += formatStat("Maximum message queue size",mmqs,mmqsm,mmqssd,"\n");
  s += formatStat("Maximum outgoing message queue size",momqs,momqsm,momqssd,"\n");
  s += formatStat("Maximum incomming message queue size",mimqs,mimqsm,mimqssd,"\n");
  s += formatStat("Motions",m,mm,msd,"\n");
  return s;
}

}
}
