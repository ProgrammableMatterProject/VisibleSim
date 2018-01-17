
/*
 * APLTypes.h
 *
 *  Created on: 29/11/2018
 *      Author: Pierre Thalamy
 */

#ifndef APLTYPES_H_
#define APLTYPES_H_

class EchoPayload {
public:
    int lDfn;
    int dfnCnt;
    EchoPayload(int _lDfn, int _dfnCnt) : lDfn(_lDfn), dfnCnt(_dfnCnt) {}
};

#endif /* APLTYPES_H_ */
