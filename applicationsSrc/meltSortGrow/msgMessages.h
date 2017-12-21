/*
 * msgMessages.h
 *
 *  Created on: 18/12/2017
 *      Author: Pierre Thalamy
 */

#ifndef MSGMESSAGES_H_
#define MSGMESSAGES_H_

#define MSG_ROOT_UPDATE 0011
#define MSG_ROOT_CONFIRM 0012
#define MSG_ROOT_NCONFIRM 0013

#define MSG_MELT_LABEL_AP 0020 // Label articulation points
#define MSG_MELT_LABEL_AP_DONE 0021 // Notify parent of labelling completion
#define MSG_MELT_FIND_MOBILE_MODULE 0022
#define MSG_MELT_FIND_MOBILE_MODULE_ACK 0023 // Notify parent we're on the move!
#define MSG_MELT_FIND_MOBILE_MODULE_NACK 0024 // Notify parent we couldn't find non AP (Convergence condition)

#define MSG_MELT_RESET_GRAPH 0025
#define MSG_MELT_RESET_GRAPH_DONE 0026
#define MSG_MELT_RESET_GRAPH_NACK 0027

#define MSG_MELT_APL_START 0030 //24
#define MSG_MELT_APL_TOKEN 0031 //25
#define MSG_MELT_APL_ECHO 0032 // 26
#define MSG_MELT_APL_VISITED 0033 // 27

#define MSG_GROW_FINDPATH 0040 // 32
#define MSG_GROW_FINDPATH_FOUND 0041 // 33
#define MSG_GROW_FINDPATH_NOTFOUND 0042 // 34
#define MSG_GROW_FINDPATH_IGNORE 0043 // 35
#define MSG_GROW_NEXTMODULE 0044 // 36

#endif // MSGMESSAGES_H_
