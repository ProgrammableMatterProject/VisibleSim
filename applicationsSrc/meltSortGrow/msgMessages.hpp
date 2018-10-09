/*
 * msgMessages.h
 *
 *  Created on: 18/12/2017
 *      Author: Pierre Thalamy
 */

#ifndef MSGMESSAGES_H_
#define MSGMESSAGES_H_

#define MSG_ROOT_UPDATE 0011 // 9
#define MSG_ROOT_CONFIRM 0012 // 10
#define MSG_ROOT_NCONFIRM 0013 // 11

#define MSG_MELT_LABEL_AP 0020 // 16 Label articulation points
#define MSG_MELT_LABEL_AP_DONE 0021 // 17 Notify parent of labelling completion
#define MSG_MELT_FIND_MOBILE_MODULE 0022 // 18
#define MSG_MELT_FIND_MOBILE_MODULE_ACK 0023 // 19 Notify parent we're on the move!
#define MSG_MELT_FIND_MOBILE_MODULE_NACK 0024 // 20 Notify parent we couldn't find non AP (Convergence condition)

#define MSG_MELT_RESET_GRAPH 0025 // 21
#define MSG_MELT_RESET_GRAPH_DONE 0026 // 22
#define MSG_MELT_RESET_GRAPH_NACK 0027 // 23

#define MSG_MELT_APL_START 0030 // 24
#define MSG_MELT_APL_TOKEN 0031 //25
#define MSG_MELT_APL_ECHO 0032 // 26
#define MSG_MELT_APL_VISITED 0033 // 27

#define MSG_GROW_FINDPATH 0034 // 28
#define MSG_GROW_FINDPATH_FOUND 0035 // 29
#define MSG_GROW_FINDPATH_NOTFOUND 0036 // 30
#define MSG_GROW_FINDPATH_IGNORE 0037 // 31
#define MSG_GROW_NEXTMODULE 0040 // 32

#define MSG_GROW_BUILDPATH 0041 // 33
#define MSG_GROW_BUILDPATH_SUCCESS 0042 // 34
#define MSG_GROW_BUILDPATH_FAILURE 0043 // 35
#define MSG_GROW_BUILDPATH_IGNORE 0044 //36


#endif // MSGMESSAGES_H_
