/*
 * coloration_BlockCode.h
 *
 *  Created on: 21 avril 2013
 *      Author: nico
 */

#ifndef coloration_messages_h
#define coloration_messages_h

#define COLOR_MSG	  9006

#include "network.h"
#include <boost/shared_ptr.hpp>
#include "colors.h"

class Color_message;

typedef std::shared_ptr<Color_message> Color_message_ptr;


class Color_message : public Message {
	color my_color;
	color horizontal_color;
	color vertical_color;
public :
	Color_message( color my_color, color vertical_color, color horizontal_color ) { this->my_color = my_color; this->vertical_color = vertical_color; this->horizontal_color = horizontal_color; };
	~Color_message() {};

	color get_my_color() { return my_color; };
	color get_vertical_color() { return vertical_color; };
	color get_horizontal_color() { return horizontal_color; };
};
#endif //coloration_messages_h
