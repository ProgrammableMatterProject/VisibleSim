#include <iostream>
#include "colors.h"

std::string color_to_string( color c ) {
	switch( c ) {
		case red :
			return "red";
		break;
		case blue :
			return "blue";
		break;
		case green :
			return "green";
		break;
		case yellow :
			return "yellow";
		break;
		default :
			return "unrecognized color";
	}
}
