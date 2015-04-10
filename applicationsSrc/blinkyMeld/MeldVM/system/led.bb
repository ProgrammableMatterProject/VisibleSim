#include "led.bbh"
#include "../hw-api/hwLED.h"

// stolen from: http://www.free-webmaster-tools.com/colorpicker.htm
//enum {RED, ORANGE, YELLOW, GREEN, BLUE, INDIGO, VIOLET, WHITE, AQUA, FUCHSIA, LAWNGREEN, LIGHTPINK, LIGHTSLATEGRAY, SADDLEBROWN, GOLD, NUM_COLORS};
uint8_t colors[NUM_COLORS+2][3] = {
#ifdef BBSIM
{0xFF, 0x00, 0x00}, // red
{0xFF, 0xA5, 0x00}, // orange
{0xFF, 0xFF, 0x00}, // yellow
{0x00, 0xFF, 0x00}, // green
{0x00, 0xFF, 0xFF}, // aqua
{0x00, 0x00, 0xFF}, // blue
{0xFF, 0xFF, 0xFF}, // white
{0xFF, 0x00, 0xFF}, // purple
{0xFF, 0x00, 0x2D}, // pink
#else
{0xFF, 0x00, 0x00}, // red
{0xE1, 0x2D, 0x00}, // orange
{0xFF, 0xEB, 0x00}, // yellow
{0x00, 0xFF, 0x00}, // green
{0x00, 0xFF, 0xFF}, // aqua
{0x00, 0x00, 0xFF}, // blue
{0xFF, 0xFF, 0xFF}, // white
{0xFF, 0x00, 0xFF}, // purple
{0xFF, 0x00, 0x2D}, // pink
//--> new user colors here
// System reserved colors, should not be used by users. (asserts, debug, memoryCheck colors)
// PUT ALL USER COLORS BEFORE THESE ONES.
{0x4B, 0x00, 0xB0}, // indigo
{0x8B, 0x45, 0x13}, // saddlebrown
#endif
//{0x7C, 0xFC, 0x00}, // lawngrean
//{0x77, 0x88, 0x99}, // light slate gray
//{0xFF, 0xB6, 0xC1}, // lightpink
//{0xFF, 0xD7, 0x00}, // gold
//{0xEE, 0x82, 0xEE}, // violet
};

threadvar byte 		currentRGB[3];
threadvar Color 	currentColor;
threadvar Intensity 	currentIntensity = INTENSITY_MAX;

Color getColor()
{
	return currentColor;
}

Color getNextColor()
{
	Color tmp = currentColor;
	
	tmp++;
	
	if(tmp >= NUM_COLORS)
	{
		tmp = 0;
	}

	return tmp;
}

void setColor(Color c)
{
	currentColor = c;

	setLED(colors[c][0], colors[c][1], colors[c][2], currentIntensity);
}

Color setNextColor()
{
	Color tmp = getNextColor();

	setColor(tmp);
	
	return currentColor;
}

void setLED(byte r, byte g, byte b, Intensity i)
{
	setHWLED(r,g,b,i);
}

void setIntensity(Intensity i)
{
	currentIntensity = i;
	
	setLED(currentRGB[0], currentRGB[1], currentRGB[2], i);
}

Intensity getIntensity()
{
	return currentIntensity;
}

void initLED()
{
	initHWLED();
}
