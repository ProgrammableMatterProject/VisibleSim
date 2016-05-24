#include <iostream>     // std::cout
#include <fstream>      // std::ifstream
#include <cstring>

#include "pixel.hpp"
#include "targa.hpp"

using namespace std;

#define R_INDEX 0
#define G_INDEX 1
#define B_INDEX 2
#define A_INDEX 3
#define P_SIZE 4

#define TRANSPARENT 0

#define DEF_targaHeaderLength			12
#define DEF_targaHeaderContent		"\x00\x02\x00\x00\x00\x00\x00\x00\x00\x00\x00"

Targa::Targa(string i) {
  input = i;
  buffer = NULL;
  read();
}

Targa::Targa(const Targa &t) {
  input = t.input;
  buffer = NULL;
  read();
}

Targa::~Targa() {
  delete buffer;
}

void Targa::read() {
  
  ifstream fin;
  char *pData;
  streampos maxLen=0;

  fin.open(input.c_str(),ios::binary);
  if (!fin.is_open()) return;

  // calcul la longueur du fichier
  fin.seekg (0, ios::end);
  maxLen = fin.tellg();
  fin.seekg (0, ios::beg);

  // allocation de la mémoire pour le fichier
  pData = new char [int(maxLen)];

  // lecture des données du fichier
  fin.read(pData,maxLen);

  fin.close();

  int commentOffset = int( (unsigned char)*pData );
  if( memcmp( pData + 1, DEF_targaHeaderContent, DEF_targaHeaderLength - 1 ) != 0 ) {
    cerr << "Unknown format: " << input << endl;
    return;
    }
  unsigned char smallArray[ 2 ];

  memcpy( smallArray, pData + DEF_targaHeaderLength + 0, 2 );
  width = smallArray[ 0 ] + smallArray[ 1 ] * 0x0100;

  memcpy( smallArray, pData + DEF_targaHeaderLength + 2, 2 );
  height = smallArray[ 0 ] + smallArray[ 1 ] * 0x0100;

  memcpy( smallArray, pData + DEF_targaHeaderLength + 4, 2 );
  int depth = smallArray[ 0 ];
  //	int pixelBitFlags = smallArray[ 1 ];

  if( ( width <= 0 ) || ( height <= 0 ) )
    return;

  // Only allow 24-bit and 32-bit!
  bool is24Bit( depth == 24 );
  bool is32Bit( depth == 32 );
  if( !( is24Bit || is32Bit ) )
    return;

  // Make it a BGRA array for now.
  int bodySize( width * height * 4 );
  unsigned char * pBuffer = new unsigned char[ bodySize ];
  if( is32Bit )
    {
      // Easy, just copy it.
      memcpy( pBuffer, pData + DEF_targaHeaderLength + 6 + commentOffset, bodySize );
    }
  else if( is24Bit )
    {
      int bytesRead = DEF_targaHeaderLength + 6 + commentOffset;
      for( int loop = 0; loop < bodySize; loop += 4, bytesRead += 3 )
	{
	  memcpy( pBuffer + loop, pData + bytesRead, 3 );
	  pBuffer[ loop + 3 ] = 255;			// Force alpha to max.
	}
    }
  else return;

  // Swap R & B (convert to RGBA).
  for( int loop = 0; loop < bodySize; loop += 4 )
    {
      unsigned char tempC = pBuffer[ loop + 0 ];
      pBuffer[ loop + 0 ] = pBuffer[ loop + 2 ];
      pBuffer[ loop + 2 ] = tempC;
    }

  delete [] pData;

  /*if (retourner)
    { unsigned char * pBufferRet = new unsigned char[ bodySize ],
	*ptr1=pBuffer+width*(height-1)*4,*ptr2=pBufferRet;
      for (int loop=0; loop<height; loop++)
	{ memcpy(ptr2,ptr1,width*4);
	  ptr2+=width*4;
	  ptr1-=width*4;
	}
      delete [] pBuffer;
      return pBufferRet;
      }*/
  
  // Ownership moves out.
  buffer = pBuffer;
}

ostream& Targa::exportToVisibleSim(ostream &output) {
  int tx = width/2;
  int ty = height/2;
  
  cerr << "exporting to VisibleSim..." << endl;

  // header
  output << "<world gridSize=\"" << width+1 << "," << height+1 << "\">" << endl;
  output << "<camera target=\"" << tx <<"," << ty << ",0\" directionSpherical=\"5," << ty << "," << width << "\" angle=\"45\"/>" << endl;
  output << "<spotlight target=\"" << tx << "," << ty << ",0\" directionSpherical=\"-45," << ty << "," << width << "\" angle=\"30\"/>" << endl;

  // module list
  output << "<blockList color=\"100,100,100\" blocksize=\"1,5,1\"> " << endl;
  for (int w = 0; w < width; w++) {
    for (int h = 0; h < height; h++) {
      // curent pixel index: h*width + w
      // position in map : (w,h)
      byte *buf = buffer+ (h*width + w)*P_SIZE; 
      byte r = buf[R_INDEX];
      byte g = buf[G_INDEX];
      byte b = buf[B_INDEX];
      byte a = buf[A_INDEX];

      int y = h;
      int x = w + (h%2);
      if (a != TRANSPARENT) {
	Pixel p(r,g,b,a);
	output << "<block position=\"" << x << "," << y << "\" "
	       << "color=\"" << p << "\"/>" << endl;
      }
      
    }
  }
  output << "</blockList>" << endl;
  output << "</world>" << endl;
  return output;
}

/*
<world gridSize="30,30"><camera target="17,1,0" directionSpherical="5,25,25" angle="45"/><spotlight target="20,18,0" directionSpherical="-45,40,50" angle="30"/><!-- x,y -->
<blockList color="100,100,100" blocksize="1,5,1"><!-- x,y 
	 <block position="8,0"/>
	 <block position="7,0"/>
	 <block position="6,0"/>
	 <block position="5,0"/>
	 <block position="4,0"/>
	 <block position="3,0"/>
	 <block position="7,1"/>
	 <block position="6,1"/>
	 <block position="5,1"/>
	 <block position="4,1"/> 
--><block position="3,0"/><block position="4,0"/><block position="5,0"/><block position="6,0"/><block position="7,0"/><block position="8,0"/><block position="4,1"/><block position="5,1"/><block position="6,1"/><block position="7,1"/></blockList><targetGrid><block position="3,0"/><block position="4,0"/><block position="5,0"/><block position="6,0"/><block position="3,1"/><block position="4,1"/><block position="5,1"/><block position="4,2"/><block position="5,2"/><block position="4,3"/></targetGrid></world>
 */
