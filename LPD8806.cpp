/*

Arduino library to control LPD8806-based RGB LED Strips
Original code by Adafruit.com
Modified by Adam Haile (adamhaile.net) to support BRG ordered strips
and to add gama correction and global brightness control.
MIT license

Clearing up some misconceptions about how the LPD8806 drivers work:

The LPD8806 is not a FIFO shift register.  The first data out controls the
LED *closest* to the processor (unlike a typical shift register, where the
first data out winds up at the *furthest* LED).  Each LED driver 'fills up'
with data and then passes through all subsequent bytes until a latch
condition takes place.  This is actually pretty common among LED drivers.

All color data bytes have the high bit (128) set, with the remaining
seven bits containing a brightness value (0-127).  A byte with the high
bit clear has special meaning (explained later).

The rest gets bizarre...

The LPD8806 does not perform an in-unison latch (which would display the
newly-transmitted data all at once).  Rather, each individual byte (even
the separate G, R, B components of each LED) is latched AS IT ARRIVES...
or more accurately, as the first bit of the subsequent byte arrives and
is passed through.  So the strip actually refreshes at the speed the data
is issued, not instantaneously (this can be observed by greatly reducing
the data rate).  This has implications for POV displays and light painting
applications.  The 'subsequent' rule also means that at least one extra
byte must follow the last pixel, in order for the final blue LED to latch.

To reset the pass-through behavior and begin sending new data to the start
of the strip, a number of zero bytes must be issued (remember, all color
data bytes have the high bit set, thus are in the range 128 to 255, so the
zero is 'special').  This should be done before each full payload of color
values to the strip.  Curiously, zero bytes can only travel one meter (32
LEDs) down the line before needing backup; the next meter requires an
extra zero byte, and so forth.  Longer strips will require progressively
more zeros.  *(see note below)

In the interest of efficiency, it's possible to combine the former EOD
extra latch byte and the latter zero reset...the same data can do double
duty, latching the last blue LED while also resetting the strip for the
next payload.

So: reset byte(s) of suitable length are issued once at startup to 'prime'
the strip to a known ready state.  After each subsequent LED color payload,
these reset byte(s) are then issued at the END of each payload, both to
latch the last LED and to prep the strip for the start of the next payload
(even if that data does not arrive immediately).  This avoids a tiny bit
of latency as the new color payload can begin issuing immediately on some
signal, such as a timer or GPIO trigger.

Technically these zero byte(s) are not a latch, as the color data (save
for the last byte) is already latched.  It's a start-of-data marker, or
an indicator to clear the thing-that's-not-a-shift-register.  But for
conversational consistency with other LED drivers, we'll refer to it as
a 'latch' anyway.

* This has been validated independently with multiple customers'
  hardware.  Please do not report as a bug or issue pull requests for
  this.  Fewer zeros sometimes gives the *illusion* of working, the first
  payload will correctly load and latch, but subsequent frames will drop
  data at the end.  The data shortfall won't always be visually apparent
  depending on the color data loaded on the prior and subsequent frames.
  Tested.  Confirmed.  Fact.
*/


#include "SPI.h"
#include "LPD8806.h"
#include <avr/pgmspace.h>

/*****************************************************************************/

// Gamma correction compensates for our eyes' nonlinear perception of
// intensity.  It's the LAST step before a pixel value is stored, and
// allows intermediate rendering/processing to occur in linear space.
// The table contains 256 elements (8 bit input), though the outputs are
// only 7 bits (0 to 127).  This is normal and intentional by design: it
// allows all the rendering code to operate in the more familiar unsigned
// 8-bit colorspace (used in a lot of existing graphics code), and better
// preserves accuracy where repeated color blending operations occur.
// Only the final end product is converted to 7 bits, the native format
// for the LPD8806 LED driver.  Gamma correction and 7-bit decimation
// thus occur in a single operation.
PROGMEM prog_uchar gammaTable[]  = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 11, 11, 11, 11, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15, 16, 16, 16, 17, 17, 18, 18, 18, 19, 19, 19, 20, 20, 21, 21, 22, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 33, 34, 34, 35, 35, 36, 36, 37, 38, 38, 39, 39, 40, 41, 41, 42, 43, 43, 44, 45, 45, 46, 47, 47, 48, 49, 49, 50, 51, 52, 52, 53, 54, 54, 55, 56, 57, 58, 58, 59, 60, 61, 61, 62, 63, 64, 65, 66, 66, 67, 68, 69, 70, 71, 72, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 101, 102, 103, 104, 106, 107, 108, 109, 110, 111, 112, 114, 115, 116, 117, 118, 119, 121, 122, 123, 124, 126, 127 };
//PROGMEM prog_uchar gammaTable[]  = {0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 8, 8, 8, 8, 9, 9, 9, 9, 10, 10, 10, 10, 11, 11, 11, 12, 12, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15, 16, 16, 16, 17, 17, 18, 18, 18, 19, 19, 19, 20, 20, 21, 21, 22, 22, 22, 23, 23, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30, 30, 31, 31, 32, 32, 33, 33, 34, 34, 35, 36, 36, 37, 37, 38, 39, 39, 40, 40, 41, 42, 42, 43, 44, 44, 45, 46, 46, 47, 48, 48, 49, 50, 50, 51, 52, 53, 53, 54, 55, 56, 56, 57, 58, 59, 59, 60, 61, 62, 63, 63, 64, 65, 66, 67, 68, 69, 69, 70, 71, 72, 73, 74, 75, 76, 77, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91, 92, 93, 94, 95, 96, 97, 98, 99, 100, 102, 103, 104, 105, 106, 107, 108, 109, 110, 112, 113, 114, 115, 116, 117, 119, 120, 121, 122, 123, 125, 126, 127 };
/*{
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
    0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,  1,  1,  1,
    1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,  2,  2,  2,  2,
    2,  2,  2,  2,  2,  3,  3,  3,  3,  3,  3,  3,  3,  4,  4,  4,
    4,  4,  4,  4,  5,  5,  5,  5,  5,  6,  6,  6,  6,  6,  7,  7,
    7,  7,  7,  8,  8,  8,  8,  9,  9,  9,  9, 10, 10, 10, 10, 11,
   11, 11, 12, 12, 12, 13, 13, 13, 13, 14, 14, 14, 15, 15, 16, 16,
   16, 17, 17, 17, 18, 18, 18, 19, 19, 20, 20, 21, 21, 21, 22, 22,
   23, 23, 24, 24, 24, 25, 25, 26, 26, 27, 27, 28, 28, 29, 29, 30,
   30, 31, 32, 32, 33, 33, 34, 34, 35, 35, 36, 37, 37, 38, 38, 39,
   40, 40, 41, 41, 42, 43, 43, 44, 45, 45, 46, 47, 47, 48, 49, 50,
   50, 51, 52, 52, 53, 54, 55, 55, 56, 57, 58, 58, 59, 60, 61, 62,
   62, 63, 64, 65, 66, 67, 67, 68, 69, 70, 71, 72, 73, 74, 74, 75,
   76, 77, 78, 79, 80, 81, 82, 83, 84, 85, 86, 87, 88, 89, 90, 91,
   92, 93, 94, 95, 96, 97, 98, 99,100,101,102,104,105,106,107,108,
  109,110,111,113,114,115,116,117,118,120,121,122,123,125,126,127
};*/

// This function (which actually gets 'inlined' anywhere it's called)
// exists so that gammaTable can reside out of the way down here in the
// utility code...didn't want that huge table distracting or intimidating
// folks before even getting into the real substance of the program, and
// the compiler permits forward references to functions but not data.
inline byte gamma(byte x) {
  return pgm_read_byte(&gammaTable[x]);
}

// Constructor for use with hardware SPI (specific clock/data pins):
LPD8806::LPD8806(uint16_t n) {
  pixels = NULL;
  begun  = false;
  updateLength(n);
  updatePins();
}

// Constructor for use with arbitrary clock/data pins:
LPD8806::LPD8806(uint16_t n, uint8_t dpin, uint8_t cpin) {
  pixels = NULL;
  begun  = false;
  updateLength(n);
  updatePins(dpin, cpin);
}

// via Michael Vogt/neophob: empty constructor is used when strip length
// isn't known at compile-time; situations where program config might be
// read from internal flash memory or an SD card, or arrive via serial
// command.  If using this constructor, MUST follow up with updateLength()
// and updatePins() to establish the strip length and output pins!
LPD8806::LPD8806(void) {
  numLEDs = numBytes = 0;
  pixels  = NULL;
  begun   = false;
  updatePins(); // Must assume hardware SPI until pins are set
}

// Activate hard/soft SPI as appropriate:
void LPD8806::begin(void) {
  if(hardwareSPI == true) startSPI();
  else                    startBitbang();
  begun = true;
}

// Change pin assignments post-constructor, switching to hardware SPI:
void LPD8806::updatePins(void) {
  hardwareSPI = true;
  datapin     = clkpin = 0;
  // If begin() was previously invoked, init the SPI hardware now:
  if(begun == true) startSPI();
  // Otherwise, SPI is NOT initted until begin() is explicitly called.

  // Note: any prior clock/data pin directions are left as-is and are
  // NOT restored as inputs!
}

// Change pin assignments post-constructor, using arbitrary pins:
void LPD8806::updatePins(uint8_t dpin, uint8_t cpin) {

  datapin     = dpin;
  clkpin      = cpin;
  clkport = dataport = 0;
  clkpinmask = datapinmask = 0;

#if defined(__AVR_ATmega168__) || defined(__AVR_ATmega328P__) || defined (__AVR_ATmega328__) || defined(__AVR_ATmega8__) || (__AVR_ATmega1281__) || defined(__AVR_ATmega2561__) || defined(__AVR_ATmega2560__) || defined(__AVR_ATmega1280__)
  clkport     = portOutputRegister(digitalPinToPort(cpin));
  clkpinmask  = digitalPinToBitMask(cpin);
  dataport    = portOutputRegister(digitalPinToPort(dpin));
  datapinmask = digitalPinToBitMask(dpin);
#endif

  if(begun == true) { // If begin() was previously invoked...
    // If previously using hardware SPI, turn that off:
    if(hardwareSPI == true) SPI.end();
    startBitbang(); // Regardless, now enable 'soft' SPI outputs
  } // Otherwise, pins are not set to outputs until begin() is called.

  // Note: any prior clock/data pin directions are left as-is and are
  // NOT restored as inputs!

  hardwareSPI = false;
}

#ifndef SPI_CLOCK_DIV8
  #define SPI_CLOCK_DIV8 4
#endif

// Enable SPI hardware and set up protocol details:
void LPD8806::startSPI(void) {
  SPI.begin();
  SPI.setBitOrder(MSBFIRST);
  SPI.setDataMode(SPI_MODE0);

  SPI.setClockDivider(SPI_CLOCK_DIV8);  // 2 MHz
  // SPI bus is run at 2MHz.  Although the LPD8806 should, in theory,
  // work up to 20MHz, the unshielded wiring from the Arduino is more
  // susceptible to interference.  Experiment and see what you get.

#if defined(__AVR_ATmega168__) || defined(__AVR_ATmega328P__) || defined (__AVR_ATmega328__) || defined(__AVR_ATmega8__) || (__AVR_ATmega1281__) || defined(__AVR_ATmega2561__) || defined(__AVR_ATmega2560__) || defined(__AVR_ATmega1280__)

  // Issue initial latch/reset to strip:
  SPDR = 0; // Issue initial byte
  for(uint16_t i=((numLEDs+31)/32)-1; i>0; i--) {
    while(!(SPSR & (1<<SPIF))); // Wait for prior byte out
    SPDR = 0;                   // Issue next byte
  }
#else
  SPI.transfer(0);
  for(uint16_t i=((numLEDs+31)/32)-1; i>0; i--) {
    SPI.transfer(0);
  }
#endif
}

// Enable software SPI pins and issue initial latch:
void LPD8806::startBitbang() {
  pinMode(datapin, OUTPUT);
  pinMode(clkpin , OUTPUT);
  if (dataport != 0) {
    // use low level bitbanging when we can
    *dataport &= ~datapinmask; // Data is held low throughout (latch = 0)
    for(uint16_t i=((numLEDs+31)/32)*8; i>0; i--) {
      *clkport |=  clkpinmask;
      *clkport &= ~clkpinmask;
    }
  } else {
    // can't do low level bitbanging, revert to digitalWrite
    digitalWrite(datapin, LOW);
    for(uint16_t i=((numLEDs+31)/32)*8; i>0; i--) {
      digitalWrite(clkpin, HIGH);
      digitalWrite(clkpin, LOW);
    }
  }
}

// Change strip length (see notes with empty constructor, above):
void LPD8806::updateLength(uint16_t n) {
  uint8_t latchBytes = (n + 31) / 32;
  if(pixels != NULL) free(pixels); // Free existing data (if any)
  numLEDs    = n;
  n         *= 3; // 3 bytes per pixel
  numBytes   = n + latchBytes;
  if(NULL != (pixels = (uint8_t *)malloc(numBytes))) { // Alloc new data
    memset( pixels   , 0x80, n);          // Init to RGB 'off' state
    memset(&pixels[n], 0   , latchBytes); // Clear latch bytes
  } else numLEDs = numBytes = 0; // else malloc failed
  // 'begun' state does not change -- pins retain prior modes
}

uint16_t LPD8806::numPixels(void) {
  return numLEDs;
}

// This is how data is pushed to the strip.  Unfortunately, the company
// that makes the chip didnt release the protocol document or you need
// to sign an NDA or something stupid like that, but we reverse engineered
// this from a strip controller and it seems to work very nicely!
void LPD8806::show(void) {
  uint8_t  *ptr = pixels;
  uint16_t i    = numBytes;

  // This doesn't need to distinguish among individual pixel color
  // bytes vs. latch data, etc.  Everything is laid out in one big
  // flat buffer and issued the same regardless of purpose.
  if(hardwareSPI) {
    while(i--) {
#if defined(__AVR_ATmega168__) || defined(__AVR_ATmega328P__) || defined (__AVR_ATmega328__) || defined(__AVR_ATmega8__) || (__AVR_ATmega1281__) || defined(__AVR_ATmega2561__) || defined(__AVR_ATmega2560__) || defined(__AVR_ATmega1280__)
      while(!(SPSR & (1<<SPIF))); // Wait for prior byte out
      SPDR = *ptr++;              // Issue new byte
#else
      SPI.transfer(*ptr++);
#endif
    }
  } else {
    uint8_t p, bit;

    while(i--) {
      p = *ptr++;
      for(bit=0x80; bit; bit >>= 1) {
	if (dataport != 0) {
	  if(p & bit) *dataport |=  datapinmask;
	  else        *dataport &= ~datapinmask;
	  *clkport |=  clkpinmask;
	  *clkport &= ~clkpinmask;
	} else {
	  if (p&bit) digitalWrite(datapin, HIGH);
	  else digitalWrite(datapin, LOW);
	  digitalWrite(clkpin, HIGH);
	  digitalWrite(clkpin, LOW);
	}
      }
    }
  }
}

// Convert separate R,G,B into combined 32-bit GRB color:
uint32_t LPD8806::Color(byte r, byte g, byte b) {
  
  r = gamma(r * bright_level);
  g = gamma(g * bright_level);
  b = gamma(b * bright_level);
  
  return ((uint32_t)(g | 0x80) << 16) |
         ((uint32_t)(r | 0x80) <<  8) |
                     b | 0x80 ;
}

// Set pixel color from separate 7-bit R, G, B components:
void LPD8806::setPixelColor(uint16_t n, uint8_t r, uint8_t g, uint8_t b) {
  if(n < numLEDs) { // Arrays are 0-indexed, thus NOT '<='
    uint8_t *p = &pixels[n * 3];
    
    
    r = gamma(r * bright_level);
    g = gamma(g * bright_level);
    b = gamma(b * bright_level);
    
  
    *p++ = b | 0x80; 
    *p++ = r | 0x80; 
    *p++ = g | 0x80; 
  }
}

// Set pixel color from 'packed' 32-bit GRB (not RGB) value:
void LPD8806::setPixelColor(uint16_t n, uint32_t c) {
  if(n < numLEDs) { // Arrays are 0-indexed, thus NOT '<='
    uint8_t *p = &pixels[n * 3];

    *p++ =  c        | 0x80;
    *p++ = (c >>  8) | 0x80;
    *p++ = (c >> 16) | 0x80;
  }
}

// Query color from previously-set pixel (returns packed 32-bit GRB value)
uint32_t LPD8806::getPixelColor(uint16_t n) {
  if(n < numLEDs) {
    uint16_t ofs = n * 3;
    return ((uint32_t)(pixels[ofs    ] & 0x7f) << 16) |
           ((uint32_t)(pixels[ofs + 1] & 0x7f) <<  8) |
            (uint32_t)(pixels[ofs + 2] & 0x7f);
  }

  return 0; // Pixel # is out of bounds
}

