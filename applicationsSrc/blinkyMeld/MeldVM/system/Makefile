BB=$(BBASE)/src-bobby/bbcompiler/compile_bb.pl
CC=gcc

ifeq ($(BBASE)x,x)
 $(error "You must define BBASE")
endif

ifeq ($(ARCH)x,x)
 X := $(shell uname -m)
 Y := $(shell uname -o)
 Y := $(subst /,-,$Y)
 ARCH := $X-$Y
 $(info Setting ARCH to $(ARCH))
endif

TARGET := block-system.o
OBJECTS := accelerometer.o block_config.o block.o boot.o circ_buffer.o data_link.o debug.o ensemble.o handler.o led.o memory.o message.o queues.o serial.o system.o test.o time.o log.o clock.o
SOURCES := accelerometer.bb accelerometer.bbh block_config.bb block_config.bbh block.bb block.bbh boot.bb boot.bbh circ_buffer.bb circ_buffer.bbh data_link.bb data_link.bbh debug.bb debug.bbh defs.bbh ensemble.bb ensemble.bbh handler.bb handler.bbh led.bb led.bbh memory.bb memory.bbh message.bb message.bbh queues.bb queues.bbh serial.bb serial.bbh system.bb system.bbh test.bb hardwaretime.bb hardwaretime.bbh log.bb log.bbh clock.bb clock.bbh

top:	$(TARGET)

$(TARGET): sources $(OBJECTS)
	ld -r -o $(TARGET) $(OBJECTS)

sources: $(SOURCES)
	$(BB) $(SOURCES)

%.o:	%.c
	$(CC) -c -o $@ $<

install:	$(TARGET)
	cp $(TARGET) $(BBASE)/bin/arch-$(ARCH)

.PHONY: clean
clean:
	/bin/rm -f *.o *.c *.h

.PHONY: reallyclean
reallyclean:
	/bin/rm -f *.o $(TARGET)


