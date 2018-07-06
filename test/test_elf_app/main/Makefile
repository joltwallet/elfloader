#
# This is a project Makefile. It is assumed the directory this Makefile resides in is a
# project subdirectory.
#

all: main.o main.elf

%.o: %.c
	xtensa-esp32-elf-gcc -fno-common -mlongcalls -Wall -o $@ -c $<

%.elf: %.o
	xtensa-esp32-elf-gcc -Wl,-r -nostartfiles -nodefaultlibs -nostdlib -g -Wl,-Tesp32.ld -o $@ $< 

clean:
	rm -f *.o *.elf

