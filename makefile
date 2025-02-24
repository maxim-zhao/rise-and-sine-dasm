WLAZ80 = wla-z80.exe
WLALINK = wlalink.exe

# These are targets that aren't files
.PHONY: all clean default

# Disable built-in rules
.SUFFIXES:

default: rise-and-sine.sms

# Build
rise-and-sine.sms.o: rise-and-sine.asm
	$(WLAZ80) -o $@ $<

rise-and-sine.sms: rise-and-sine.sms.o
	echo [objects] > linkfile
	echo $< >> linkfile
	$(WLALINK) -d -r -v -s linkfile $@
