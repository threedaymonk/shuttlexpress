.PHONY: clean

shuttle: shuttle.scm support.scm hid.scm
	csc shuttle.scm

clean:
	rm -f shuttle
