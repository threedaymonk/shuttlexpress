.PHONY: clean

shuttle: shuttle.scm support.scm
	csc shuttle.scm

clean:
	rm -f shuttle
