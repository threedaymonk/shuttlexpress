.PHONY: clean

shuttle: shuttle.scm interface.scm support.scm keysymdef.scm fake-keys.scm
	csc -L/usr/X11R6/lib -lX11 $<

keysymdef.scm: parse-keysymdef.scm
	csi -script $< > $@

clean:
	rm -f shuttle keysymdef.scm
