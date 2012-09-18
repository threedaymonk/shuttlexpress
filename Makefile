.PHONY: clean

shuttle: shuttle.scm interface.scm support.scm keysymdef.scm
	csc $<

keysymdef.scm: parse-keysymdef.scm
	csi -script $< > $@

clean:
	rm -f shuttle
