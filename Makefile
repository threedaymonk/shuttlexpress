.PHONY: clean udev

shuttle: shuttle.scm interface.so fake-keys.so
	csc -L/usr/X11R6/lib -lX11 $<

%.so: %.scm
	csc -L/usr/X11R6/lib -lX11 -J -s $<

fake-keys.so: keysymdef.so

interface.so: support.so

keysymdef.scm: parse-keysymdef.scm
	csi -script $< > $@

udev: /etc/udev/rules.d/90-shuttlexpress.rules

/etc/udev/rules.d/90-shuttlexpress.rules:
	cp 90-shuttlexpress.rules $@

clean:
	rm -f shuttle keysymdef.scm *.so *.import.scm
