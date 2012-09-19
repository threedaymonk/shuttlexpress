.PHONY: clean udev

shuttle: shuttle.scm interface.scm support.scm keysymdef.scm fake-keys.scm
	csc -L/usr/X11R6/lib -lX11 $<

keysymdef.scm: parse-keysymdef.scm
	csi -script $< > $@

udev: /etc/udev/rules.d/90-shuttlexpress.rules

/etc/udev/rules.d/90-shuttlexpress.rules:
	cp 90-shuttlexpress.rules $@

clean:
	rm -f shuttle keysymdef.scm
