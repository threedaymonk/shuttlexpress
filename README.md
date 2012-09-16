# ShuttleXpress

A program for responding to Contour ShuttleXpress events in Linux.

At the moment, it simply parses the raw event packets and prints out a line
for every state change. In the future, it will probably do something more
useful.

## Requirements

* Linux
* Chicken Scheme
* The **matchable** egg
* A Contour ShuttleXpress

The device also needs to be accessible to the user running the program, via a
udev rule or sudo.
