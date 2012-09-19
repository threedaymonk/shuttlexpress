# ShuttleXpress

A program for responding to Contour ShuttleXpress events in Linux.

At the moment, events are hard-coded to simulated key presses in `shuttle.scm`
but this will probably change.

## Requirements

* Linux
* Chicken Scheme
* The **matchable** egg
* Xlib headers
* A Contour ShuttleXpress

# Instructions

Copy the udev rule to `/etc/udev/rules.d`:

    sudo make udev

Build it:

    make

Run it:

    ./shuttle
