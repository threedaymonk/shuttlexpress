(use posix extras srfi-69)
(include "keysymdef.scm")

(foreign-declare "#include <X11/Xlib.h>")
(foreign-declare "#include <X11/keysym.h>")

(foreign-declare "
  XKeyEvent createKeyEvent(Display *display, Window window,
                           Window root_window, Bool press,
                           int keysym, int modifiers
  ) {
    XKeyEvent event;

    event.display     = display;
    event.window      = window;
    event.root        = root_window;
    event.subwindow   = None;
    event.time        = CurrentTime;
    event.x           = 1;
    event.y           = 1;
    event.x_root      = 1;
    event.y_root      = 1;
    event.same_screen = True;
    event.keycode     = XKeysymToKeycode(display, keysym);
    event.state       = modifiers;
    event.type        = (press) ? KeyPress : KeyRelease;

    return event;
  }
  ")

(define send-key-events
  (foreign-lambda* int ((int keysym) (int modifiers)) "
    Display *display;
    Window root_window, focused_window;
    XKeyEvent event;
    int revert;

    display = XOpenDisplay(0);
    if (display == NULL) return -1;

    root_window = XDefaultRootWindow(display);
    XGetInputFocus(display, &focused_window, &revert);

    event = createKeyEvent(display, focused_window, root_window, True, keysym, modifiers);
    XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);

    event = createKeyEvent(display, focused_window, root_window, False, keysym, modifiers);
    XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);

    XCloseDisplay(display);
    return 0;
    "))

(define xk-modifier-map
  '(("Shift" .   1)
    ("Control" . 4)
    ("Mod1" .    8)
    ("Mod2" .   16)
    ("Mod3" .   32)
    ("Mod4" .   64)
    ("Mod5" .  128)))

(define (xk-modifier-mask name)
  (cdr (assoc name xk-modifier-map)))

(define (decode-modifiers modifiers)
  (let loop ((state 0) (modifiers modifiers))
    (if (null? modifiers)
      state
      (loop (bitwise-ior state (xk-modifier-mask (car modifiers)))
            (cdr modifiers)))))

(define (fake-keypress name . modifiers)
  (let ((keysym (hash-table-ref keysymdef name)))
    (send-key-events keysym (decode-modifiers modifiers))))
