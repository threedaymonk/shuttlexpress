(use posix extras srfi-69)
(include "keysymdef.scm")

(foreign-declare "#include <X11/Xlib.h>")
(foreign-declare "#include <X11/keysym.h>")

(foreign-declare "
  void sendKeyEvent(Display *display, Window window, Window root,
                    Bool press, int keysym, int modifiers
  ) {
    XKeyEvent event;

    event.display     = display;
    event.window      = window;
    event.root        = root;
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

    XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);
  }
  ")

(define send-key-events
  (foreign-lambda* int ((int keysym) (int modifiers)) "
    Display *display;
    Window root, focused;
    int revert;

    display = XOpenDisplay(NULL);
    if (display == NULL) return -1;

    root = XDefaultRootWindow(display);
    XGetInputFocus(display, &focused, &revert);

    sendKeyEvent(display, focused, root, True,  keysym, modifiers);
    sendKeyEvent(display, focused, root, False, keysym, modifiers);

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
