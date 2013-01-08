(module fake-keys (fake-keypress)
(import chicken scheme foreign)
(use posix extras srfi-69 keysymdef)
(foreign-declare "#include <X11/Xlib.h>")

; Send a simulated key event to the display and window.
; press is #t for press, #f for release.
; keysym is a code obtained from keysymdef.h.
; modifiers is a mask representing the state of the modifier keys.
(define send-key-event
  (foreign-lambda* void ((c-pointer disp) (c-pointer window) (bool press)
                         (int keysym) (int modifiers))
    "XKeyEvent event;

     event.display     = disp;
     event.window      = *(Window *)window;
     event.root        = XDefaultRootWindow(disp);
     event.subwindow   = None;
     event.time        = CurrentTime;
     event.x           = 1;
     event.y           = 1;
     event.x_root      = 1;
     event.y_root      = 1;
     event.same_screen = True;
     event.keycode     = XKeysymToKeycode(disp, keysym);
     event.state       = modifiers;
     event.type        = (press) ? KeyPress : KeyRelease;
 
     XSendEvent(event.display, event.window, True, KeyPressMask, (XEvent *)&event);"))

; Call f with the XDisplay object representing the default display.
; The display is freed after use.
(define (with-x-display f)
  (define (open)
    (foreign-value "XOpenDisplay(NULL)" c-pointer))
  (define close
    (foreign-lambda* void ((c-pointer d)) "XCloseDisplay(d);"))
  (let ((x-display (open)))
    (f x-display)
    (close x-display)))

; Call f with the currently focused window.
; disp is the display, obtained via with-x-display.
; The window is freed after use. (In fact, it's only a pointer to an unsigned
; long (probably) but as Chicken's FFI doesn't let us use opaque typedefs,
; it's being passed as a void pointer and freed afterwards.)
(define (with-focused-window disp f)
  (define get-focused-window
    (foreign-lambda* c-pointer ((c-pointer disp)) "
      Window *focused;
      int revert;
      focused = (Window *)malloc(sizeof(Window));
      XGetInputFocus(disp, focused, &revert);
      C_return(focused);"))
  (define clean-up
    (foreign-lambda* void ((c-pointer focused)) "
      free(focused);"))
  (let ((focused-window (get-focused-window disp)))
    (f focused-window)
    (clean-up focused-window)))

; Return the bit mask for a named modifier
(define (xk-modifier-mask name)
  (define mapping
    '(("Shift" .   1)
      ("Control" . 4)
      ("Mod1" .    8)
      ("Mod2" .   16)
      ("Mod3" .   32)
      ("Mod4" .   64)
      ("Mod5" .  128)))
  (cdr (assoc name mapping)))

; Convert a list of named modifiers to a bit mask
(define (list->modifier-mask modifiers)
  (let loop ((state 0) (modifiers modifiers))
    (if (null? modifiers)
      state
      (loop (bitwise-ior state (xk-modifier-mask (car modifiers)))
            (cdr modifiers)))))

; Send a fake keypress (press, then release).
; name is a symbol from keysymdef.h without the leading XK_.
; modifiers is a list of "Shift", "Control", and "Mod1" through "Mod5".
; Case is significant (because "a" is different to "A").
(define (fake-keypress name . modifiers)
  (let ((keysym (hash-table-ref keysymdef:table name))
        (modmask (list->modifier-mask modifiers)))
    (with-x-display (lambda (disp)
      (with-focused-window disp (lambda (win)
        (send-key-event disp win #t keysym modmask)
        (send-key-event disp win #f keysym modmask)))))))

)
