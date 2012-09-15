(foreign-declare "#include <sys/ioctl.h>")
(foreign-declare "#include <linux/hidraw.h>")

; Return the vendor ID and product ID for file descriptor fd as a single 32-bit
; value
(define hid-device-id
  (foreign-lambda* unsigned-int32 ((int fd))
    "struct hidraw_devinfo devinfo;
     ioctl(fd, HIDIOCGRAWINFO, &devinfo);
     C_return(devinfo.vendor * 0x10000 + devinfo.product);"))
