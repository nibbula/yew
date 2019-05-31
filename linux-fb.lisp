;;
;; linux-fb.lisp - Interface to the Linux framebuffer device.
;;

(defpackage :linux-fb
  (:documentation "Interface to the Linux framebuffer device.")
  (:use :cl :dlib :opsys :opsys-unix :cffi :table :table-print :collections)
  (:export
   #:memory-region
   #:memory-region-start
   #:memory-region-length
   #:framebuffer
   #:framebuffer-fd
   #:framebuffer-name
   #:framebuffer-id
   #:framebuffer-type
   #:framebuffer-hardware-type
   #:framebuffer-text-type
   #:framebuffer-visual
   #:framebuffer-line-length
   #:framebuffer-width
   #:framebuffer-height
   #:framebuffer-depth
   #:framebuffer-red
   #:framebuffer-green
   #:framebuffer-blue
   #:framebuffer-width-mm
   #:framebuffer-height-mm
   #:framebuffer-system-memory
   #:framebuffer-mapped-memory
   #:framebuffer-memory
   #:start
   #:describe-framebuffer
   #:done
   #:context
   #:context-fb
   #:context-mem
   #:context-stride
   #:*context*
   #:new-gc
   #:pixel-to-color
   #:color-pixel
   #:get-pixel
   #:set-pixel
   #:rectangle
   #:rectangle-fill
   #:line
   #:show-off
   ))
(in-package :linux-fb)

;; (declaim (optimize (speed 0) (safety 3) (debug 2) (space 0) (compilation-speed 0)))
(declaim (optimize (safety 3) (debug 3)))
;; (declaim (optimize (speed 3) (safety 0) (debug 0) (space 0) (compilation-speed 0)))

(defstruct memory-region
  start
  length)

(defstruct framebuffer
  fd
  name
  id
  type
  hardware-type
  text-type
  visual
  (line-length  0 :type fixnum)		; in bytes
  (width        0 :type fixnum)		; in pixels
  (height       0 :type fixnum)		; in pixels
  (depth        0 :type fixnum)		; in bits per pixel
  red green blue
  width-mm
  height-mm
  system-memory
  mapped-memory
  memory)

(defcstruct fb-copyarea
  (dx       :uint32)
  (dy       :uint32)
  (width    :uint32)
  (height   :uint32)
  (sx       :uint32)
  (sy       :uint32))

(defcstruct fb-vblank
  (flags    :uint32)			; FB_VBLANK flags
  (count    :uint32)			; counter of retraces since boot
  (vcount   :uint32)			; current scanline position
  (hcount   :uint32)			; current scandot position
  (reserved :uint32 :count 4))		; reserved for future compatibility

(defcstruct fb-cmap
  (start   :uint32)			; First entry
  (len     :uint32)			; Number of entries
  (red     (:pointer :uint16))		; Red values
  (green   (:pointer :uint16))
  (blue    (:pointer :uint16))
  (transp  (:pointer :uint16)))		; transparency, can be NULL

(defconstant +ROP-COPY+ 0)
(defconstant +ROP-XOR+  1)

(defcstruct fb-fillrect
  (dx       :uint32)				; screen-relative
  (dy       :uint32)
  (width    :uint32)
  (height   :uint32)
  (color    :uint32)
  (rop      :uint32))

(defcstruct fb-image
  (dx       :uint32)			; Where to place image
  (dy       :uint32)
  (width    :uint32)			; Size of image
  (height   :uint32)
  (fg_color :uint32)			; Only used when a mono bitmap
  (bg_color :uint32)
  (depth    :uint8)			; Depth of the image
  (data     (:pointer :char))		; Pointer to image data
  (cmap     (:struct fb-cmap)))		; color map info

(defcstruct fbcurpos
  (x :uint16)
  (y :uint16))

(defconstant +FB-CUR-SETIMAGE+  #x01)
(defconstant +FB-CUR-SETPOS+    #x02)
(defconstant +FB-CUR-SETHOT+    #x04)
(defconstant +FB-CUR-SETCMAP+   #x08)
(defconstant +FB-CUR-SETSHAPE+  #x10)
(defconstant +FB-CUR-SETSIZE+   #x20)
(defconstant +FB-CUR-SETALL+    #xFF)

(defcstruct fb-cursor
  (set    :uint16)		; what to set
  (enable :uint16)		; cursor on/off
  (rop    :uint16)		; bitop operation
  (mask   (:pointer :char))	; cursor mask bits
  (hot    (:struct fbcurpos))	; cursor hot spot
  (image  (:struct fb-image)))	; Cursor image

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant _IOC_NRBITS	8)
  (defconstant _IOC_TYPEBITS	8)
  (defconstant _IOC_SIZEBITS	14)
  (defconstant _IOC_DIRBITS	2)

  (defconstant _IOC_NONE	0)
  (defconstant _IOC_WRITE	1)
  (defconstant _IOC_READ	2)

  (defconstant _IOC_NRSHIFT	0)
  (defconstant _IOC_TYPESHIFT	(+ _IOC_NRSHIFT   _IOC_NRBITS))
  (defconstant _IOC_SIZESHIFT	(+ _IOC_TYPESHIFT _IOC_TYPEBITS))
  (defconstant _IOC_DIRSHIFT	(+ _IOC_SIZESHIFT _IOC_SIZEBITS))

  (defun _IOC (dir type nr size)
    (logior (ash dir  _IOC_DIRSHIFT)
	    (ash type _IOC_TYPESHIFT)
	    (ash nr   _IOC_NRSHIFT)
	    (ash size _IOC_SIZESHIFT)))

  (defun _IOC_TYPECHECK (type) (cffi:foreign-type-size type))

  (defun IOR (type nr size)
    (_IOC _IOC_READ (char-code type) nr (_IOC_TYPECHECK size)))
  (defun IOW  (type nr size)
    (_IOC _IOC_WRITE (char-code type) nr (_IOC_TYPECHECK size)))
  (defun IOWR (type nr size)
    (_IOC (logior _IOC_READ _IOC_WRITE) (char-code type) nr (_IOC_TYPECHECK size))))

(defconstant +FBIOGET-VSCREENINFO+      #x4600)
(defconstant +FBIOPUT-VSCREENINFO+      #x4601)
(defconstant +FBIOGET-FSCREENINFO+      #x4602)
(defconstant +FBIOGETCMAP+              #x4604)
(defconstant +FBIOPUTCMAP+              #x4605)
(defconstant +FBIOPAN-DISPLAY+          #x4606)
(defconstant +FBIO-CURSOR+              (IOWR #\F #x08 '(:struct fb-cursor)))

(defconstant +FBIOGET-CON2FBMAP+        #x460F)
(defconstant +FBIOPUT-CON2FBMAP+        #x4610)
(defconstant +FBIOBLANK+	        #x4611) ; arg: 0 or vesa level + 1
(defconstant +FBIOGET-VBLANK+	        (IOR #\F #x12 '(:struct fb-vblank)))
(defconstant +FBIO-ALLOC+               #x4613)
(defconstant +FBIO-FREE+                #x4614)
(defconstant +FBIOGET-GLYPH+            #x4615)
(defconstant +FBIOGET-HWCINFO+          #x4616)
(defconstant +FBIOPUT-MODEINFO+         #x4617)
(defconstant +FBIOGET-DISPINFO+         #x4618)
(defconstant +FBIO-WAITFORVSYNC+        (IOW #\F #x20 :uint32))

(defparameter *fb-types* nil)
(define-to-list *fb-types*
  #(#(+FB-TYPE-PACKED-PIXELS+       0 "Packed Pixels")
    #(+FB-TYPE-PLANES+              1 "Non interleaved planes")
    #(+FB-TYPE-INTERLEAVED-PLANES+  2 "Interleaved planes")
    #(+FB-TYPE-TEXT+                3 "Text/attributes")
    #(+FB-TYPE-VGA-PLANES+          4 "EGA/VGA planes")
    #(+FB-TYPE-FOURCC+              5 "Type identified by a V4L2 FOURCC")))

(defparameter *fb-aux-text-types* nil)
(define-to-list *fb-aux-text-types*
  #(#(+FB-AUX-TEXT-MDA+             0 "Monochrome text")
    #(+FB-AUX-TEXT-CGA+             1 "CGA/EGA/VGA Color text")
    #(+FB-AUX-TEXT-S3-MMIO+         2 "S3 MMIO fasttext")
    #(+FB-AUX-TEXT-MGA-STEP16+      3 "MGA Millenium I: text, attr, 14 reserved bytes")
    #(+FB-AUX-TEXT-MGA-STEP8+       4 "other MGAs:      text, attr,  6 reserved bytes")
    #(+FB-AUX-TEXT-SVGA-GROUP+      8 "8-15: SVGA tileblit compatible modes")
    #(+FB-AUX-TEXT-SVGA-MASK+       7 "lower three bits says step")
    #(+FB-AUX-TEXT-SVGA-STEP2+      8 "SVGA text mode:  text, attr")
    #(+FB-AUX-TEXT-SVGA-STEP4+      9 "SVGA text mode:  text, attr,  2 reserved bytes")
    #(+FB-AUX-TEXT-SVGA-STEP8+     10 "SVGA text mode:  text, attr,  6 reserved bytes")
    #(+FB-AUX-TEXT-SVGA-STEP16+    11 "SVGA text mode:  text, attr, 14 reserved bytes")
    #(+FB-AUX-TEXT-SVGA-LAST+      15 "reserved up to 15")))

(defconstant +FB-AUX-VGA-PLANES-VGA4+ 0	"16 color planes (EGA/VGA)")
(defconstant +FB-AUX-VGA-PLANES-CFB4+ 1	"CFB4 in planes (VGA)")
(defconstant +FB-AUX-VGA-PLANES-CFB8+ 2	"CFB8 in planes (VGA)")

(defparameter *fb-visuals* nil)
(define-to-list *fb-visuals*
  #(#(+FB-VISUAL-MONO01+             0 "Monochr. 1=Black 0=White")
    #(+FB-VISUAL-MONO10+             1 "Monochr. 1=White 0=Black")
    #(+FB-VISUAL-TRUECOLOR+          2 "True color")
    #(+FB-VISUAL-PSEUDOCOLOR+        3 "Pseudo color (like atari)")
    #(+FB-VISUAL-DIRECTCOLOR+        4 "Direct color")
    #(+FB-VISUAL-STATIC-PSEUDOCOLOR+ 5 "Pseudo color readonly")
    #(+FB-VISUAL-FOURCC+             6 "Visual identified by a V4L2 FOURCC")))

(defparameter *fb-accel-types* nil)
(define-to-list *fb-accel-types*
  #(#(+FB-ACCEL-NONE+                  0 "no hardware accelerator")
    #(+FB-ACCEL-ATARIBLITT+            1 "Atari Blitter")
    #(+FB-ACCEL-AMIGABLITT+            2 "Amiga Blitter")
    #(+FB-ACCEL-S3-TRIO64+             3 "Cybervision64 (S3 Trio64)")
    #(+FB-ACCEL-NCR-77C32BLT+          4 "RetinaZ3 (NCR 77C32BLT)")
    #(+FB-ACCEL-S3-VIRGE+              5 "Cybervision64/3D (S3 ViRGE)")
    #(+FB-ACCEL-ATI-MACH64GX+          6 "ATI Mach 64GX family")
    #(+FB-ACCEL-DEC-TGA+               7 "DEC 21030 TGA")
    #(+FB-ACCEL-ATI-MACH64CT+          8 "ATI Mach 64CT family")
    #(+FB-ACCEL-ATI-MACH64VT+          9 "ATI Mach 64CT family VT class")
    #(+FB-ACCEL-ATI-MACH64GT+         10 "ATI Mach 64CT family GT class")
    #(+FB-ACCEL-SUN-CREATOR+          11 "Sun Creator/Creator3D")
    #(+FB-ACCEL-SUN-CGSIX+            12 "Sun cg6")
    #(+FB-ACCEL-SUN-LEO+              13 "Sun leo/zx")
    #(+FB-ACCEL-IMS-TWINTURBO+        14 "IMS Twin Turbo")
    #(+FB-ACCEL-3DLABS-PERMEDIA2+     15 "3Dlabs Permedia 2")
    #(+FB-ACCEL-MATROX-MGA2064W+      16 "Matrox MGA2064W (Millenium)")
    #(+FB-ACCEL-MATROX-MGA1064SG+     17 "Matrox MGA1064SG (Mystique)")
    #(+FB-ACCEL-MATROX-MGA2164W+      18 "Matrox MGA2164W (Millenium II)")
    #(+FB-ACCEL-MATROX-MGA2164W-AGP+  19 "Matrox MGA2164W (Millenium II)")
    #(+FB-ACCEL-MATROX-MGAG100+       20 "Matrox G100 (Productiva G100)")
    #(+FB-ACCEL-MATROX-MGAG200+       21 "Matrox G200 (Myst, Mill, ...)")
    #(+FB-ACCEL-SUN-CG14+             22 "Sun cgfourteen")
    #(+FB-ACCEL-SUN-BWTWO+            23 "Sun bwtwo")
    #(+FB-ACCEL-SUN-CGTHREE+          24 "Sun cgthree")
    #(+FB-ACCEL-SUN-TCX+              25 "Sun tcx")
    #(+FB-ACCEL-MATROX-MGAG400+       26 "Matrox G400")
    #(+FB-ACCEL-NV3+                  27 "nVidia RIVA 128")
    #(+FB-ACCEL-NV4+                  28 "nVidia RIVA TNT")
    #(+FB-ACCEL-NV5+                  29 "nVidia RIVA TNT2")
    #(+FB-ACCEL-CT-6555x+             30 "C&T 6555x")
    #(+FB-ACCEL-3DFX-BANSHEE+         31 "3Dfx Banshee")
    #(+FB-ACCEL-ATI-RAGE128+          32 "ATI Rage128 family")
    #(+FB-ACCEL-IGS-CYBER2000+        33 "CyberPro 2000")
    #(+FB-ACCEL-IGS-CYBER2010+        34 "CyberPro 2010")
    #(+FB-ACCEL-IGS-CYBER5000+        35 "CyberPro 5000")
    #(+FB-ACCEL-SIS-GLAMOUR+          36 "SiS 300/630/540")
    #(+FB-ACCEL-3DLABS-PERMEDIA3+     37 "3Dlabs Permedia 3")
    #(+FB-ACCEL-ATI-RADEON+           38 "ATI Radeon family")
    #(+FB-ACCEL-I810+                 39 "Intel 810/815")
    #(+FB-ACCEL-SIS-GLAMOUR-2+        40 "SiS 315, 650, 740")
    #(+FB-ACCEL-SIS-XABRE+            41 "SiS 330 ("Xabre")")
    #(+FB-ACCEL-I830+                 42 "Intel 830M/845G/85x/865G")
    #(+FB-ACCEL-NV-10+                43 "nVidia Arch 10")
    #(+FB-ACCEL-NV-20+                44 "nVidia Arch 20")
    #(+FB-ACCEL-NV-30+                45 "nVidia Arch 30")
    #(+FB-ACCEL-NV-40+                46 "nVidia Arch 40")
    #(+FB-ACCEL-XGI-VOLARI-V+         47 "XGI Volari V3XT, V5, V8")
    #(+FB-ACCEL-XGI-VOLARI-Z+         48 "XGI Volari Z7")
    #(+FB-ACCEL-OMAP1610+             49 "TI OMAP16xx")
    #(+FB-ACCEL-TRIDENT-TGUI+         50 "Trident TGUI")
    #(+FB-ACCEL-TRIDENT-3DIMAGE+      51 "Trident 3DImage")
    #(+FB-ACCEL-TRIDENT-BLADE3D+      52 "Trident Blade3D")
    #(+FB-ACCEL-TRIDENT-BLADEXP+      53 "Trident BladeXP")
    #(+FB-ACCEL-CIRRUS-ALPINE+        53 "Cirrus Logic 543x/544x/5480")
    #(+FB-ACCEL-NEOMAGIC-NM2070+      90 "NeoMagic NM2070")
    #(+FB-ACCEL-NEOMAGIC-NM2090+      91 "NeoMagic NM2090")
    #(+FB-ACCEL-NEOMAGIC-NM2093+      92 "NeoMagic NM2093")
    #(+FB-ACCEL-NEOMAGIC-NM2097+      93 "NeoMagic NM2097")
    #(+FB-ACCEL-NEOMAGIC-NM2160+      94 "NeoMagic NM2160")
    #(+FB-ACCEL-NEOMAGIC-NM2200+      95 "NeoMagic NM2200")
    #(+FB-ACCEL-NEOMAGIC-NM2230+      96 "NeoMagic NM2230")
    #(+FB-ACCEL-NEOMAGIC-NM2360+      97 "NeoMagic NM2360")
    #(+FB-ACCEL-NEOMAGIC-NM2380+      98 "NeoMagic NM2380")
    #(+FB-ACCEL-PXA3XX+               99 "PXA3xx")
    #(+FB-ACCEL-SAVAGE4+            #x80 "S3 Savage4")
    #(+FB-ACCEL-SAVAGE3D+           #x81 "S3 Savage3D")
    #(+FB-ACCEL-SAVAGE3D-MV+        #x82 "S3 Savage3D-MV")
    #(+FB-ACCEL-SAVAGE2000+         #x83 "S3 Savage2000")
    #(+FB-ACCEL-SAVAGE-MX-MV+       #x84 "S3 Savage/MX-MV")
    #(+FB-ACCEL-SAVAGE-MX+          #x85 "S3 Savage/MX")
    #(+FB-ACCEL-SAVAGE-IX-MV+       #x86 "S3 Savage/IX-MV")
    #(+FB-ACCEL-SAVAGE-IX+          #x87 "S3 Savage/IX")
    #(+FB-ACCEL-PROSAVAGE-PM+       #x88 "S3 ProSavage PM133")
    #(+FB-ACCEL-PROSAVAGE-KM+       #x89 "S3 ProSavage KM133")
    #(+FB-ACCEL-S3TWISTER-P+        #x8a "S3 Twister")
    #(+FB-ACCEL-S3TWISTER-K+        #x8b "S3 TwisterK")
    #(+FB-ACCEL-SUPERSAVAGE+        #x8c "S3 Supersavage")
    #(+FB-ACCEL-PROSAVAGE-DDR+      #x8d "S3 ProSavage DDR")
    #(+FB-ACCEL-PROSAVAGE-DDRK+     #x8e "S3 ProSavage DDR-K")
    #(+FB-ACCEL-PUV3-UNIGFX+        #xa0 "PKUnity-v3 Unigfx")))

(defconstant +FB-CAP-FOURCC+	1 "Device supports FOURCC-based formats")

(defcstruct fb-fix-screeninfo
  (id           :char :count 16)   ; identification string eg "TT Builtin"
  (smem-start   :unsigned-long)    ; Start of frame buffer mem (phys addr)
  (smem-len     :uint32)           ; Length of frame buffer mem
  (type         :uint32)           ; see FB_TYPE_*
  (type-aux     :uint32)           ; Interleave for interleaved Planes
  (visual       :uint32)           ; see FB_VISUAL_*
  (xpanstep     :uint16)           ; zero if no hardware panning
  (ypanstep     :uint16)           ; zero if no hardware panning
  (ywrapstep    :uint16)           ; zero if no hardware ywrap
  (line-length  :uint32)           ; length of a line in bytes
  (mmio-start   :unsigned-long)    ; Start of Memory Mapped I/O (phys addr)
  (mmio-len     :uint32)           ; Length of Memory Mapped I/O
  (accel        :uint32)           ; Indicate to driver the specific chip/card
  (capabilities :uint16)           ; see FB_CAP_*
  (reserved     :uint16 :count 2)) ; Reserved for future compatibility

#|
## Interpretation of offset for color fields:
## All offsets are from the right, inside a pixel value, which is exactly
## ‘bits-per-pixel’ wide. This means you can use the offset as the count
## argument to ash. A pixel afterwards is a bit stream and is written to video
## memory as that unmodified.
##
## For pseudocolor: offset and length should be the same for all color
## components. Offset specifies the position of the least significant bit
## of the pallette index in a pixel value. Length indicates the number
## of available palette entries (i.e. # of entries = (ash 1 length).
|#

(defcstruct fb-bitfield
  (offset :uint32)	; beginning of bitfield
  (length :uint32)	; length of bitfield
  (msb_right :uint32))	; != 0 : Most significant bit is right

(defconstant +FB-NONSTD-HAM+            1 "Hold-And-Modify (HAM)")
(defconstant +FB-NONSTD-REV-PIX-IN-B+   2 "order of pixels in each byte is reversed")

(defconstant +FB-ACTIVATE-NOW+          0 "set values immediately (or vbl)")
(defconstant +FB-ACTIVATE-NXTOPEN+      1 "activate on next open")
(defconstant +FB-ACTIVATE-TEST+         2 "don't set, round up impossible values")
(defconstant +FB-ACTIVATE-MASK+        15)

(defconstant +FB-ACTIVATE-VBL+          16 "activate values on next vbl")
(defconstant +FB-CHANGE-CMAP-VBL+       32 "change colormap on vbl")
(defconstant +FB-ACTIVATE-ALL+          64 "change all VCs on this fb")
(defconstant +FB-ACTIVATE-FORCE+       128 "force apply even when no change")
(defconstant +FB-ACTIVATE-INV-MODE+    256 "invalidate videomode")

(defconstant +FB-ACCELF-TEXT+          1 "(OBSOLETE) see fb_info.flags and vc_mode")

(defconstant +FB-SYNC-HOR-HIGH-ACT+      1 "horizontal sync high active")
(defconstant +FB-SYNC-VERT-HIGH-ACT+     2 "vertical sync high active")
(defconstant +FB-SYNC-EXT+               4 "external sync")
(defconstant +FB-SYNC-COMP-HIGH-ACT+     8 "composite sync high active")
(defconstant +FB-SYNC-BROADCAST+        16 "broadcast video timings")
                                        ; vtotal = 144d/288n/576i => PAL
                                        ; vtotal = 121d/242n/484i => NTSC
(defconstant +FB-SYNC-ON-GREEN+         32 "sync on green")

(defparameter *fb-vmode-list* nil)

(defconstant +FB-VMODE-NONINTERLACED+   0 "non interlaced")
(defconstant +FB-VMODE-INTERLACED+      1 "interlaced")
(defconstant +FB-VMODE-DOUBLE+          2 "double scan")
(defconstant +FB-VMODE-ODD-FLD-FIRST+   4 "interlaced: top line first")
(defconstant +FB-VMODE-MASK+          255)

(defconstant +FB-VMODE-YWRAP+         256 "ywrap instead of panning")
(defconstant +FB-VMODE-SMOOTH-XPAN+   512 "smooth xpan possible (internally used)")
(defconstant +FB-VMODE-CONUPDATE+     512 "don't update x/yoffset")

(defconstant +FB-ROTATE-UR+      0)
(defconstant +FB-ROTATE-CW+      1)
(defconstant +FB-ROTATE-UD+      2)
(defconstant +FB-ROTATE-CCW+     3)

(defun picoseconds-to-kHz (a) (/ 1000000000 a))
(defun kHz-to-picoseconds (a) (/ 1000000000 a))

(defcstruct fb-var-screeninfo
  (xres             :uint32)		   ; visible resolution
  (yres             :uint32)
  (xres-virtual     :uint32)		   ; virtual resolution
  (yres-virtual     :uint32)
  (xoffset          :uint32)	           ; offset from virtual to visible
  (yoffset          :uint32)	           ; resolution
  (bits-per-pixel   :uint32)	           ; guess what
  (grayscale        :uint32)	           ; 0 = color 1 = grayscale >1 = FOURCC
  (red              (:struct fb-bitfield)) ; bitfield in fb mem if true color,
  (green            (:struct fb-bitfield)) ; else only length is significant
  (blue             (:struct fb-bitfield))
  (transp           (:struct fb-bitfield)) ; transparency
  (nonstd           :uint32)		   ; != 0 Non standard pixel format
  (activate         :uint32)		   ; see FB_ACTIVATE_*
  (height           :uint32)		   ; height of picture in mm
  (width            :uint32)		   ; width of picture in mm
  (accel-flags      :uint32)		   ; (OBSOLETE) see fb_info.flags
  ;; Timing: All values in pixclocks, except pixclock (of course)
  (pixclock         :uint32)               ; pixel clock in ps (pico seconds)
  (left-margin      :uint32)               ; time from sync to picture
  (right-margin     :uint32)               ; time from picture to sync
  (upper-margin     :uint32)               ; time from sync to picture
  (lower-margin     :uint32)
  (hsync-len        :uint32)               ; length of horizontal sync
  (vsync-len        :uint32)               ; length of vertical sync
  (sync             :uint32)               ; see FB_SYNC_*
  (vmode            :uint32)               ; see FB_VMODE_*
  (rotate           :uint32)               ; angle we rotate counter clockwise
  (colorspace       :uint32)               ; colorspace for FOURCC-based modes
  (reserved         :uint32 :count 4))     ; Reserved for future compatibility

(defcstruct fb-con2fbmap
  (console     :uint32)
  (framebuffer :uint32))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +VESA-NO-BLANKING+        0)
  (defconstant +VESA-VSYNC-SUSPEND+      1)
  (defconstant +VESA-HSYNC-SUSPEND+      2)
  (defconstant +VESA-POWERDOWN+          3))

(defparameter *fb-blank-types* nil)
(define-to-list *fb-blank-types*
    #(#(+FB-BLANK-UNBLANK+        +VESA-NO-BLANKING+
	"screen: unblanked, hsync: on,  vsync: on")
      #(+FB-BLANK-NORMAL+         #.(1+ +VESA-NO-BLANKING+)
	"screen: blanked,   hsync: on,  vsync: on")
      #(+FB-BLANK-VSYNC-SUSPEND+  #.(1+ +VESA-VSYNC-SUSPEND+)
	"screen: blanked,   hsync: on,  vsync: off")
      #(+FB-BLANK-HSYNC-SUSPEND+  #.(1+ +VESA-HSYNC-SUSPEND+)
	"screen: blanked,   hsync: off, vsync: on")
      #(+FB-BLANK-POWERDOWN+      #.(1+ +VESA-POWERDOWN+)
	"screen: blanked,   hsync: off, vsync: off")))

(defconstant +FB-VBLANK-VBLANKING+      #x001 "currently in a vertical blank")
(defconstant +FB-VBLANK-HBLANKING+      #x002 "currently in a horizontal blank")
(defconstant +FB-VBLANK-HAVE-VBLANK+    #x004 "vertical blanks can be detected")
(defconstant +FB-VBLANK-HAVE-HBLANK+    #x008 "horizontal blanks can be detected")
(defconstant +FB-VBLANK-HAVE-COUNT+     #x010 "global retrace counter is available")
(defconstant +FB-VBLANK-HAVE-VCOUNT+    #x020 "the vcount field is valid")
(defconstant +FB-VBLANK-HAVE-HCOUNT+    #x040 "the hcount field is valid")
(defconstant +FB-VBLANK-VSYNCING+       #x080 "currently in a vsync")
(defconstant +FB-VBLANK-HAVE-VSYNC+     #x100 "verical syncs can be detected")

(defconstant +FB-BACKLIGHT-LEVELS+ 128)
(defconstant +FB-BACKLIGHT-MAX+    #xFF)

(defun start (&key (device-name "/dev/fb0"))
  "Start using the framebuffer in DEVICE-NAME. Return a FRAMEBUFFER structure.
DONE should be called when done using it to free resources."
  (let ((fd (error-check (posix-open device-name (logior +O_RDWR+ +O_CLOEXEC+) 0)
			 "Can't open the framebuffer ~s." device-name))
	pointer length)
    (with-foreign-objects ((fixed-info '(:struct fb-fix-screeninfo))
			   (var-info '(:struct fb-var-screeninfo)))
      (error-check (posix-ioctl fd +FBIOGET-FSCREENINFO+ fixed-info)
		   "Can't get fixed screen info.")
      (error-check (posix-ioctl fd +FBIOGET-VSCREENINFO+ var-info)
		   "Can't get variable screen info.")
      (with-foreign-slots ((id type visual type-aux line-length accel
			    capabilities smem-start smem-len
			    mmio-start mmio-len)
			   fixed-info (:struct fb-fix-screeninfo))
	(with-foreign-slots ((xres yres bits-per-pixel width height
			      red green blue)
			     var-info (:struct fb-var-screeninfo))
	  (multiple-value-setq (pointer length)
	    (map-file device-name :mapping-type :shared :access '(:read :write)
		      :length smem-len))
	  (make-framebuffer
	   :fd fd
	   :name device-name
	   :id (foreign-string-to-lisp id :count 15)
	   :type (find visual *fb-types* :key (_ (symbol-value _)))
	   :hardware-type (find visual *fb-accel-types* :key (_ (symbol-value _)))
	   :text-type (find type-aux *fb-aux-text-types* :key (_ (symbol-value _)))
	   :visual (find visual *fb-visuals* :key (_ (symbol-value _)))
	   :line-length line-length
	   :width xres
	   :height yres
	   :depth bits-per-pixel
	   :red red
	   :green green
	   :blue blue
	   :width-mm (when (/= width #xffffffff) width)
	   :height-mm (when (/= height #xffffffff) height)
	   :system-memory (make-memory-region
			   :start smem-start
			   :length smem-len)
	   :mapped-memory (make-memory-region
			   :start mmio-start
			   :length mmio-len)
	   :memory (make-memory-region
		    :start pointer
		    :length length)))))))

(defun describe-framebuffer (fb)
  (table-print:print-table
   (table:make-table-from fb)))

(defun done (fb)
  "Close and unmap the framebuffer FB."
  (error-check (posix-close (framebuffer-fd fb))
	       "Failed to close the framebuffer ~s." (framebuffer-name fb))
  (setf (framebuffer-fd fb) nil)
  (let* ((mem (framebuffer-memory fb))
	 (ptr (and mem (memory-region-start mem)))
	 (len (and mem (memory-region-length mem))))
    (when (pointerp ptr)
      (error-check (munmap ptr len)
		   "Failed to unmap the framebuffer ~s" (framebuffer-name fb)))
    (setf (framebuffer-memory fb) nil)))

(defun pixel-to-color (pixel)
  "Convert a framebuffer pixel into a color structure."
  (color:make-color
   :rgb8
   :red   (ash (logand pixel #x00ff0000) -16)
   :green (ash (logand pixel #x0000ff00) -8)
   :blue       (logand pixel #x000000ff)))

(defun color-pixel (r g b)
  "Return a framebuffer pixel value."
  (logior (ash (logand r #xff) 16)
	  (ash (logand g #xff) 8)
	       (logand b #xff)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Drawing

;; @@@ Do everything the slow easy way for now.

(defstruct context
  fb
  mem
  stride)

(defvar *context* nil
  "Current graphics context.")

(defun new-gc (fb)
  (let* ((nbby 8)
	 (bytes-per-pixel (/ (framebuffer-depth fb) nbby)))
    (setf *context*
	  (make-context
	   :fb fb
	   :mem (memory-region-start (framebuffer-memory fb))
	   :stride (/ (framebuffer-line-length fb) bytes-per-pixel)))))
	
(defmacro pixel-at (x y mem stride)
  `(mem-aref ,mem :uint32 (+ (* ,y ,stride) ,x)))

(defun get-pixel (x y)
  (let ((mem (context-mem *context*))
	(stride (context-stride *context*)))
    (pixel-at x y mem stride)))

(defun set-pixel (x y pixel)
  "Set a pixel at X Y in FB to PIXEL."
  (let ((mem (context-mem *context*))
	(stride (context-stride *context*)))
    (setf (pixel-at x y mem stride) pixel)))

(defun scanline-set (x y n pixel mem stride)
  (let ((start (+ (* y stride) x)))
    (dotimes (i n)
      (setf (mem-aref mem :uint32 (+ start i)) pixel))))

(defun rectangle-fill (x1 y1 x2 y2 pixel)
  (let ((mem (context-mem *context*))
	(stride (context-stride *context*)))
    (loop :for y :from y1 :below y2 :do
	 (scanline-set x1 y (- x2 x1) pixel mem stride))))

(defun rectangle (x1 y1 x2 y2 pixel)
  (let ((mem (context-mem *context*))
	(stride (context-stride *context*)))
    ;;(loop :for x :from x1 :below x2 :do (set-pixel x  y1 pixel))
    (scanline-set x1 y1 (- x2 x1) pixel mem stride)
    (loop :for y :from y1 :below y2 :do (set-pixel x1 y  pixel))
    (loop :for y :from y1 :below y2 :do (set-pixel x2 y  pixel))
    ;;(loop :for x :from x1 :below x2 :do (set-pixel x  y2 pixel)))
    (scanline-set x1 y2 (- x2 x1) pixel mem stride)))

(defun line (start-x start-y end-x end-y pixel)
  (let* ((x-inc (signum (- end-x start-x)))
	 (y-inc (signum (- end-y start-y)))
	 (slope (when (not (zerop (- end-x start-x)))
		  (/ (- end-y start-y) (- end-x start-x)))))
    (cond
      ((not slope)
       ;; vertical line
       (when (minusp y-inc)
	 (rotatef start-y end-y))

       (loop :for y :from start-y :below end-y :do
	  (set-pixel start-x y pixel)))

      ((zerop slope) ;; horizontal
       (when (minusp x-inc)
	 (rotatef start-x end-x))

       (loop :for x :from start-x :below end-x :do
	  (set-pixel x start-y pixel)))

       (t ;; sloped line
	(when (< x-inc 0)
	  (rotatef start-x end-x)
	  (rotatef start-y end-y))
	;;(when (< y-inc 0) (rotatef start-y end-y))

	(loop
	   ;;:with x-dist = (abs (- start-x end-x))
	   ;;:and  y-dist = (abs (- start-y end-y))
	   :for x :from start-x :below end-x
	   :for y = start-y :then (+ y slope)
	   :do

	   (set-pixel (truncate x) (truncate y) pixel)
	   
	   ;;(tt-move-to (- (tt-height) 2) 0)
	   ;;(tt-format "x = ~s y = ~s" x y)
	   ;;(tt-get-key)
	   )))))

(defun clip-rect (x y end-x end-y)
  "Return the input rectangle clipped to the framebuffer."
  (values (max x 0) (max y 0)
	  (min end-x (1- (framebuffer-width (context-fb *context*))))
	  (min end-y (1- (framebuffer-height (context-fb *context*))))))

(defun put-image (image x y &key (subimage 0) #|(scale 1)|#)
  "Draw an image at coordinates X and Y."
  (multiple-value-bind (cx cy cw ch)
      (clip-rect x y
		 (+ x (image:image-width image))
		 (+ y (image:image-height image)))
    (let* ((data (image:sub-image-data
		  (aref (image:image-subimages image) subimage)))
	   ;; (step (max 1 (round 1 scale)))
	   (start-x (- cx x))
	   (start-y (- cy y))
	   (end-x (- (image:image-width image)
		     (- (+ x (image:image-width image)) cw)))
	   (end-y (- (image:image-height image)
		     (- (+ y (image:image-height image)) ch))))
      (when (and (plusp (- end-y start-y))
		 (plusp (- end-x start-x)))
	(loop :for iy :from start-y :below end-y :do
	   (loop :for ix :from start-x :below end-x :do
	      (linux-fb:set-pixel
	       (+ ix x) (+ iy y)
	       (ash (image:get-whole-pixel data iy ix) -8))))))))


;; How to use:
;;
;; Setup:
;;
;; > (lab)
;; > l linux-fb
;; > pkg :linux-fb
;;
;; (defvar fb (start))
;; (new-gc fb)
;;
;; .. do some stuff ...
;; e.g. (show-off)
;;
;; (done fb)
;;
;; pkg

;; EOF
