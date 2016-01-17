;; -*- geiser-scheme-implementation: 'chicken -*-

(import foreign srfi-4 srfi-1)
(require-extension srfi-4 srfi-1)

(foreign-declare #<<ENDC
#include <GL/gl.h>
#include "nanovg/src/nanovg.h"
#include "nanovg/src/nanovg_gl.h"
#include <string.h>
ENDC
)

(define-syntax define-foreign-enum
  (syntax-rules ()
    ((define-foreign-enum name c-name)
     (define name (foreign-value c-name integer)))))

(define-foreign-enum create/anti-alias "NVG_ANTIALIAS")
(define-foreign-enum create/stencil-strokes "NVG_STENCIL_STROKES")
(define-foreign-enum create/debug "NVG_DEBUG")

(define-foreign-enum winding/ccw "NVG_CCW")
(define-foreign-enum winding/cw "NVG_CW")

(define-foreign-enum solidity/solid "NVG_SOLID")
(define-foreign-enum solidity/hole "NVG_HOLE")

(define-foreign-enum line-cap/butt "NVG_BUTT")
(define-foreign-enum line-cap/round "NVG_ROUND")
(define-foreign-enum line-cap/square "NVG_SQUARE")
(define-foreign-enum line-cap/bevel "NVG_BEVEL")
(define-foreign-enum line-cap/miter "NVG_MITER")

(define-foreign-enum align/left "NVG_ALIGN_LEFT")
(define-foreign-enum align/center "NVG_ALIGN_CENTER")
(define-foreign-enum align/right "NVG_ALIGN_RIGHT")
(define-foreign-enum align/top "NVG_ALIGN_TOP")
(define-foreign-enum align/middle "NVG_ALIGN_MIDDLE")
(define-foreign-enum align/bottom "NVG_ALIGN_BOTTOM")
(define-foreign-enum align/baseline "NVG_ALIGN_BASELINE")

(define-foreign-enum image/generate-mipmaps "NVG_IMAGE_GENERATE_MIPMAPS")
(define-foreign-enum image/repeat-x "NVG_IMAGE_REPEATX")
(define-foreign-enum image/repeat-y "NVG_IMAGE_REPEATY")
(define-foreign-enum image/flip-y "NVG_IMAGE_FLIPY")
(define-foreign-enum image/premultiplied "NVG_IMAGE_PREMULTIPLIED")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Context
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-type context (c-pointer (struct "NVGcontext")))

(define create-context*
  (case nanovg-gl-version
    ((gl2) (foreign-lambda* context ((int flags)) "C_return((intptr_t)nvgCreateGL2(flags));"))
    ((gl3) (foreign-lambda* context ((int flags)) "C_return((intptr_t)nvgCreateGL3(flags));"))
    ((gles2) (foreign-lambda* context ((int flags)) "C_return((intptr_t)nvgCreateGLES2(flags));"))
    ((gles3) (foreign-lambda* context ((int flags)) "C_return((intptr_t)nvgCreateGLES3(flags));"))))

(define delete-context!
  (case nanovg-gl-version
    ((gl2) (foreign-lambda void "nvgDeleteGL2" context))
    ((gl3) (foreign-lambda void "nvgDeleteGL3" context))
    ((gles2) (foreign-lambda void "nvgDeleteGLES2" context))
    ((gles3) (foreign-lambda void "nvgDeleteGLES3" context))))

(define (create-context #!key (anti-alias #f) (stencil-strokes #f) (debug #f) (flags #f))
  (let* ((flags
	  (or flags
	      (+ (if anti-alias create/anti-alias 0)
		 (if stencil-strokes create/stencil-strokes 0)
		 (if debug create/debug 0))))
	 (ctx (create-context* flags)))
    (set-finalizer! ctx delete-context!)
    ctx))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Color
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-type color (c-pointer (struct "NVGcolor")))

(define-syntax make-color-uninitialized
  (syntax-rules ()
    ((make-color-uninitialized)
     (make-blob (foreign-type-size "NVGcolor")))))

(define (make-color-rgb r g b)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (integer r) (integer g) (integer b)) "*clr = nvgRGB(r, g, b);") (location color) r g b)
    color))

(define (make-color-rgbf r g b)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float r) (float g) (float b)) "*clr = nvgRGBf(r, g, b);") (location color) r g b)
    color))

(define (make-color-rgba r g b a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (integer r) (integer g) (integer b) (integer a)) "*clr = nvgRGBA(r, g, b, a);") (location color) r g b a)
    color))

(define (make-color-rgbaf r g b a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float r) (float g) (float b) (float a)) "*clr = nvgRGBAf(r, g, b, a);") (location color) r g b a)
    color))

(define (make-color-lerp clr1 clr2 u)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (color clr1) (color clr2) (float u)) "*clr = nvgLerpRGBA(*clr1, *clr2, u);") (location color) clr1 clr2 u)
    color))

(define (make-color-transparency clr a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (color clr1) (integer a)) "*clr = nvgTransRGBA(*clr1, a);") (location color) clr a)
    color))

(define (make-color-transparencyf clr a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (color clr1) (float a)) "*clr = nvgTransRGBAf(*clr1, a);") (location color) clr a)
    color))

(define (make-color-hsl h s l)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float h) (float s) (float l)) "*clr = nvgHSL(h, s, l);") (location color) h s l)
    color))

(define (make-color-hsla h s l a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float h) (float s) (float l) (float a)) "*clr = nvgHSLA(h, s, l, a);") (location color) h s l a)
    color))

(define (color-red color)
  ((foreign-lambda* float ((color clr)) "C_return(clr->r);") (location color)))

(define (color-green color)
  ((foreign-lambda* float ((color clr)) "C_return(clr->g);") (location color)))

(define (color-blue color)
  ((foreign-lambda* float ((color clr)) "C_return(clr->b);") (location color)))

(define (color-alpha color)
  ((foreign-lambda* float ((color clr)) "C_return(clr->a);") (location color)))

(define (color-rgba color)
  (let ((buf (make-f32vector 4)))
    ((foreign-lambda* void ((color clr) (f32vector buf)) "memcpy(buf, clr->rgba, sizeof(float) * 4);") (location color) buf)
    buf))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-type paint (c-pointer (struct "NVGpaint")))

(define-syntax make-paint-uninitialized
  (syntax-rules ()
    ((make-paint-uninitialized)
     (make-blob (foreign-type-size "NVGpaint")))))

(define (paint-transform paint)
  (let ((buf (make-f32vector 6)))
    ((foreign-lambda* void ((paint p) (f32vector buf)) "memcpy(buf, p->xform, sizeof(float) * 6);") (location paint) buf)
    buf))

(define (paint-extent paint)
  (let ((buf (make-f32vector 2)))
    ((foreign-lambda* void ((paint p) (f32vector buf)) "memcpy(buf, p->extent, sizeof(float) *2);") (location paint) buf)
    buf))

(define (paint-radius paint)
  ((foreign-lambda* float ((paint p)) "C_return(p->radius);") (location paint)))

(define (paint-feather paint)
  ((foreign-lambda* float ((paint p)) "C_return(p->feather);") (location paint)))

(define (paint-inner-color paint)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((paint p) (color c)) "memcpy(c, &p->innerColor, sizeof(NVGcolor));") (location paint) color)
    color))

(define (paint-outer-color paint)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((paint p) (color c)) "memcpy(c, &p->outerColor, sizeof(NVGcolor));") (location paint) color)
    color))

(define (paint-image paint)
  ((foreign-lambda* integer ((paint p)) "C_return(p->image);") (location paint)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyph Position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record glyph-position
  index x minx maxx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record text-row
  start end next width minx maxx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin-frame! context window-width window-height pixel-ratio)
  ((foreign-lambda void "nvgBeginFrame" context integer integer float) context window-width window-height pixel-ratio))

(define (cancel-frame! context)
  ((foreign-lambda void "nvgCancelFrame" context) context))

(define (end-frame! context)
  ((foreign-lambda void "nvgEndFrame" context) context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (save-state! context)
  ((foreign-lambda void "nvgSave" context) context))

(define (restore-state! context)
  ((foreign-lambda void "nvgRestore" context) context))

(define (reset-state! context)
  ((foreign-lambda void "nvgReset" context) context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (stroke-color! context color)
  ((foreign-lambda* void ((context ctx) (color clr)) "nvgStrokeColor(ctx, *clr);") context (location color)))

(define (stroke-paint! context paint)
  ((foreign-lambda* void ((context ctx) (paint pnt)) "nvgStrokePaint(ctx, *pnt);") context (location paint)))

(define (fill-color! context color)
  ((foreign-lambda* void ((context ctx) (color clr)) "nvgFillColor(ctx, *clr);") context (location color)))

(define (fill-paint! context paint)
  ((foreign-lambda* void ((context ctx) (paint pnt)) "nvgFillPaint(ctx, *pnt);") context (location paint)))

(define (miter-limit! context limit)
  ((foreign-lambda void "nvgMiterLimit" context float) context limit))

(define (stroke-width! context width)
  ((foreign-lambda void "nvgStrokeWidth" context float) context width))

(define (line-cap! context line-cap-style)
  ((foreign-lambda void "nvgLineCap" context integer) context line-cap-style))

(define (line-join! context line-join-style)
  ((foreign-lambda void "nvgLineJoin" context integer) context line-join-style))

(define (global-alpha! context alpha)
  ((foreign-lambda void "nvgGlobalAlpha" context float) context alpha))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-type transform f32vector)

(define (reset-transform! context)
  ((foreign-lambda void "nvgResetTransform" context) context))

(define (transform! context transform)
  ((foreign-lambda* void ((context ctx) (transform t)) "nvgTransform(ctx, t[0], t[1], t[2], t[3], t[4], t[5]);") context transform))

(define (translate! context x y)
  ((foreign-lambda void "nvgTranslate" context float float) context x y))

(define (rotate! context radians)
  ((foreign-lambda void "nvgRotate" context float) context radians))

(define (skew-x! context radians)
  ((foreign-lambda void "nvgSkewX" context float) context radians))

(define (skew-y! context radians)
  ((foreign-lambda void "nvgSkewY" context float) context radians))

(define (scale! context x y)
  ((foreign-lambda void "nvgScale" context float float) context x y))

(define (make-transform)
  (make-f32vector 6))

(define (current-transform context)
  (let ((buf (make-transform)))
    ((foreign-lambda void "nvgCurrentTransform" context transform) context buf)
    buf))

(define (transform-identity! transform)
  ((foreign-lambda void "nvgTransformIdentity" transform) transform))

(define (transform-translate! transform x y)
  ((foreign-lambda void "nvgTransformTranslate" transform float float) transform x y))

(define (transform-scale! transform x y)
  ((foreign-lambda void "nvgTransformScale" transform float float) transform x y))

(define (transform-rotate! transform radians)
  ((foreign-lambda void "nvgTransformRotate" transform float) transform radians))

(define (transform-skew-x! transform radians)
  ((foreign-lambda void "nvgTransformSkewX" transform float) transform radians))

(define (transform-skew-y! transform radians)
  ((foreign-lambda void "nvgTransformSkewY" transform float) transform radians))

(define (transform-multiply! t1 t2)
  ((foreign-lambda void "nvgTransformMultiply" transform (const transform)) t1 t2)
  t1)

(define (transform-premultiply! t1 t2)
  ((foreign-lambda void "nvgTransformPremultiply" transform (const transform)) t1 t2)
  t1)

(define (transform-point transform x y)
  (let-location ((dx float)
		 (dy float))
    ((foreign-lambda void "nvgTransformPoint" (c-pointer float) (c-pointer float) (const transform) float float) (location dx) (location dy) transform x y)
    (values dx dy)))

(define (degrees-to-radians degrees)
  ((foreign-lambda float "nvgDegToRad" float) degrees))

(define (radians-to-degrees radians)
  ((foreign-lambda float "nvgRadToDeg" float) radians))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-image/file! context file-name flags)
  (let ((id ((foreign-lambda integer "nvgCreateImage" context (const c-string) integer) context file-name flags)))
    (if (= id -1) #f id)))

(define (create-image/blob! context blob flags)
  (let ((id ((foreign-lambda integer "nvgCreateImageMem" context integer blob integer) context flags blob (blob-size blob))))
    (if (= id -1) #f id)))

(define (create-image/rgba! context width height flags data)
  (let ((id ((foreign-lambda integer "nvgCreateImageRGBA" context integer integer integer (const nonnull-u8vector)) context width height flags data)))
    (if (= id -1) #f id)))

(define (update-image! context id data)
  ((foreign-lambda void "nvgUpdateImage" context integer (const nonnull-u8vector)) context id data))

(define (image-size context id)
  (let-location ((sx integer)
		 (sy integer))
    ((foreign-lambda void "nvgImageSize" context integer (c-pointer integer) (c-pointer integer)) context id (location sx) (location sy))
    (values sx sy)))

(define (delete-image! context id)
  ((foreign-lambda void "nvgDeleteImage" context integer) context id))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-linear-gradient context sx sy ex ey icolor ocolor)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float sx) (float sy) (float ex) (float ey) (color icol) (color ocol)) "*p = nvgLinearGradient(ctx, sx, sy, ex, ey, *icol, *ocol);") (location out) context sx sy ex ey (location icolor) (location ocolor))
    out))

(define (make-box-gradient context x y width height radius feather icolor ocolor)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float x) (float y) (float w) (float h) (float r) (float f) (color icol) (color ocol)) "*p = nvgBoxGradient(ctx, x, y, w, h, r, f, *icol, *ocol);") (location out) context x y width height radius feather (location icolor) (location ocolor))
    out))

(define (make-radial-gradient context cx cy inr outr icolor ocolor)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float cx) (float cy) (float inr) (float outr) (color icol) (color ocol)) "*p = nvgRadialGradient(ctx, cx, cy, inr, outr, *icol, *ocol);") (location out) context cx cy inr outr (location icolor) (location ocolor))
    out))

(define (make-image-pattern context ox oy ex ey radians image alpha)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float ox) (float oy) (float ex) (float ey) (float radians) (integer image) (float alpha)) "*p = nvgImagePattern(ctx, ox, oy, ex, ey, radians, image, alpha);") (location out) context ox oy ex ey radians image alpha)
    out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scissoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (scissor! context x y width height)
  ((foreign-lambda void "nvgScissor" context float float float float) context x y width height))

(define (intersect-scissor! context x y width height)
  ((foreign-lambda void "nvgIntersectScissor" context float float float float) context x y width height))

(define (reset-scissor! context)
  ((foreign-lambda void "nvgResetScissor" context) context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (begin-path! context)
  ((foreign-lambda void "nvgBeginPath" context) context))

(define (close-path! context)
  ((foreign-lambda void "nvgClosePath" context) context))

(define (move-to! context x y)
  ((foreign-lambda void "nvgMoveTo" context float float) context x y))

(define (line-to! context x y)
  ((foreign-lambda void "nvgLineTo" context float float) context x y))

(define (bezier-to! context control1-x control1-y control2-x control2-y x y)
  ((foreign-lambda void "nvgBezierTo" context float float float float float float) context control1-x control1-y control2-x control2-y x y))

(define (quad-to! context control-x control-y x y)
  ((foreign-lambda void "nvgQuadTo" context float float float float) context control-x control-y x y))

(define (arc-to! context x1 y1 x2 y2 radius)
  ((foreign-lambda void "nvgArcTo" context float float float float float) context x1 y1 x2 y2 radius))

(define (path-winding! context path-winding-style)
  ((foreign-lambda void "nvgPathWinding" context integer) context path-winding-style))

(define (arc! context center-x center-y radius radian0 radian1 sweep-direction)
  ((foreign-lambda void "nvgArc" context float float float float float integer) context center-x center-y radius radian0 radian1 sweep-direction))

(define (rectangle! context x y width height)
  ((foreign-lambda void "nvgRect" context float float float float) context x y width height))

(define (rounded-rectangle! context x y width height radius)
  ((foreign-lambda void "nvgRoundedRect" context float float float float float) context x y width height radius))

(define (ellipse! context center-x center-y radius-x radius-y)
  ((foreign-lambda void "nvgEllipse" context float float float float) context center-x center-y radius-x radius-y))

(define (circle! context center-x center-y radius)
  ((foreign-lambda void "nvgCircle" context float float float) context center-x center-y radius))

(define (fill! context)
  ((foreign-lambda void "nvgFill" context) context))

(define (stroke! context)
  ((foreign-lambda void "nvgStroke" context) context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (create-font! context name file-name)
  (let ((id ((foreign-lambda integer "nvgCreateFont" context (const c-string) (const c-string)) context name file-name)))
    (if (= id -1) #f id)))

(define (create-font/blob! context name data)
  (let ((id ((foreign-lambda integer "nvgCreateFontMem" context (const c-string) u8vector integer integer) context name data (blob-size data) 0)))
    (if (= id -1) #f id)))

(define (find-font context name)
  (let ((id ((foreign-lambda integer "nvgFindFont" context (const c-string)) context name)))
    (if (= id -1) #f id)))

(define (font-size! context size)
  ((foreign-lambda void "nvgFontSize" context float) context size))

(define (font-blur! context blur)
  ((foreign-lambda void "nvgFontBlur" context float) context blur))

(define (text-letter-spacing! context spacing)
  ((foreign-lambda void "nvgTextLetterSpacing" context float) context spacing))

(define (text-line-height! context height)
  ((foreign-lambda void "nvgTextLineHeight" context float) context height))

(define (text-align! context text-align-flags)
  ((foreign-lambda void "nvgTextAlign" context integer) context text-align-flags))

(define (font-face-id! context id)
  ((foreign-lambda void "nvgFontFaceId" context integer) context id))

(define (font-face! context name)
  ((foreign-lambda void "nvgFontFace" context (const c-string)) context name))

(define (text! context x y text #!optional (last-char-pointer #f))
  ((foreign-lambda void "nvgText" context float float (const c-string) (const c-pointer)) context x y text last-char-pointer))

(define (text-box! context x y break-row-width text #!optional (last-char-pointer #f))
  ((foreign-lambda void "nvgTextBox" context float float float (const c-string) (const c-pointer)) context x y break-row-width text last-char-pointer))

(define (text-bounds! context x y string #!optional (last-char-pointer #f))
  (let* ((buf (make-f32vector 4))
	 (advance
	  ((foreign-lambda void "nvgTextBounds" context float float (const c-string) (const c-pointer) f32vector)
	   context x y string last-char-pointer buf)))
    (values advance buf)))

(define (text-box-bounds context x y break-row-width string #!optional (last-char-pointer #f))
  (let* ((buf (make-f32vector 4))
	 (advance
	  ((foreign-lambda void "nvgTextBoxBounds" context float float float (const c-string) (const c-pointer) f32vector)
	   context x y break-row-width string last-char-pointer buf)))
    (values advance buf)))

(define maximum-glyph-positions (make-parameter 100))
(define-foreign-type glyph-position (c-pointer (struct "NVGglyphPosition")))

(define (text-glyph-positions context x y string)
  (let* ((buf (make-blob (* (foreign-type-size "NVGglyphPosition") (maximum-glyph-positions))))
	 (buf-loc (location buf))
	 (count
	  ((foreign-lambda* integer ((context ctx) (float x) (float y) ((const c-string) str) (c-pointer buf) (integer max)) "
// str is a temporary string copied by Chicken; using locatives results in weird strings with extra bytes
int count = nvgTextGlyphPositions(ctx, x, y, str, NULL, buf, max);
NVGglyphPosition *positions = (NVGglyphPosition *)buf;

// Make relative locations
int i;
for (i = 0; i < count; ++i) {
  positions[i].str = (char *)(positions[i].str - str);
}
C_return(count);")
	   context x y string buf-loc (maximum-glyph-positions)))
	 (gp-index
	  (foreign-lambda* size_t ((glyph-position gp) (integer offset)) "C_return((size_t)gp[offset].str);"))
	 (gp-x
	  (foreign-lambda* float ((glyph-position gp) (integer offset)) "C_return(gp[offset].x);"))
	 (gp-minx
	  (foreign-lambda* float ((glyph-position gp) (integer offset)) "C_return(gp[offset].minx);"))
	 (gp-maxx
	  (foreign-lambda* float ((glyph-position gp) (integer offset)) "C_return(gp[offset].maxx);")))
    (map
     (lambda (idx)	     
       (make-glyph-position
	(gp-index buf-loc idx)
	(gp-x buf-loc idx)
	(gp-minx buf-loc idx)
	(gp-maxx buf-loc idx)))
     (iota count))))

(define (text-metrics context)
  (let-location ((ascender float)
		 (descender float)
		 (lineh float))
    ((foreign-lambda void "nvgTextMetrics" context (c-pointer float) (c-pointer float) (c-pointer float))
     context (location ascender) (location descender) (location lineh))
    (values ascender descender lineh)))

(define maximum-text-rows (make-parameter 100))
(define-foreign-type text-row (c-pointer (struct "NVGtextRow")))

(define (text-break-lines context string row-width)
  (let* ((buf (make-blob (* (foreign-type-size "NVGtextRow") (maximum-text-rows))))
	 (buf-loc (location buf))
	 (count
	  ((foreign-lambda* integer ((context ctx) ((const c-string) str) (float width) (c-pointer buf) (integer max)) "
// str is a temporary string copied by Chicken; using locatives results in weird strings with extra bytes
int count = nvgTextBreakLines(ctx, str, NULL, width, buf, max);
NVGtextRow *rows = (NVGtextRow *)buf;

// Make relative locations
int i;
for (i = 0; i < count; ++i) {
  rows[i].start = (char *)(rows[i].start - str);
  rows[i].end = (char *)(rows[i].end - str);
  rows[i].next = (char *)(rows[i].next - str);
}
C_return(count);")
	   context string row-width buf-loc (maximum-text-rows)))
	 (tr-start-index
	  (foreign-lambda* size_t ((text-row tr) (integer offset)) "C_return((size_t)tr[offset].start);"))
	 (tr-end-index
	  (foreign-lambda* size_t ((text-row tr) (integer offset)) "C_return((size_t)tr[offset].end);"))
	 (tr-next-index
	  (foreign-lambda* size_t ((text-row tr) (integer offset)) "C_return((size_t)tr[offset].next);"))
	 (tr-width
	  (foreign-lambda* float ((text-row tr) (integer offset)) "C_return(tr[offset].width);"))
	 (tr-minx
	  (foreign-lambda* float ((text-row tr) (integer offset)) "C_return(tr[offset].minx);"))
	 (tr-maxx
	  (foreign-lambda* float ((text-row tr) (integer offset)) "C_return(tr[offset].maxx);")))
    (map
     (lambda (idx)
       (make-text-row
	(tr-start-index buf-loc idx)
	(tr-end-index buf-loc idx)
	(tr-next-index buf-loc idx)
	(tr-width buf-loc idx)
	(tr-minx buf-loc idx)
	(tr-maxx buf-loc idx)))
     (iota count))))
