;; -*- geiser-scheme-implementation: 'chicken -*-

(import foreign srfi-4)

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
    ((foreign-lambda* void ((color clr) (unsigned-char r) (unsigned-char g) (unsigned-char b)) "*clr = nvgRGB(r, g, b);") color r g b)
    color))

(define (make-color-rgbf r g b)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float r) (float g) (float b)) "*clr = nvgRGBf(r, g, b);") color r g b)
    color))

(define (make-color-rgba r g b a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (unsigned-char r) (unsigned-char g) (unsigned-char b) (unsigned-char a)) "*clr = nvgRGBA(r, g, b, a);") color r g b a)
    color))

(define (make-color-rgbaf r g b a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float r) (float g) (float b) (float a)) "*clr = nvgRGBAf(r, g, b, a);") color r g b a)
    color))

(define (make-color-lerp clr1 clr2 u)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (color clr1) (color clr2) (float u)) "*clr = nvgLerpRGBA(*clr1, *clr2, u);") color clr1 clr2 u)
    color))

(define (make-color-transparency clr a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (color clr1) (unsigned-char a)) "*clr = nvgTransRGBA(*clr1, a);") color clr a)
    color))

(define (make-color-transparencyf clr a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (color clr1) (float a)) "*clr = nvgTransRGBAf(*clr1, a);") color clr a)
    color))

(define (make-color-hsl h s l)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float h) (float s) (float l)) "*clr = nvgHSL(h, s, l);") color h s l)
    color))

(define (make-color-hsla h s l a)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((color clr) (float h) (float s) (float l) (float a)) "*clr = nvgHSLA(h, s, l, a);") color h s l a)
    color))

(define color-red
  (foreign-lambda* float ((color clr)) "C_return(clr->r);"))

(define color-green
  (foreign-lambda* float ((color clr)) "C_return(clr->g);"))

(define color-blue
  (foreign-lambda* float ((color clr)) "C_return(clr->b);"))

(define color-alpha
  (foreign-lambda* float ((color clr)) "C_return(clr->a);"))

(define (color-rgba color)
  (let ((buf (make-f32vector 4)))
    ((foreign-lambda* void ((color clr) (f32vector buf)) "memcpy(buf, clr->rgba, sizeof(float) * 4);") color buf)
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
    ((foreign-lambda* void ((paint p) (f32vector buf)) "memcpy(buf, p->xform, sizeof(float) * 6);") paint buf)
    buf))

(define (paint-extent paint)
  (let ((buf (make-f32vector 2)))
    ((foreign-lambda* void ((paint p) (f32vector buf)) "memcpy(buf, p->extent, sizeof(float) *2);") paint buf)
    buf))

(define paint-radius
  (foreign-lambda* float ((paint p)) "C_return(p->radius);"))

(define paint-feather
  (foreign-lambda* float ((paint p)) "C_return(p->feather);"))

(define (paint-inner-color paint)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((paint p) (color c)) "memcpy(c, &p->innerColor, sizeof(NVGcolor));") paint color)
    color))

(define (paint-outer-color paint)
  (let ((color (make-color-uninitialized)))
    ((foreign-lambda* void ((paint p) (color c)) "memcpy(c, &p->outerColor, sizeof(NVGcolor));") paint color)
    color))

(define paint-image
  (foreign-lambda* integer ((paint p)) "C_return(p->image);"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Glyph Position
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record glyph-position
  offset x minx maxx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text Row
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-record text-row
  start-offset end-offset next-offset width minx maxx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame Control
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define begin-frame!
  (foreign-lambda void "nvgBeginFrame" context integer integer float))

(define cancel-frame!
  (foreign-lambda void "nvgCancelFrame" context))

(define end-frame!
  (foreign-lambda void "nvgEndFrame" context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; State
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define save-state!
  (foreign-lambda void "nvgSave" context))

(define restore-state!
  (foreign-lambda void "nvgRestore" context))

(define reset-state!
  (foreign-lambda void "nvgReset" context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render Styles
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define stroke-color!
  (foreign-lambda* void ((context ctx) (color clr)) "nvgStrokeColor(ctx, *clr);"))

(define stroke-paint!
  (foreign-lambda* void ((context ctx) (paint pnt)) "nvgStrokePaint(ctx, *pnt);"))

(define fill-color!
  (foreign-lambda* void ((context ctx) (color clr)) "nvgFillColor(ctx, *clr);"))

(define fill-paint!
  (foreign-lambda* void ((context ctx) (paint pnt)) "nvgFillPaint(ctx, *pnt);"))

(define miter-limit!
  (foreign-lambda void "nvgMiterLimit" context float))

(define stroke-width!
  (foreign-lambda void "nvgStrokeWidth" context float))

(define line-cap!
  (foreign-lambda void "nvgLineCap" context integer))

(define line-join!
  (foreign-lambda void "nvgLineJoin" context integer))

(define global-alpha!
  (foreign-lambda void "nvgGlobalAlpha" context float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Transforms
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-foreign-type transform f32vector)

(define reset-transform!
  (foreign-lambda void "nvgResetTransform" context))

(define transform!
  (foreign-lambda* void ((context ctx) (transform t)) "nvgTransform(ctx, t[0], t[1], t[2], t[3], t[4], t[5]);"))

(define translate!
  (foreign-lambda void "nvgTranslate" context float float))

(define rotate!
  (foreign-lambda void "nvgRotate" context float))

(define skew-x!
  (foreign-lambda void "nvgSkewX" context float))

(define skew-y!
  (foreign-lambda void "nvgSkewY" context float))

(define scale!
  (foreign-lambda void "nvgScale" context float float))

(define (make-transform)
  (make-f32vector 6))

(define (current-transform context)
  (let ((buf (make-transform)))
    ((foreign-lambda void "nvgCurrentTransform" context transform) context buf)
    buf))

(define transform-identity!
  (foreign-lambda void "nvgTransformIdentity" transform))

(define transform-translate!
  (foreign-lambda void "nvgTransformTranslate" transform float float))

(define transform-scale!
  (foreign-lambda void "nvgTransformScale" transform float float))

(define transform-rotate!
  (foreign-lambda void "nvgTransformRotate" transform float))

(define transform-skew-x!
  (foreign-lambda void "nvgTransformSkewX" transform float))

(define transform-skew-y!
  (foreign-lambda void "nvgTransformSkewY" transform float))

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

(define degrees-to-radians
  (foreign-lambda float "nvgDegToRad" float))

(define radians-to-degrees
  (foreign-lambda float "nvgRadToDeg" float))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Images
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define create-image/file!
  (foreign-lambda integer "nvgCreateImage" context (const c-string) integer))

(define (create-image/blob! context blob flags)
  ((foreign-lambda integer "nvgCreateImageMem" context integer blob integer) context flags blob (blob-size blob)))

(define create-image/rgba!
  (foreign-lambda integer "nvgCreateImageRGBA" context integer integer integer (const nonnull-u8vector)))

(define update-image!
  (foreign-lambda void "nvgUpdateImage" context integer (const nonnull-u8vector)))

(define (image-size context image)
  (let-location ((sx integer)
		 (sy integer))
    ((foreign-lambda void "nvgImageSize" context integer (c-pointer integer) (c-pointer integer)) context image (location sx) (location sy))
    (values sx sy)))

(define delete-image!
  (foreign-lambda void "nvgDeleteImage" context integer))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paints
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (linear-gradient context sx sy ex ey icolor ocolor)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float sx) (float sy) (float ex) (float ey) (color icol) (color ocol)) "*p = nvgLinearGradient(ctx, sx, sy, ex, ey, *icol, *ocol);") out context sx sy ex ey icolor ocolor)
    out))

(define (box-gradient context x y width height radius feather icolor ocolor)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float x) (float y) (float w) (float h) (float r) (float f) (color icol) (color ocol)) "*p = nvgBoxGradient(ctx, x, y, w, h, r, f, *icol, *ocol);") out context x y width height radius feather icolor ocolor)
    out))

(define (radial-gradient context cx cy inr outr icolor ocolor)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float cx) (float cy) (float inr) (float outr) (color icol) (color ocol)) "*p = nvgRadialGradient(ctx, cx, cy, inr, outr, *icol, *ocol);") out context cx cy inr outr icolor ocolor)
    out))

(define (image-pattern context ox oy ex ey angle image alpha)
  (let ((out (make-paint-uninitialized)))
    ((foreign-lambda* void ((paint p) (context ctx) (float ox) (float oy) (float ex) (float ey) (float angle) (integer image) (float alpha)) "*p = nvgImagePattern(ctx, ox, oy, ex, ey, angle, image, alpha);") out context ox oy ex ey angle image alpha)
    out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Scissoring
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define scissor!
  (foreign-lambda void "nvgScissor" context float float float float))

(define intersect-scissor!
  (foreign-lambda void "nvgIntersectScissor" context float float float float))

(define reset-scissor!
  (foreign-lambda void "nvgResetScissor" context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Paths
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define begin-path!
  (foreign-lambda void "nvgBeginPath" context))

(define close-path!
  (foreign-lambda void "nvgClosePath" context))

(define-syntax with-path!
  (syntax-rules ()
    ((with-path ctx . rest)
     (begin
       (begin-path! ctx)
       (begin . rest)
       (close-path! ctx)))))

(define move-to!
  (foreign-lambda void "nvgMoveTo" context float float))

(define line-to!
  (foreign-lambda void "nvgLineTo" context float float))

(define bezier-to!
  (foreign-lambda void "nvgBezierTo" context float float float float float float))

(define quad-to!
  (foreign-lambda void "nvgQuadTo" context float float float float))

(define arc-to!
  (foreign-lambda void "nvgArcTo" context float float float float float))

(define path-winding!
  (foreign-lambda void "nvgPathWinding" context integer))

(define arc!
  (foreign-lambda void "nvgArc" context float float float float float integer))

(define rectangle!
  (foreign-lambda void "nvgRect" context float float float float))

(define rounded-rectangle!
  (foreign-lambda void "nvgRoundedRect" context float float float float float))

(define ellipse!
  (foreign-lambda void "nvgEllipse" context float float float float))

(define circle!
  (foreign-lambda void "nvgCircle" context float float float))

(define fill!
  (foreign-lambda void "nvgFill" context))

(define stroke!
  (foreign-lambda void "nvgStroke" context))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Text
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define create-font!
  (foreign-lambda integer "nvgCreateFont" context (const c-string) (const c-string)))

(define (create-font/blob! context name data)
  ((foreign-lambda integer "nvgCreateFontMem" context (const c-string) u8vector integer integer) context name data (blob-size data) 0))

(define find-font
  (foreign-lambda integer "nvgFindFont" context (const c-string)))

(define font-size!
  (foreign-lambda void "nvgFontSize" context float))

(define font-blur!
  (foreign-lambda void "nvgFontBlur" context float))

(define text-letter-spacing!
  (foreign-lambda void "nvgTextLetterSpacing" context float))

(define text-line-height!
  (foreign-lambda void "nvgTextLineHeight" context float))

(define text-align!
  (foreign-lambda void "nvgTextAlign" context integer))

(define font-face-id!
  (foreign-lambda void "nvgFontFaceId" context integer))

(define font-face!
  (foreign-lambda void "nvgFontFace" context (const c-string)))

(define text!
  (foreign-lambda void "nvgText" context float float (const c-string) (const c-pointer)))

(define text-box!
  (foreign-lambda void "nvgTextBox" context float float float (const c-string) (const c-pointer)))

(define (text-bounds context x y string end)
  (let* ((buf (make-f32vector 4))
	 (advance ((foreign-lambda void "nvgTextBounds" context float float (const c-string) (const c-pointer) f32vector) context x y string end buf)))
    (values advance buf)))

(define (text-box-bounds context x y break-row-width string end)
  (let* ((buf (make-f32vector 4))
	 (advance ((foreign-lambda void "nvgTextBoxBounds" context float float float (const c-string) (const c-pointer) f32vector) context x y break-row-width string end buf)))
    (values advance buf)))

(define maximum-glyph-positions (make-parameter 100))

(define (text-glyph-positions context x y string)
  (define-foreign-type glyph-position (c-pointer (struct "NVGglyphPosition")))

  (define get-index
    (foreign-lambda* size_t ((glyph-position gp) (integer offset) ((const c-string) original)) "C_return(gp[offset].str - original);"))

  (define get-x
    (foreign-lambda* float ((glyph-position gp) (integer offset)) "C_return(gp[offset].x);"))

  (define get-minx
    (foreign-lambda* float ((glyph-position gp) (integer offset)) "C_return(gp[offset].minx);"))

  (define get-maxx
    (foreign-lambda* float ((glyph-position gp) (integer offset)) "C_return(gp[offset].maxx);"))

  (let* ((buf (make-blob (* (foreign-type-size "NVGglyphPosition") (maximum-glyph-positions))))
	 (count ((foreign-lambda integer "nvgTextGlyphPositions" context float float (const c-string) (const c-string) glyph-position integer) context x y string #f buf (maximum-glyph-positions))))
    (do ((idx count (sub1 idx))
	 (positions '()))
	((<= idx 0) positions)
      (set! positions
	(cons
	 (let ((idx (sub1 idx)))
	   (make-glyph-position
	    (get-index buf idx string)
	    (get-x buf idx)
	    (get-minx buf idx)
	    (get-maxx buf idx)))
	 positions)))))

(define maximum-text-rows (make-parameter 100))

(define (text-metrics context)
  (let-location ((ascender float)
		 (descender float)
		 (lineh float))
    ((foreign-lambda void "nvgTextMetrics" context (c-pointer float) (c-pointer float) (c-pointer float)) context (location ascender) (location descender) (location lineh))
    (values ascender descender lineh)))

(define (text-break-lines context string row-width)
  (define-foreign-type text-row (c-pointer (struct "NVGtextRow")))

  (define get-start-index
    (foreign-lambda* size_t ((text-row tr) (integer offset) ((const c-string) original)) "C_return(tr[offset].start - original);"))

  (define get-end-index
    (foreign-lambda* size_t ((text-row tr) (integer offset) ((const c-string) original)) "C_return(tr[offset].end - original);"))

  (define get-next-index
    (foreign-lambda* size_t ((text-row tr) (integer offset) ((const c-string) original)) "C_return(tr[offset].next - original);"))

  (define get-width
    (foreign-lambda* float ((text-row tr) (integer offset)) "C_return(tr[offset].width);"))

  (define get-minx
    (foreign-lambda* float ((text-row tr) (integer offset)) "C_return(tr[offset].minx);"))

  (define get-maxx
    (foreign-lambda* float ((text-row tr) (integer offset)) "C_return(tr[offset].maxx);"))

  (let* ((buf (make-blob (* (foreign-type-size "NVGtextRow") (maximum-text-rows))))
	 (count ((foreign-lambda integer "nvgTextBreakLines" context (const c-string) (const c-string) float text-row integer) context string #f row-width buf (maximum-text-rows))))
    (do ((idx count (sub1 idx))
	 (rows '()))
	((<= idx 0) rows)
      (set! rows
	(cons
	 (let ((idx (sub1 idx)))
	   (make-text-row
	    (get-start-index buf idx string)
	    (get-end-index buf idx string)
	    (get-next-index buf idx string)
	    (get-width buf idx)
	    (get-minx buf idx)
	    (get-maxx buf idx)))
	 rows)))))
