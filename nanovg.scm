;; -*- geiser-scheme-implementation: 'chicken -*-

#>
#include <GL/gl.h>
#include "nanovg/src/nanovg.h"
#include "nanovg/src/nanovg_gl.h"
<#

(import foreign foreigners)

(define-foreign-enum-type (create-flag int)
  (create-flag->int int->create-flag)
  ((anti-alias) NVG_ANTIALIAS)
  ((stencil-strokes) NVG_STENCIL_STROKES)
  ((debug) NVG_DEBUG))

(define-foreign-record-type (context "NVGcontext"))

(define create-context*
  (cond-expand
    (nanovg-gl2 (foreign-lambda context "nvgCreateGL2" int))
    (nanovg-gl3 (foreign-lambda context "nvgCreateGL3" int))
    (nanovg-gles2 (foreign-lambda context "nvgCreateGLES2" int))
    (nanovg-gles3 (foreign-lambda context "nvgCreateGLES3" int))))

(define delete-context!
  (cond-expand
    (nanovg-gl2 (foreign-lambda void "nvgDeleteGL2" context))
    (nanovg-gl3 (foreign-lambda void "nvgDeleteGL3" context))
    (nanovg-gles2 (foreign-lambda void "nvgDeleteGLES2" context))
    (nanovg-gles3 (foreign-lambda void "nvgDeleteGLES3" context))))
