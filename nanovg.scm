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

(define create-context
  (cond-expand
    (nanovg-gl2 (foreign-lambda* context ((int flags)) "C_return(nvgCreateGL2(flags));"))
    (nanovg-gl3 (foreign-lambda* context ((int flags)) "C_return(nvgCreateGL3(flags));"))
    (nanovg-gles2 (foreign-lambda* context ((int flags)) "C_return(nvgCreateGLES2(flags));"))
    (nanovg-gles3 (foreign-lambda* context ((int flags)) "C_return(nvgCreateGLES3(flags));"))))

(define delete-context
  (cond-expand
    (nanovg-gl2 (foreign-lambda* void ((context ctx)) "nvgDeleteGL2(ctx);"))
    (nanovg-gl3 (foreign-lambda* void ((context ctx)) "nvgDeleteGL3(ctx);"))
    (nanovg-gles2 (foreign-lambda* void ((context ctx)) "nvgDeleteGLES2(ctx);"))
    (nanovg-gles3 (foreign-lambda* void ((context ctx)) "nvgDeleteGLES3(ctx);"))))
