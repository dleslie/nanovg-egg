(module nanovg-gl3 *
  (import chicken scheme foreign)

  (foreign-declare "#define NANOVG_GL3_IMPLEMENTATION")
  (define nanovg-gl-version 'gl3)
  
  (include "nanovg.scm"))
