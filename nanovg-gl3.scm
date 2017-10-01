(module nanovg-gl3 *
  (import chicken scheme foreign)

  (foreign-declare "#define NANOVG_GL3_IMPLEMENTATION")
  
  (include "nanovg.scm"))
