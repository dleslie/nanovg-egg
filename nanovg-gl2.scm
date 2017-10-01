(module nanovg-gl2 *
  (import chicken scheme foreign)

  (foreign-declare "#define NANOVG_GL2_IMPLEMENTATION")
  
  (include "nanovg.scm"))
