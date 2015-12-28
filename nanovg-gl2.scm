(module nanovg-gl2 *
  (import chicken scheme foreign)

  (foreign-declare "#define NANOVG_GL2_IMPLEMENTATION")
  (define nanovg-gl-version 'gl2)

  (include "nanovg.scm"))
