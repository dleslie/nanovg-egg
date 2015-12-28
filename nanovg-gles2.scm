(module nanovg-gles2 *
  (import chicken scheme foreign)

  (foreign-declare "#define NANOVG_GLES2_IMPLEMENTATION")
  (define nanovg-gl-version 'gles2)

  (include "nanovg.scm"))
