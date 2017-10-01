(module nanovg-gles3 *
  (import chicken scheme foreign)

  (foreign-declare "#define NANOVG_GLES3_IMPLEMENTATION")

  (include "nanovg.scm"))
