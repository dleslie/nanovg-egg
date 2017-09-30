(use (only allegro
	   init new-display-flags-set! display-flag->int make-display make-keyboard-state target-backbuffer-set! flip-display keyboard-state-init! keyboard-state-key-down? current-time make-mouse-state mouse-state-init! mouse-state-x mouse-state-y rest)
     (prefix allegro al:)

     (only opengl-glew
	   init clear +color-buffer-bit+ +depth-buffer-bit+ +stencil-buffer-bit+ viewport)
     (prefix opengl-glew gl:))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utils
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (display-exception exn)
  (define (display-call-entry call)
    (let ((type (vector-ref call 0))
	  (line (vector-ref call 1)))
      (cond
       ((equal? type "<syntax>")
	(display (string-append type " ")) (write line) (newline))
       ((equal? type "<eval>")
	(display (string-append type "   ")) (write line) (newline)))))

  (with-exception-handler (lambda a a)
    (lambda ()
      (display (format "Error: (~s) ~s: ~s"
		       ((condition-property-accessor 'exn 'location) exn)
		       ((condition-property-accessor 'exn 'message) exn)
		       ((condition-property-accessor 'exn 'arguments) exn)))
      (newline)
      (display "Call history: ") (newline)
      (map display-call-entry ((condition-property-accessor 'exn 'call-chain) exn))
      (newline))))

(define (safe-apply symbol args #!key (overwrite-function '(lambda a a)) (print-exception #t))
  (call/cc
   (lambda (return)
     (with-exception-handler
	 (lambda (x)
	   (when print-exception
	     (display-exception x))
	   (when overwrite-function
	     (eval `(set! ,symbol ,overwrite-function)))
	   (return x))
       (lambda ()
	 (apply (eval symbol) args))))))

(define (abort x)
  (display x)
  (newline)
  (exit))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init Window and Input
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if (not (al:init))
    (abort "Could not init Allegro"))

(al:init 'keyboard)
(al:init 'image)
(al:init 'mouse)

(al:new-display-flags-set! (al:display-flag->int 'opengl))
(define main-display (al:make-display 1000 600))
(if (not main-display)
    (abort "Error creating display"))

(gl:init)

(define kb-state (al:make-keyboard-state))
(define mouse-state (al:make-mouse-state))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Render Step
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define render-thunks (make-parameter '()))

(define-record frame-data
  delta
  time
  mouse-x
  mouse-y
  display-width
  display-height)

(define (render data)
  (al:target-backbuffer-set! main-display)
  
  (gl:viewport 0 0 (frame-data-display-width data) (frame-data-display-height data))
  (gl:clear-color 0 0 0 0)
  (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+ gl:+stencil-buffer-bit+))

  (map
   (lambda (t) (safe-apply t (list data)))
   (render-thunks))

  (al:flip-display))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Loop
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (main-loop)
  (let loop ((last-render-time (al:current-time)))
    (al:keyboard-state-init! kb-state)
    (al:mouse-state-init! mouse-state)
    (let* ((current-time (al:current-time))
	   (data
	    (make-frame-data
	     (- current-time last-render-time)
	     current-time
	     (al:mouse-state-x mouse-state)
	     (al:mouse-state-y mouse-state)
	     (al:display-width main-display)
	     (al:display-height main-display)))
	   (last-render-time (al:current-time)))
      (when (not (al:keyboard-state-key-down? kb-state 'escape))
	(render data)
	(al:rest (min 0 (- (/ 1 60) (- (al:current-time) last-render-time))))
	(loop last-render-time)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Load Demo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(include "demo.scm")

;; Start
(main-loop)

