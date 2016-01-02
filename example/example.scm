(use (only allegro
	   init new-display-flags-set! display-flag->int make-display new-bitmap-flags-set! bitmap-flag->int make-bitmap opengl-texture make-keyboard-state target-bitmap-set! target-backbuffer-set! bitmap-draw flip-display keyboard-state-init! keyboard-state-key-down? current-time make-mouse-state mouse-state-init! mouse-state-x mouse-state-y)
     (prefix allegro al:)

     (only opengl-glew
	   init clear +color-buffer-bit+ +depth-buffer-bit+ viewport)
     (prefix opengl-glew gl:)
     srfi-18)

(define (abort x)
  (display x)
  (newline)
  (exit))

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

(al:new-bitmap-flags-set! (al:bitmap-flag->int 'video-bitmap))
(define buffer (al:make-bitmap (al:display-width main-display)
			       (al:display-height main-display)))
(if (or (not buffer)
	(not (al:opengl-texture buffer)))
    (abort "Could not create render buffer"))

(define kb-state (al:make-keyboard-state))
(define mouse-state (al:make-mouse-state))

(define render-thunks (make-parameter '()))

(define last-render-time (al:current-time))

(define-record frame-data
  delta
  mouse-x
  mouse-y
  display-width
  display-height)

(include "demo")

(define (render data)
  (al:target-bitmap-set! buffer)
  
  (gl:viewport 0 0 (frame-data-display-width data) (frame-data-display-height data))
  (gl:clear-color 0 0 0 0)
  (gl:clear (bitwise-ior gl:+color-buffer-bit+ gl:+depth-buffer-bit+))

  (map (lambda (t)  (apply (eval t) (list data))) (render-thunks))
    
  (al:target-backbuffer-set! main-display)

  (al:bitmap-draw buffer 0 0 0)
  (al:flip-display))

(define (main-loop)
  (al:keyboard-state-init! kb-state)
  (al:mouse-state-init! mouse-state)
  (let* ((current-time (al:current-time)))
    (set! last-render-time (al:current-time))

    (define data
      (make-frame-data
       (- current-time last-render-time)
       (al:mouse-state-x mouse-state)
       (al:mouse-state-y mouse-state)
       (al:display-width main-display)
       (al:display-height main-display)))

    (when (not (al:keyboard-state-key-down? kb-state 'escape))
      (render data)
      (thread-sleep! (seconds->time (- (/ 1 30) (- (al:current-time) last-render-time))))
      (main-loop))))

(define main-thread (make-thread main-loop "Main Thread"))
(thread-start! main-thread)

;; This trick allows interactive calling of the example
(cond-expand
  (csi (begin
	 (use parley)
	 (let ((old (current-input-port)))
	   (current-input-port (make-parley-port old)))))
  (else (thread-join! main-thread)))
