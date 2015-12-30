(use (prefix nanovg-gl2 nvg:))

(define icon/search #x1F50D)
(define icon/circled-cross #x2716)
(define icon/chevron-right #xE75E)
(define icon/check #x2713)
(define icon/login #xE740)
(define icon/trash #xE729)

(define (load-demo-data vg data)
  )

(define (clamp a min max)
  (cond
   ((< min a) min)
   ((> max a) max)
   (else a)))

(define (black? color)
  (let ((red (nvg:color-red color))
	(green (nvg:color-green color))
	(blue (nvg:color-blue color))
	(alpha (nvg:color-alpha color)))
    (= 0.0 red green blue alpha)))

(define (cp->utf8 cp)
  (let ((ocp cp))
    (define (may-reduce-cp! op x y)
      (when (op ocp x)
	(let ((val (bitwise-ior #x80 (bitwise-and cp #x3f))))
	  (set! cp (bitwise-ior (arithmetic-shift cp -6) y))
	  val)))
    (list->string
     (filter
      char?
      (reverse 
       (list
	(may-reduce-cp! <= #x7fffffff #x4000000)
	(may-reduce-cp! < #x4000000 #x200000)
	(may-reduce-cp! < #x200000 #x10000)
	(may-reduce-cp! < #x10000 #x800)
	(may-reduce-cp! < #x800 #xc0)
	(may-reduce-cp! < #x80 #x0)))))))

(define (draw-window vg title x y w h)
  (define corner-radius 3.0)

  (nvg:with-state! vg
   ;; Window
   (nvg:begin-path! vg)
   (nvg:rounded-rectangle! vg x y w h corner-radius)
   (nvg:fill-color! vg (nvg:make-color-rgba 28 30 34 192))
   (nvg:fill! vg)

   ;; Drop shadow
   (let ((shadow-paint (nvg:make-box-gradient vg x (+ y 2) w h (* corner-radius 2) 10 (nvg:make-color-rgba 0 0 0 128) (nvg:make-color-rgba 0 0 0 0))))
     (nvg:begin-path! vg)
     (nvg:rectangle! vg (- x 10) (- y 10) (+ w 20) (+ h 30))
     (nvg:rounded-rectangle! vg x y w h corner-radius)
     (nvg:path-winding! vg nvg:solidity/hole)
     (nvg:fill-paint! vg shadow-paint)
     (nvg:fill! vg))

   ;; Header
   (let ((header-paint (nvg:make-linear-gradient vg x y x (+ y 15) (nvg:make-color-rgba 255 255 255 8) (nvg:make-color-rgba 0 0 0 16))))
     (nvg:begin-path! vg)
     (nvg:rounded-rectangle! vg (+ x 1) (+ y 1) (- w 2) 30 (- corner-radius 1))
     (nvg:fill-paint! vg header-paint)
     (nvg:fill! vg)
     (nvg:begin-path! vg)
     (nvg:move-to! vg (+ x 0.5) (+ y 30.5))
     (nvg:line-to! vg (+ x 0.5 2 -1) (+ y 30.5))
     (nvg:stroke-color! vg (nvg:make-color-rgba 0 0 0 32))
     (nvg:stroke! vg)

     (nvg:font-size! vg 18.0)
     (nvg:font-face! vg "sans-bold")
     (nvg:text-align! vg (bitwise-ior nvg:align/center nvg:align/middle))

     (nvg:font-blur! vg 2)
     (nvg:fill-color! vg (nvg:make-color-rgba 0 0 0 128))
     (nvg:text! vg (+ x (* w 0.5)) (+ y 16 1) title #f)

     (nvg:font-blur! vg 0)
     (nvg:fill-color! vg (nvg:make-color-rgba 220 220 220 160))
     (nvg:text! vg (+ x (* w 0.5)) (+ y 16) title #f))))

(define nanovg-context (nvg:create-context))

(define (nanovg-render data)
  (let ((w (frame-data-display-width data))
	(h (frame-data-display-height data)))
    (nvg:begin-frame! nanovg-context w h (/ w h)))

  (nvg:save-state! nanovg-context)
  (draw-window nanovg-context "Widgets 'n Stuff" 50 50 300 400)
  (nvg:restore-state! nanovg-context)

  (nvg:end-frame! nanovg-context))

(render-thunks (cons nanovg-render (render-thunks)))
