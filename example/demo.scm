(use (prefix nanovg-gl2 nvg:)
     srfi-1)

(define (integer-list->string lst)
  (list->string (map integer->char lst)))

(define icon/search (integer-list->string '(-16 -97 -108 -115)))
(define icon/circled-cross (integer-list->string '(-30 -100 -106)))
(define icon/chevron-right (integer-list->string '(-18 -99 -98)))
(define icon/check (integer-list->string '(-30 -100 -109)))
(define icon/login (integer-list->string '(-18 -99 -128)))
(define icon/trash (integer-list->string '(-18 -100 -87)))

(define-record demo-data
  images font-icons font-normal font-bold)

(define (load-demo-data! vg fail)
  (define (warn-then-fail id file)
    (when (not id)
      (display (format "Could not load ~s" file))
      (newline)
      (fail)))
  
  (define images
    (map
     (lambda (idx)
       (let* ((loc (format "./data/images/image~S.jpg" (add1 idx)))
	      (image (nvg:create-image/file! vg loc 0)))
	 (warn-then-fail image loc)
	 image))
     (iota 12 0)))

  (define (load-font name loc)
    (let ((id (nvg:create-font! vg name loc)))
      (warn-then-fail id loc)
      id))

  (define font-icons (load-font "icons" "./data/entypo.ttf"))
  (define font-normal (load-font "sans" "./data/Roboto-Regular.ttf"))
  (define font-bold (load-font "sans-bold" "./data/Roboto-Bold.ttf"))

  (make-demo-data images font-icons font-normal font-bold))

(define (free-demo-data! vg data)
  (map
   (cut nvg:delete-image! vg <>)
   (demo-data-images data)))

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

(define (draw-eyes vg x y w h mx my t)
  (let* ((ex (* w 0.23))
	 (ey (* h 0.5))
	 (lx (+ x ex))
	 (ly (+ y ey))
	 (rx (- (+ x w) ex))
	 (ry (+ y ey))
	 (br (* 0.5 (if (< ex ey) ex ey)))
	 (blink (- 1 (* 0.8 (expt (sin (* 0.5 t)) 200)))))

    ;; Shadow
    (let ((bg (nvg:make-linear-gradient vg x (+ y (* h 0.5)) (+ x (* w 0.1)) (+ y h) (nvg:make-color-rgba 0 0 0 32) (nvg:make-color-rgba 0 0 0 16))))
      (nvg:begin-path! vg)
      (nvg:ellipse! vg (+ lx 3.0) (+ ly 16.0) ex ey)
      (nvg:fill-paint! vg bg)
      (nvg:fill! vg)

      (nvg:begin-path! vg)
      (nvg:ellipse! vg (+ rx 3.0) (+ ry 16.0) ex ey)
      (nvg:fill-paint! vg bg)
      (nvg:fill! vg))

    ;; Whites
    (let ((bg (nvg:make-linear-gradient vg x (+ y (* h 0.25)) (+ x (* w 0.1)) (+ y h) (nvg:make-color-rgba 220 220 220 255) (nvg:make-color-rgba 128 128 128 255))))
      (nvg:begin-path! vg)
      (nvg:ellipse! vg rx ry ex ey)
      (nvg:fill-paint! vg bg)
      (nvg:fill! vg)

      (nvg:begin-path! vg)
      (nvg:ellipse! vg lx ly ex ey)
      (nvg:fill-paint! vg bg)
      (nvg:fill! vg))

    ;; Retina
    (let* ((dx (/ (- mx rx) (* ex 10)))
	   (dy (/ (- my ry) (* ey 10)))
	   (d (sqrt (+ (* dx dx) (* dy dy))))
	   (dx (if (> d 1.0) (/ dx d) dx))
	   (dy (if (> d 1.0) (/ dy d) dy))
	   (dx (* dx ex 0.4))
	   (dy (* dy ey 0.5)))
      (nvg:begin-path! vg)
      (nvg:ellipse! vg (+ lx dx) (+ ly dy (* ey 0.25 (- 1 blink))) br (* br blink))
      (nvg:fill-color! vg (nvg:make-color-rgba 32 32 32 255))
      (nvg:fill! vg)

      (nvg:begin-path! vg)
      (nvg:ellipse! vg (+ rx dx) (+ ry dy (* ey 0.25 (- 1 blink))) br (* br blink))
      (nvg:fill-color! vg (nvg:make-color-rgba 32 32 32 255))
      (nvg:fill! vg))

    ;; Gloss
    (let ((gloss (nvg:make-radial-gradient vg (- lx (* ex 0.25)) (- ly (* ey 0.5)) (* ex 0.1) (* ex 0.75) (nvg:make-color-rgba 255 255 255 128) (nvg:make-color-rgba 255 255 255 0))))
      (nvg:begin-path! vg)
      (nvg:ellipse! vg lx ly ex ey)
      (nvg:fill-paint! vg gloss)
      (nvg:fill! vg))

    ;; Gloss
    (let ((gloss (nvg:make-radial-gradient vg (- rx (* ex 0.25)) (- ry (* ey 0.5)) (* ex 0.1) (* ex 0.75) (nvg:make-color-rgba 255 255 255 128) (nvg:make-color-rgba 255 255 255 0))))
      (nvg:begin-path! vg)
      (nvg:ellipse! vg rx ry ex ey)
      (nvg:fill-paint! vg gloss)
      (nvg:fill! vg))))

(define (draw-paragraph vg x y width height mx my)
  (nvg:save-state! vg)
  (let-values
      (((text) "This is longer chunk of text.\n  \n  Would have used lorem ipsum but she    was busy jumping over the lazy dog with the fox and all the men who came to the aid of the party.")
       ((gutter gx gy) (values #f 0 0))
       ((ascender descender lineh) (nvg:text-metrics vg)))

    (nvg:font-size! vg 18.0)
    (nvg:font-face! vg "sans")
    (nvg:text-align! vg (bitwise-ior nvg:align/left nvg:align/top))

    (do
	((lnum 0 (add1 lnum))
	 (break-lines (nvg:text-break-lines vg text width) (cdr break-lines)))
	((null? break-lines))
      (let* ((hit (and (> mx x) (< mx (+ x width)) (>= my y) (< my (+ y lineh))))
	     (row (car break-lines))
	     (row-width (nvg:text-row-width row))
	     (row-text (substring text (nvg:text-row-start row) (nvg:text-row-end row))))
	(nvg:begin-path! vg)
	(nvg:fill-color! vg (nvg:make-color-rgba 255 255 255 (if hit 64 16)))
	(nvg:rectangle! vg x y row-width lineh)
	(nvg:fill! vg)

	(nvg:fill-color! vg (nvg:make-color-rgba 255 255 255 255))
	(nvg:text! vg x y row-text)
	
	(when hit
	  (let ((caretx (if (< mx (+ x (* 0.5 row-width))) x (+ x row-width)))
		(px x))
	    (do ((glyphs (nvg:text-glyph-positions vg x y row-text) (cdr glyphs)))
		((null? glyphs))
	      (let* ((glyph (car glyphs))
		     (x0 (nvg:glyph-position-x glyph))
		     (x1 (if (null? (cdr glyphs)) (+ x row-width) (nvg:glyph-position-x (cadr glyphs))))
		     (gx (+ (* x0 0.3) (* x1 0.7))))
		(when (and (>= mx px) (< mx gx))
		  (set! caretx x0))
		(set! px gx)))
	    (nvg:begin-path! vg)
	    (nvg:fill-color! vg (nvg:make-color-rgba 255 192 0 255))
	    (nvg:rectangle! vg caretx y 1 lineh)
	    (nvg:fill! vg)

	    (set! gutter (+ 1 lnum))
	    (set! gx (- x 10))
	    (set! gy (+ y (* 0.5 lineh)))))
	(set! y (+ y lineh))))
    (when gutter
      (let ((txt (->string gutter)))
	(nvg:font-size! vg 13.0)
	(nvg:text-align! vg (bitwise-ior nvg:align/right nvg:align/middle))

	(let-values (((advance bounds) (nvg:text-bounds! vg gx gy txt)))
	  (nvg:begin-path! vg)
	  (nvg:fill-color! vg (nvg:make-color-rgba 255 192 0 255))
	  (nvg:rounded-rectangle! vg
				  (fptruncate (- (f32vector-ref bounds 0) 4))
				  (fptruncate (- (f32vector-ref bounds 1) 2))
				  (fptruncate (+ (- (f32vector-ref bounds 2) (f32vector-ref bounds 0)) 8))
				  (fptruncate (+ (- (f32vector-ref bounds 3) (f32vector-ref bounds 1)) 4))
				  (fptruncate (- (* 0.5 (+ (- (f32vector-ref bounds 3) (f32vector-ref bounds 1)) 4)) 1)))
	  (nvg:text! vg gx gy txt))))
    
    (nvg:font-size! vg 13.0)
    (nvg:text-align! vg (bitwise-ior nvg:align/right nvg:align/top))
    (let ((y (+ 20 y))
	  (text "Hover your mouse over the text to see calculated caret position."))
      (let-values
	  (((advance bounds) (nvg:text-bounds! vg x y text)))
	(let* ((gx (fpabs (/ (- mx (* 0.5 (+ (f32vector-ref bounds 0) (f32vector-ref bounds 2))))
			     (- (f32vector-ref bounds 0) (f32vector-ref bounds 2)))))
	       (gy (fpabs (/ (- my (* 0.5 (+ (f32vector-ref bounds 1) (f32vector-ref bounds 3))))
			     (- (f32vector-ref bounds 1) (f32vector-ref bounds 3)))))
	       (a (clamp (fpmax gx gy) 0 1))
	       (px (fptruncate (* 0.5 (+ (f32vector-ref bounds 2) (f32vector-ref bounds 0))))))
	  (nvg:global-alpha! vg a)
	  
	  (nvg:begin-path! vg)
	  (nvg:fill-color! vg (nvg:make-color-rgba 220 220 220 255))
	  (nvg:rounded-rectangle! vg
				  (- (f32vector-ref bounds 0) 2)
				  (- (f32vector-ref bounds 1) 2)
				  (fptruncate (+ (- (f32vector-ref bounds 2) (f32vector-ref bounds 0)) 4))
				  (fptruncate (+ (- (f32vector-ref bounds 3) (f32vector-ref bounds 1)) 4))
				  3)

	  (nvg:move-to! vg px (- (f32vector-ref bounds 1) 10))
	  (nvg:line-to! vg (+ px 7) (add1 (f32vector-ref bounds 1)))
	  (nvg:line-to! vg (- px 7) (add1 (f32vector-ref bounds 1)))
	  (nvg:fill! vg)

	  (nvg:fill-color! vg (nvg:make-color-rgba 0 0 0 220))
	  (nvg:text-box! vg x y 150 text)))))
  (nvg:restore-state! vg))

(define (draw-window vg title x y w h)
  (define corner-radius 3.0)

  (nvg:save-state! vg)

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
    (nvg:text! vg (+ x (* w 0.5)) (+ y 16) title #f))

  (nvg:restore-state! vg))

(define (draw-search-box vg text x y w h)
  (define corner-radius (- (* h 0.5) 1))
  
  (let ((bg (nvg:make-box-gradient vg x (+ y 1.5) w h (* h 0.5) 5 (nvg:make-color-rgba 0 0 0 16) (nvg:make-color-rgba 0 0 0 92))))
    (nvg:begin-path! vg)
    (nvg:rounded-rectangle! vg x y w h corner-radius)
    (nvg:fill-paint! vg bg)
    (nvg:fill! vg))

  (nvg:font-size! vg (* h 1.3))
  (nvg:font-face! vg "icons")
  (nvg:fill-color! vg (nvg:make-color-rgba 255 255 255 64))
  (nvg:text-align! vg (bitwise-ior nvg:align/center nvg:align/middle))
  (nvg:text! vg (+ x (* h 0.55)) (+ y (* h 0.55)) icon/search)

  (nvg:font-size! vg 20.0)
  (nvg:font-face! vg "sans")
  (nvg:fill-color! vg (nvg:make-color-rgba 255 255 255 32))
  (nvg:text-align! vg (bitwise-ior nvg:align/left nvg:align/middle))
  (nvg:text! vg (+ x (* h 1.05)) (+ y (* h 0.5)) text)

  (nvg:font-size! vg (* h 1.3))
  (nvg:font-face! vg "icons")
  (nvg:fill-color! vg (nvg:make-color-rgba 255 255 255 32))
  (nvg:text-align! vg (bitwise-ior nvg:align/center nvg:align/middle))
  (nvg:text! vg (- (+ x w) (* h 0.55)) (+ y (* h 0.55)) icon/circled-cross))

(define (draw-drop-down vg text x y w h)
  (define corner-radius 4.0)

  (let ((bg (nvg:make-linear-gradient vg x y x (+ y h) (nvg:make-color-rgba 255 255 255 16) (nvg:make-color-rgba 0 0 0 16))))
    (nvg:begin-path! vg)
    (nvg:rounded-rectangle! vg (add1 x) (add1 y) (- w 2) (- h 2) (sub1 corner-radius))
    (nvg:fill-paint! vg bg)
    (nvg:fill! vg))

  (nvg:begin-path! vg)
  (nvg:rounded-rectangle! vg (+ x 0.5) (+ y 0.5) (sub1 w) (sub1 h) (- corner-radius 0.5))
  (nvg:stroke-color! vg (nvg:make-color-rgba 0 0 0 48))
  (nvg:stroke! vg)

  (nvg:font-size! vg 20.0)
  (nvg:font-face! vg "sans")
  (nvg:fill-color! vg (nvg:make-color-rgba 255 255 255 160))
  (nvg:text-align! vg (bitwise-ior nvg:align/left nvg:align/middle))
  (nvg:text! vg (+ x (* h 0.4)) (+ y (* h 0.5)) text)

  (nvg:font-size! vg (* h 1.3))
  (nvg:font-face! vg "icons")
  (nvg:fill-color! vg (nvg:make-color-rgba 255 255 255 64))
  (nvg:text-align! vg (bitwise-ior nvg:align/center nvg:align/middle))
  (nvg:text! vg (- (+ x w) (* h 0.5)) (+ y (* h 0.5)) icon/chevron-right))

(define nanovg-context (nvg:create-context))

(define demo-data (load-demo-data! nanovg-context exit))
(set-finalizer! demo-data (cut free-demo-data! nanovg-context <>))

(define (nanovg-render data)
  (let ((w (frame-data-display-width data))
	(h (frame-data-display-height data))
	(mx (frame-data-mouse-x data))
	(my (frame-data-mouse-y data))
	(t (frame-data-time data)))
    (nvg:begin-frame! nanovg-context w h (/ w h))

    (draw-eyes nanovg-context (- w 250) 50 150 100 mx my t)
    (draw-paragraph nanovg-context (- w 450) 50 150 100 mx my)
    
    (draw-window nanovg-context "Widgets 'n Stuff" 50 50 300 400)
    (draw-search-box nanovg-context "Search" 60 95 280 25)
    (draw-drop-down nanovg-context "Effects" 60 135 280 28)

    (nvg:end-frame! nanovg-context)))

(render-thunks (cons 'nanovg-render (render-thunks)))
