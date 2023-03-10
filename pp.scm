;; pp.scm - pretty printer


; Copyright (c) 1991, Marc Feeley
; Author: Marc Feeley (feeley@iro.umontreal.ca)
; Distribution restrictions: none

; modified for use with this Scheme system

(define *pretty-print-width* 79)
(define (pretty-print-hook x out) #f)	; must tail-call "out"!

(define (pretty-print obj . opt)
  (let ((port (if (pair? opt) (car opt) (current-output-port))))
    (define generic-write
      (lambda (obj display? width output)

	(define (read-macro? l)
	  (define (length1? l) (and (pair? l) (null? (cdr l))))
	  (let ((head (car l)) (tail (cdr l)))
	    (case head
	      ((quote quasiquote unquote unquote-splicing) (length1? tail))
	      (else                                        #f))))

	(define (read-macro-body l)
	  (cadr l))

	(define (read-macro-prefix l)
	  (let ((head (car l)) (tail (cdr l)))
	    (case head
	      ((quote)            "'")
	      ((quasiquote)       "`")
	      ((unquote)          ",")
	      ((unquote-splicing) ",@"))))

	(define (out str col)
	  (and col (output str) (+ col (string-length str))))

	(define (char-name c)
	  (case c
	    ((#\space) "space")
	    ((#\newline) "newline")
	    ((#\return) "return")
	    ((#\tab) "tab")
	    (else #f)))

	(define (wr obj col)

	  (define (out/col col)
	    (lambda (x) (out x col)))

	  (define (wr-expr expr col)
	    (if (read-macro? expr)
		(wr (read-macro-body expr) (out (read-macro-prefix expr) col))
		(wr-lst expr col)))

	  (define (wr-lst l col)
	    (if (pair? l)
		(let loop ((l (cdr l))
			   (col (and col (wr (car l) (out "(" col)))))
		  (cond ((not col) col)
			((pair? l)
			 (loop (cdr l) (wr (car l) (out " " col))))
			((null? l) (out ")" col))
			(else      (out ")" (wr l (out " . " col))))))
		(out "()" col)))

	  (define (bytevector->list bv)
	    (let loop ((i (- (bytevector-length bv) 1)) (lst '()))
	      (if (negative? i)
		  lst
		  (loop (- i 1) (cons (bytevector-u8-ref bv i) lst)))))
	  
	  (cond ((pretty-print-hook obj (out/col col)))
		((pair? obj)        (wr-expr obj col))
		((null? obj)        (wr-lst obj col))
		((boolean? obj)     (out (if obj "#t" "#f") col))
		((number? obj)      (out (number->string obj) col))
		((symbol? obj)      (out (symbol->string obj) col))
		((vector? obj)      (wr-lst (vector->list obj) (out "#" col)))
		((bytevector? obj)  (wr-lst (bytevector->list obj) (out "#u8" col)))
		((string? obj)      (if display?
					(out obj col)
					(let loop ((i 0) (j 0) (col (out "\"" col)))
					  (if (and col (< j (string-length obj)))
					      (let ((c (string-ref obj j)))
						(if (or (char=? c #\\)
							(char=? c #\"))
						    (loop j
							  (+ j 1)
							  (out "\\"
							       (out (substring obj i j)
								    col)))
						    (loop i (+ j 1) col)))
					      (out "\""
						   (out (substring obj i j) col))))))
		((procedure? obj)   (out "#<procedure>" col)) ; not used
		((char? obj)        (if display?
					(out (make-string 1 obj) col)
					(let ([code (char->integer obj)])
					  (out "#\\" col)
					  (cond [(char-name obj) 
						 => (lambda (cn) 
						      (out cn col) ) ]
						[(< code 32)
						 (out "x" col)
						 (out (number->string code 16) col) ]
						[(> code 255)
						 (out (if (> code #xffff) "U" "u") col)
						 (out (number->string code 16) col) ]
						[else (out (make-string 1 obj) col)] ) ) ) )
		((eof-object? obj)  (out "#<eof>" col))
		((input-port? obj)  (out "#<input port>" col))
		((output-port? obj)  (out "#<output port>" col))
		((eq? (void) obj)   (out "#<undefined>" col))
		(else               (out "#<unprintable object>" col)) ) )

	(define (pp obj col)

	  (define (spaces n col)
	    (if (> n 0)
		(if (> n 7)
		    (spaces (- n 8) (out "        " col))
		    (out (substring "        " 0 n) col))
		col))

	  (define (indent to col)
	    (and col
		 (if (< to col)
		     (and (out (make-string 1 #\newline) col) (spaces to 0))
		     (spaces (- to col) col))))

	  (define (pr obj col extra pp-pair)
	    (if (or (pair? obj) (vector? obj)) ; may have to split on multiple lines
		(let ((result '())
		      (left (max (+ (- (- width col) extra) 1) max-expr-width)))
		  (generic-write obj display? #f
				 (lambda (str)
				   (set! result (cons str result))
				   (set! left (- left (string-length str)))
				   (> left 0)))
		  (if (> left 0)      ; all can be printed on one line
		      (out (reverse-string-append result) col)
		      (if (pair? obj)
			  (pp-pair obj col extra)
			  (pp-list (vector->list obj) (out "#" col) extra pp-expr))))
		(wr obj col)))

	  (define (pp-expr expr col extra)
	    (if (read-macro? expr)
		(pr (read-macro-body expr)
		    (out (read-macro-prefix expr) col)
		    extra
		    pp-expr)
		(let ((head (car expr)))
		  (if (symbol? head)
		      (let ((proc (style head)))
			(if proc
			    (proc expr col extra)
			    (if (> (string-length (symbol->string head))
				   max-call-head-width)
				(pp-general expr col extra #f #f #f pp-expr)
				(pp-call expr col extra pp-expr))))
		      (pp-list expr col extra pp-expr)))))

					; (head item1
					;       item2
					;       item3)
	  (define (pp-call expr col extra pp-item)
	    (let ((col* (wr (car expr) (out "(" col))))
	      (and col
		   (pp-down (cdr expr) col* (+ col* 1) extra pp-item))))

					; (item1
					;  item2
					;  item3)
	  (define (pp-list l col extra pp-item)
	    (let ((col (out "(" col)))
	      (pp-down l col col extra pp-item)))

	  (define (pp-down l col1 col2 extra pp-item)
	    (let loop ((l l) (col col1))
	      (and col
		   (cond ((pair? l)
			  (let ((rest (cdr l)))
			    (let ((extra (if (null? rest) (+ extra 1) 0)))
			      (loop rest
				    (pr (car l) (indent col2 col) extra pp-item)))))
			 ((null? l)
			  (out ")" col))
			 (else
			  (out ")"
			       (pr l
				   (indent col2 (out "." (indent col2 col)))
				   (+ extra 1)
				   pp-item)))))))

	  (define (pp-general expr col extra named? pp-1 pp-2 pp-3)

	    (define (tail1 rest col1 col2 col3)
	      (if (and pp-1 (pair? rest))
		  (let* ((val1 (car rest))
			 (rest (cdr rest))
			 (extra (if (null? rest) (+ extra 1) 0)))
		    (tail2 rest col1 (pr val1 (indent col3 col2) extra pp-1) col3))
		  (tail2 rest col1 col2 col3)))

	    (define (tail2 rest col1 col2 col3)
	      (if (and pp-2 (pair? rest))
		  (let* ((val1 (car rest))
			 (rest (cdr rest))
			 (extra (if (null? rest) (+ extra 1) 0)))
		    (tail3 rest col1 (pr val1 (indent col3 col2) extra pp-2)))
		  (tail3 rest col1 col2)))

	    (define (tail3 rest col1 col2)
	      (pp-down rest col2 col1 extra pp-3))

	    (let* ((head (car expr))
		   (rest (cdr expr))
		   (col* (wr head (out "(" col))))
	      (if (and named? (pair? rest))
		  (let* ((name (car rest))
			 (rest (cdr rest))
			 (col** (wr name (out " " col*))))
		    (tail1 rest (+ col indent-general) col** (+ col** 1)))
		  (tail1 rest (+ col indent-general) col* (+ col* 1)))))

	  (define (pp-expr-list l col extra)
	    (pp-list l col extra pp-expr))

	  (define (pp-lambda expr col extra)
	    (pp-general expr col extra #f pp-expr-list #f pp-expr))

	  (define (pp-if expr col extra)
	    (pp-general expr col extra #f pp-expr #f pp-expr))

	  (define (pp-cond expr col extra)
	    (pp-call expr col extra pp-expr-list))

	  (define (pp-case expr col extra)
	    (pp-general expr col extra #f pp-expr #f pp-expr-list))

	  (define (pp-and expr col extra)
	    (pp-call expr col extra pp-expr))

	  (define (pp-let expr col extra)
	    (let* ((rest (cdr expr))
		   (named? (and (pair? rest) (symbol? (car rest)))))
	      (pp-general expr col extra named? pp-expr-list #f pp-expr)))

	  (define (pp-begin expr col extra)
	    (pp-general expr col extra #f #f #f pp-expr))

	  (define (pp-do expr col extra)
	    (pp-general expr col extra #f pp-expr-list pp-expr-list pp-expr))

					; define formatting style (change these to suit your style)

	  (define indent-general 2)

	  (define max-call-head-width 5)

	  (define max-expr-width 50)

	  (define (style head)
	    (case head
	      ((lambda let* letrec define) pp-lambda)
	      ((if set!)                   pp-if)
	      ((cond)                      pp-cond)
	      ((case)                      pp-case)
	      ((and or)                    pp-and)
	      ((let)                       pp-let)
	      ((begin)                     pp-begin)
	      ((do)                        pp-do)
	      (else                        #f)))

	  (pr obj col 0 pp-expr))

	(if width
	    (out (make-string 1 #\newline) (pp obj 0))
	    (wr obj 0))) )

    ;; (reverse-string-append l) = (apply string-append (reverse l))
    
    (define (reverse-string-append l)
  
      (define (rev-string-append l i)
	(if (pair? l)
	    (let* ((str (car l))
		   (len (string-length str))
		   (result (rev-string-append (cdr l) (+ i len))))
	      (let loop ((j 0) (k (- (- (string-length result) i) len)))
		(if (< j len)
		    (begin
		      (string-set! result k (string-ref str j))
		      (loop (+ j 1) (+ k 1)))
		    result)))
	    (make-string i)))

      (rev-string-append l 0))

    (generic-write obj #f *pretty-print-width* (lambda (s) (display s port) #t))
    (void)))

(define pp pretty-print)
