;;;; x86_64 specific compiler code, generates NASM syntax


(define fixnum-range '(-4611686018427387904 . 4611686018427387903))
;; (define fixnum-range '(-1073741824 . 1073741823))
(define temporary-registers '(rax r11 r15))
(define argument-registers '(SELF rcx rdx rsi rdi r8 r9 r10 r12))
(define word-size 8)			; bytes
(define target-arch 'x86_64)
(define target-endianness 'little-endian)
(define arg-register 'rax)
(define alloc-register 'ALLOC)
(define self-register 'SELF)
(define false-register 'FALSE)
(define count-register 'r11)
(define stack-register 'rsp)


(define (generate-header features)
  (emit ";;; GENERATED BY BONES " bones-version "\n")
  (for-each
   (cut emit "%define " <> "\n")
   features)
  (emit "%include \"x86_64/boneslib.s\"\n"))

(define (generate-trailer)
  (emit ";;; END OF GENERATED CODE\n"))

(define (generate-primitives)
  (emit " section .data\n")
  (for-each
   (match-lambda
     ((l . name)
      (generate-label l)
      (generate-defword (typecode 'CLOSURE 1) name)))
   primitives))

(define (generate-section name)
  (emit " section " name "\n"))

(define (generate-defword val1 . vals)
  (emit " dq ")
  (emit val1)
  (for-each (cut emit #\, <>) vals)
  (emit "\n"))

(define (generate-defbyte val1 . vals)
  (emit " db ")
  (emit val1)
  (for-each (cut emit #\, <>) vals)
  (emit "\n"))

(define (generate-defstring str)
  (emit " db ")
  (let ((len (string-length str)))
    (do ((i 0 (add1 i)))
	((>= i len))
      (unless (zero? i) (emit ", "))
      (emit (char->integer (string-ref str i)))))
  (emit "\n"))

(define (generate-deffloat val)
  (emit " dq __float64__(" val ")\n"))

(define (generate-align bytes)
  (emit " align " bytes "\n"))

(define (generate-padding bytes)
  (emit " resb " bytes "\n"))

(define (generate-equ name . vals)
  (emit name " equ ")
  (for-each emit vals)
  (emit "\n"))

(define (generate-closure-alloc n id)
  (emit " mov rax, " (typecode 'CLOSURE (add1 n)) "\n mov [ALLOC], rax\n")
  (if enable-pic
      (emit " lea rax, [f_" id "]\n"
	    " mov qword [ALLOC + " (cells 1) "], rax\n")
      (emit " mov qword [ALLOC + " (cells 1) "], f_" id "\n")))

(define (generate-move dest src)
  (unless (eq? dest src)
    (emit " mov " dest ", " src "\n")))

(define (generate-add r n)
  (emit " add " r ", " n "\n"))

(define (generate-move-to-local off src)
  (emit " mov [locals + " off "], " src "\n"))

(define (generate-reserve-on-stack bytes)
  (emit " sub rsp, " bytes "\n"))

(define (generate-pop-stack bytes)
  (emit " add rsp, " bytes "\n"))

(define (generate-comment . text)
  (emit "; ")
  (for-each emit text)
  (emit "\n"))

(define (generate-expr-comment expr)
  (emit "; ") 
  (write expr)
  (emit "\n"))

(define (generate-global-store var name src)
  (emit " mov [" name "], " src " ; (set! " var " ...)\n"))

(define (generate-global-ref dest var name)
  (emit " mov " dest ", [" name "] ; " var "\n"))

(define (generate-local-ref dest var src)
  (emit " mov " dest ", [locals + " src "] ; " var "\n"))

(define (generate-local-store var dest src)
  (emit " mov [locals + " dest "], " src " ; (set! " var " ...)\n"))

(define (generate-conditional-branch r lbl)
  (emit " cmp " r ", FALSE\n je " lbl "\n"))

(define (generate-jump lbl)
  (emit " jmp " lbl "\n"))

(define (generate-immediate-ref dest val . comment)
  (emit " mov " dest ", " val " ; ")
  (for-each emit comment)
  (emit "\n"))

(define (generate-slot-ref dest src off)
  (emit " mov " dest ", [" src " + " off "]\n"))

(define (generate-slot-store dest off src)
  (emit " mov [" dest " + " off "], " src "\n"))

(define (generate-true-ref r)
  (emit " SET_T " r "\n"))

(define (generate-alloc-check)
  (emit " cmp ALLOC, LIMIT\n ja reclaim\n"))

(define (generate-tail-call r)
  (emit " jmp " r "\n"))

(define (generate-direct-tail-call lbl)
  (emit " jmp " lbl "\n"))

(define (generate-call name)
  (emit " call " name "\n"))

(define (generate-argc-check argc rest? lbl)
  (emit " cmp r11, " argc "\n "
	(if rest? "jb " "jne ") lbl "\n"))

(define (generate-conditional-move test src dest)
  (emit " cmp " test ", FALSE\n cmove " dest ", " src "\n"))

(define (generate-alloc-alignment)
  ;; ALIGNMENT: on 32-bit platforms, we must align the data-area of a block if it is a flonum
  #f)

(define (generate-procedure-check)
  (emit " CHECK_PROCEDURE\n"))

(define (generate-conditional-set cnd r)
  (emit " SET_T " r "\n cmov" (inverted-condition cnd) " " r ", FALSE\n"))

(define (generate-conditional-branch/cond cnd r lbl)
  (emit " j" (inverted-condition cnd) " " lbl "\n"))

(define (generate-conditional-move/cond cnd src dest)
  (emit " cmov" (real-condition cnd) " " dest ", " src "\n"))

(define (real-condition cnd)
  (case (symbolify cnd)
    ((eq) 'e)
    ((lt) 'l)
    ((gt) 'g)
    (else cnd)))

(define (inverted-condition cnd)
  (case (symbolify cnd)
    ((eq) 'ne)
    ((ne) 'e)
    ((gt) 'le)
    ((lt) 'ge)
    ((ge) 'l)
    ((le) 'g)
    ((a) 'be)
    ((b) 'ae)
    ((ae) 'b)
    ((be) 'a)
    (else (error "conditional code not supported for this architecture" cnd))))

(define (generate-label name)
  (emit name ":\n"))

(define (generate-instruction instr)
  (emit " " instr "\n"))