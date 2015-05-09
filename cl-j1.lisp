
;;; j1 assembler
;;; 
;;; Here are some examples of how to us this... 
;;;
;;; Build an opcode:
;;; J1> (opcode '(:+ :ret :t->n :d++))
;;; 4737
;;;
;;; Decode an opcode:
;;; J1> (decode 4737)
;;; (:+ :RET :T->N :D++)
#|
 Assemble a line, to see the output (memory not affected)
 J1> (asm-line-prim '(:mylabel  (:>> :d++ :ret) "comment: shift right and drop, return"))
; (asm-line '(:MYLABEL (:>> :RET :D++) "comment: shift right and drop, return")
 This is actually an aline structure, displayed as an (asm-line...) form.

 Assemble code to mem vector

(defun test ()
  (asm-reset) ;; reset assembler state
  (asm 
   '(
     ( :restart (:lit 3) "counter")
     ( :loop1   (:--)    "decrement counter")
     (  (:t->n :d++)     "dup")
     (  (:cjmp :loop1))
     (  (:n :d--)        "drop" )
     (  (:jmp :restart)  "and do it again")
    )))

 And run in simulator

(j1-reset) ;reset 
(?)        ;display cpu registers
(steps 1)  ;single-step and display registers
(steps 1000) ;run a bunch of steps
mem  ;display memory
|#

(in-package :cl-j1)

(defparameter mem nil)
(defparameter symtab nil) ;a hashtable of labels 
(defparameter outptr 0) ;will assemble to here


(defparameter *op-alu* 
  '((:t     . #b0000000000000000 )
    (:n     . #b0000000100000000 )
    (:+     . #b0000001000000000 )
    (:and   . #b0000001100000000 )
    (:or    . #b0000010000000000 )
    (:xor   . #b0000010100000000 )
    (:~     . #b0000011000000000 )
    (:eq    . #b0000011100000000 )
    (:<     . #b0000100000000000 )
    (:>>    . #b0000100100000000 )
    (:--    . #b0000101000000000 )
    (:r     . #b0000101100000000 )
    (:read  . #b0000110000000000 )
    (:<<    . #b0000110100000000 )
    (:depth . #b0000111000000000 )
    (:u<    . #b0000111100000000 )))

(defparameter *op-singlebit* 
  '(;; alu operations:
    ;; and regular operations
    (:ret   . #b0001000000000000 )
    (:t->n  . #b0000000010000000 )
    (:t->r  . #b0000000001000000 )
    (:write . #b0000000000100000 )
    ))

(defparameter *op-multibit*
  '((:r++   . #b0000000000000100 )
    (:r--   . #b0000000000001100 )
    (:r-2   . #b0000000000001000 )
    (:d++   . #b0000000000000001 )
    (:d--   . #b0000000000000011 )
    (:d-2   . #b0000000000000010 )))
;; The operand may be a literal/address or a symbol in symtab...
(defun resolve-symbol (it)
  (if (symbolp it)
      (prog1
	  (multiple-value-bind (target exists) (gethash it symtab)
	    (if exists
		target
		(progn
		  (format t "Unable to resolve symbol ~S" it)))))
      
      it)
  )
;; figure out the bits
(defmacro assoc-int (item alist)
  `(or (cdr (assoc ,item ,alist)) 0))
(defmacro op-special (prefix operand)
  `(logior ,prefix (resolve-symbol ,operand)))
(defun opcode (oplist)
    (let ( (opval 0) (operand (cadr oplist)))
    (case (car oplist)
      (:lit  (setf opval (op-special #x8000 operand)))
      (:jmp  (setf opval (op-special #x0000 operand)))
      (:cjmp (setf opval (op-special #x2000 operand)))
      (:call (setf opval (op-special #x4000 operand)))
      (otherwise
       (dolist (item oplist)
	 (setf opval 
	   (logior #x6000
		   opval ;; or ... 0 assures that nil case returns 0
		   (assoc-int item *op-alu*)
		   (assoc-int item *op-singlebit*)
		   (assoc-int item *op-multibit*))))))
    opval))

;;; Instruction decoder helper routines. 
;;; These scan the appropriate alists and build name-list
(defun op-alu-name (opcode)
  (cons
   (car (rassoc (logand opcode #xF00) *op-alu*)) ;skip the :t
   nil))

(defun op-singlebit-names (opcode)
  (loop for op? in *op-singlebit*
     if (not (zerop (logand (cdr op?) opcode)))
     collect (car op?)) )

(defun op-multibit-names (opcode)
  (loop for op? in *op-multibit*
     if (or (= (cdr op?) (logand opcode #x3))	;; dstack bits
	    (= (cdr op?) (logand opcode #xC))) ;; rstack bits
     collect (car op?)))

(defun decode (opcode)
  (case (logand opcode #xE000)
    (#x8000 (list :lit (logand opcode #x7FFF)))
    (#x0000 (list :jmp (logand opcode #x1FFF)))
    (#x2000 (list :cjmp (logand opcode #x1FFF)))
    (#x4000 (list :call (logand opcode #x1FFF)))
    (otherwise
     (append (op-alu-name opcode)
	     (op-singlebit-names opcode)
	     (op-multibit-names opcode)))))

(defstruct (aline 
	     ;; Print it as an (asmline '(...)) form
	     (:print-function 
	      (lambda (struct stream depth)
		(declare (ignore depth))
		(format stream "~&(asm-line '(~S ~S ~S)"
			(aline-label struct )
			;(aline-op struct)
			(decode (aline-op struct))
			(aline-comment struct))))
	     ) label op comment)

;;; An assembly line (aline) contains 3 parts: a :label, a "comment"  and (a numeric opcode)
;; Assemble a line
(defun asm-line-prim (line)
  (let ((label NIL) (op NIL) (comment NIL))
    (dolist (fragment line)
      (typecase fragment
	(keyword (setf label fragment))       ; :keywords are considered labels
	(string  (setf comment fragment))     ; "strings" are comments
	(list    (setf op (opcode fragment))) ; (lists) are eval'd by opcode

	(integer (setf op fragment))          ; integer? just set the opcode
	(t (format t "fragment ~S is of type ~S which is not expected."
		   fragment (type-of fragment)))))
        (make-aline :label label :op op :comment comment)))

(defun asm-line (line)
  (let ((al (asm-line-prim line)))    ; assemble a line
    (if (not (null (aline-label al))) ; if there is a label
	(setf (gethash (aline-label al) symtab) outptr)) ; put it into the symbol table
    (setf (elt mem outptr) (aline-op al))   )
)





(defun asm-reset ()
    (setf
     outptr 0 
     mem (make-array 64 :element-type '(unsigned-byte 16))
     symtab (make-hash-table)
     )
  )

(defun asm (bunch)
  (dolist (line bunch)
    (asm-line line)         ; assemble a line
    (incf outptr)
    )

  )


(defun test ()
  (asm-reset)
  (asm 
   '(
     ( :restart (:lit 3) "counter")
     ( :loop1 (:--) "decrement counter")
     (  (:t->n :d++) "dup")
     (  (:cjmp :loop1))
     (  (:n :d--))
     (  (:jmp :restart))
    
    )))

;;; simulator


(defparameter dstack (make-array 32 :element-type '(unsigned-byte 16)))
(defparameter rstack (make-array 32 :element-type '(unsigned-byte 16)))
(defparameter tos 0)
(defparameter pc 0)
(defparameter dsp 0)
(defparameter rsp 0)

(defmacro wrap-word (val)
  `(logand #xFFFF ,val))

(defmacro opcode-extract-address (opcode)
  `(logand #x1FFF ,opcode))

(defun step1 ()
  ;(format t "~%ok, pc is ~A" pc)

  (let ((opcode (elt mem pc))
	(nos-ro (elt dstack dsp))
	(tor-ro (elt rstack rsp)))
    (if (logbitp 15 opcode) ;literal
	(progn
	  (setf (elt dstack dsp) tos)          ;; implied t->n
	  (incf dsp)                           ;; implied DSP++
	  (setf tos (logand #x7FFF opcode))    ;; load literal into t
	  (incf pc))
	(case (logand #x6000 opcode)  
	  ((#x0000) ; jump
	   (setf pc (opcode-extract-address opcode)))
	  ((#x2000) ; cjmp
	   (if (zerop tos)
	       (incf pc)
	       (setf pc (opcode-extract-address opcode)))
	   (setf tos nos-ro) (decf dsp)) ;implied drop
	  ((#x4000) ; call
	   (incf pc)
	   (incf rsp)
	   (setf (elt rstack rsp) pc)
	   (setf pc (opcode-extract-address opcode)))
	  (otherwise
	   (if (logbitp 12 opcode); return
	       (setf pc tor-ro))	 
	   ;;(format t "~%PC was ~A" pc)
	   (incf pc)
	   ;;(format t "~%PC is now ~A" pc)
	   (let ((alu-op (logand #x0F00 opcode)))
	     (setf tos 
		   (case alu-op 
		     ((#x0000) tos)
		     ((#x0100) nos-ro)
		     ((#x0200) (wrap-word (+      tos nos-ro)))
		     ((#x0300) (logand tos nos-ro))
		     ((#x0400) (logior tos nos-ro))
		     ((#x0500) (logxor tos nos-ro))
		     ((#x0600) (lognot tos))
		     ((#x0700) (if (= tos nos-ro) 1 0))
		     ((#x0800) (if (< nos-ro tos) 1 0))
		     ((#x0900) (ash nos-ro tos))
		     ((#x0A00) (wrap-word (1- tos)))
		     ((#x0B00) tor-ro)
		     ((#x0C00) (elt mem tos))
		     ((#x0D00) (ash nos-ro (wrap-word (- 0 tos))))
		     ((#x0E00) dsp)
		     ((#x0F00) (if (< nos-ro tos) 1 0)) ;;todo: this should be unsigned...
		     ))
	     (if (logbitp 7 opcode); (not (zerop (logand #x0080 opcode)))
		 (setf (elt dstack dsp) tos)) ;; T->N
	     (if (logbitp 6 opcode); (not (zerop (logand #x0040 opcode)))
		 (setf (elt rstack dsp) tos)) ;; T->R
	     (if (logbitp 5 opcode); (not (zerop (logand #x0020 opcode)))
		 (setf (elt mem tos) nos-ro))
	     (case (logand #x000C opcode)
	       ((#x0004) (incf rsp))
	       ((#x000C) (setf rsp (- rsp 2)))
	       ((#x0008) (decf rsp)))
	     (case (logand #x0003 opcode)
	       ((#x0001) (incf dsp))
	       ((#x0002) (setf dsp (- dsp 2)))
	       ((#x0003) (decf dsp)))  

	     ;; after 

	     ))))
;    (values  pc opcode)
    nil
    ))

(defun ? () 
  (format t "~%~4A       ~4A ~4A ~4A ~4A" "PC" "TOS" "NOS" "DSP" "RSP" )
  (format t "~%~4,'0X ~4,'0X  ~4,'0X ~4,'0X ~4,'0X ~4,'0X " pc (elt mem pc) tos (elt dstack dsp) dsp rsp ) 
  (format t " ~S" (decode (elt mem pc))))

(defun steps (thismany)
  (dotimes (i thismany) (step1))
  (?))

(defun j1-reset (&optional (addr 0))
  (setf pc addr
	dsp 0
	rsp 0
	tos 0) )
