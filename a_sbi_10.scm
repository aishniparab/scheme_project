#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;; Evan Hobbs - ehobbs
;; Aishni Parab - aparab
;; NAME
;;    sbi.scm - silly basic interpreter
;;
;; SYNOPSIS
;;    sbi.scm filename.sbir
;;
;; DESCRIPTION
;;    The file mentioned in argv[1] is read and assumed to be an SBIR
;;    program, which is the executed.  Currently it is only printed.
;;

(define *label-table* (make-hash))
(define *function-table* (make-hash))
(define *variable-table* (make-hash))

(define (put-label! key value)
   (hash-set! *label-table* key value))
(define (put-function! key value)
   (hash-set! *function-table* key value))
(define (put-variable! key value)
   (hash-set! *variable-table* key value))

;(define (put-array! name index value)
  ;(vector-set! (get-variable name) index value) 
;)
 
;(define (make-array! expr)
 ; (set! name (car expr))
 ; (set! size (handle-expr(cadr expr)))
 ; (let ((arr (make-vector size name)))
 ;  (put-variable! name (+ size 1))) 
;)

(define (get-label key)
   (hash-ref *label-table* key #f))
(define (get-function key)
   (hash-ref *function-table* key #f))
(define (get-variable key)
   (hash-ref *variable-table* key #f))

(for-each
                  (lambda (pair)
                      (put-variable! (car pair) (cadr pair)))
                  `(
                     (e       
               2.718281828459045235360287471352662497757247093)
                     (pi      
               3.141592653589793238462643383279502884197169399)

                   )
)

(for-each
                  (lambda (pair)
                      (put-function! (car pair) (cadr pair)))
                  `(
                      (log10_2 
               0.301029995663981195213738894724493026768189881)
                      (sqrt_2  
               1.414213562373095048801688724209698078569671875)
                      (e       
               2.718281828459045235360287471352662497757247093)
                      (pi      
               3.141592653589793238462643383279502884197169399)
                      (/ ,/) (abs ,abs)
                      (<= ,<=) (>= , >=) (= ,=) (> ,>) (< , <)
                      (sin ,sin)
                      (cos ,cos) 
                      (tan ,tan) 
                      (atan ,(lambda (x) (atan x)))  
                      (ceil ,ceiling) 
                      (exp ,exp) (floor ,floor)
                      (^ ,expt)
                      (asin ,asin) (acos ,acos) (round ,round) 
                      (log ,log) (sqrt ,sqrt)
                      (div     ,(lambda (x y) (floor (/ x y))))
                      (log10   ,(lambda (x) (/ (log x) (log 10.0))))
                      (mod     ,(lambda (x y) (- x (* (div x y) y))))
                      (quot    ,(lambda (x y) (truncate (/ x y))))
                      (rem     ,(lambda (x y) (- x (* (quot x y) y))))
                      (<>      ,(lambda (x y) (not (= x y))))
                      (+ ,+)
                      (- ,-)
                      (* ,*)
                      (% ,(lambda (x y) (truncate (/ x y))))
               ))

(define *stderr* (current-error-port))

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)

(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (in-var-tab? expr)
   (if (list? expr)
       (if (vector? (get-variable (car expr))) 
        (vector-ref (get-variable (car expr)) 
               (inexact->exact(handle-expr(cadr expr))))
       ;else
       (get-variable (car expr))) ;if ends here
    (get-variable expr)
   )
)

(define (handle-expr expr)
    (cond 
    [(string? expr) expr]     
 
    [(number? expr)
      (if (= expr 0) '0.0 expr)
    ]
    
    [(in-var-tab? expr) 
      (in-var-tab? expr)]

    [(pair? expr) 
     (if (get-function (car expr)) 
      (let ([var (car expr)])
       (cond
         
         ; check if var is a function
         [(symbol? var)
          (define func (get-function var))
          (if (number? func) func
          (apply func (map (lambda (x) (handle-expr x)) (cdr expr))))]
        
         ; check if var is a vector/array
         [(vector? var) 
          (vector-ref var (cadr expr))
         ]
 
         (else (die (list "")))        
       ))
     (cond 
      
       [(number? (car expr)) 
         (car expr)]
        (else (die (list "")))
     )
     );ends cond and let and if
     ] ;ends list
     )
)

(define (handle-dim line)
    (define name (caar(cdr line)))
    (define size (inexact->exact(
         handle-expr (car(cdr(car(cdr line)))))))
    (define *vector* (make-vector size))
    (put-variable! name *vector*) 
)

(define (let-array line)
    (let ((array (make-vector (handle-expr (cadr line)) (car line) )))
     (put-variable! (car line) (+ (inexact->exact(handle-expr (cadr line)))  1)))      
)

(define (let-unarray line)
       (define var-name (car(cdr line))) 
       (define value (handle-expr (caddr line)))
       (put-variable! var-name value) 
)

(define (handle-let line)
    (if (symbol?(cadr line))
       (let-unarray line)
       (let-array line)
    )
)

(define (handle-goto label)
   (process-program (get-label label))
)

(define (handle-relop relop-expr)
   (define relop (car relop-expr))
   (define l (handle-expr (cadr relop-expr)))
   (define r (handle-expr (caddr relop-expr)))
   (cond
       [(eqv? relop '=)
         (= l r)]
       [(eqv? relop '<)
         (< l r)]
       [(eqv? relop '>)
         (> l r)]
       [(eqv? relop '<>)
         ((get-function (car relop-expr)) l r)]
       [(eqv? relop '>=)
         (>= l r)]
       [(eqv? relop '<=) 
         (<= l r)]
   )
)

(define (handle-if line)
    (when (handle-relop (cadr line))
       (handle-goto (caddr line))
    )
)

(define (print-opt args)
  (if (string? (car args))
     (display (car args))
     (display (handle-expr (car args)))
  )     
  ;recusion
  (when not (null?(cdr args))
  (when (not(=(length(cdr args))0))
     (print-opt (cdr args))
  )
  )
)

(define (handle-print line)
   (if (null? (cdr line))
      (display "\n")
      ;else, get rid of () 
      (when not (=(length(cdr line))0)
         (print-opt (cdr line))
      ) 
   ) (display "\n") ;;this to print line by line
)

(define (input-opt line input-count)
  (if (null? line) 
    input-count ;if null return current input-count value
     (let ((value (read))) ;read from console let value be this
       (if (eof-object? value) 
       -1 ;return 1 if reached eof
        (begin
          (put-variable! (car line) value) ;put value in var table
          (set! input-count (+ 1 input-count)) ;increment input-count
          (input-opt (cdr line) input-count) ;recurse on the rest
        )))) ;end begin, if, let, if
)

(define (handle-input line)
  ;initialize inputcount to be 0
  (put-variable! 'inputcount 0)
  (if (null? (car line)) 
   (put-variable! 'inputcount -1)
   (begin
   (put-variable! 'inputcount (input-opt (cdr line) 0))
 ));begin, if ends here
) 

(define (read-opt line)
   (when (> (length line) 0)
   (cond
              [(eqv? (car line) 'goto)
                 (put-function! (car line) (cdr line))]
              [(eqv? (car line) 'print)
                 (put-function! (car line) (cdr line))]
              [(eqv? (car line) 'input)
                 (put-function! (car line) (cdr line))]
   )
   )
)


(define (read-program prgm)
   (when (> (length (car prgm)) 1);not (null? (cdr(car prgm)))
   (when not (null? (cdr prgm))
         (when (symbol? (cadr (car prgm)))
         ;we have a label, put it into the the label table 
            (put-label! (cadr(car prgm)) prgm)
            (read-opt (cddr(car prgm)))
         )
         (when not (symbol? (cadr (car prgm)))
            ;no label, just pass in the function we're gonna do
            (read-opt (cdr(car prgm)))
         )
   ))
   (when not (null? (cdr prgm))
   (when (> (length (cdr prgm)) 0)
         (read-program (cdr prgm))
   ))
)

(define (process-opt line)
 (when (eqv? line `()) (die (list "")))
   (define operator (car line))
   (cond
           [(eqv? operator 'dim)
              (handle-dim line)]
           [(eqv? operator 'let)
              (handle-let line)]
           [(eqv? operator 'goto)
              (handle-goto (cadr line))] 
           [(eqv? operator 'if)
              (handle-if line)]
           [(eqv? operator 'print) 
              (handle-print line)]
           [(eqv? operator 'input)
              (handle-input line)]
         )
  ;); end when
)

(define (process-program prgm)
   (when (eqv? prgm `()) (die (list ""))) ;first when ends here
   (when (> (length (car prgm)) 1);skip blank lines
  ; (when not (null? (cdr prgm)) 
        ;check if theres a label in the line
        ;and send only the operation to the sub function
        (when (symbol? (cadr (car prgm)))            
            (if (= (length (car prgm)) 2)
            (process-opt (cddr(car prgm)))
            (process-opt (car(cddr(car prgm)))))
        )
        (when not (symbol? (cadr (car prgm))); (display "\n XXX \n")
           (if (list? (car(cdr(car prgm)))) 
            (process-opt (car(cdr(car prgm))))
            ;else 
            (process-opt (cdr(car prgm)))
            )
        ) 
    );second to top when
    ;(when not (null? (cdr prgm))
    (when (> (length (cdr prgm)) 0)
        (process-program (cdr prgm))
    );bottom when
)


(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               (read-program program)
               (process-program program)
        )
    )
)

(main (vector->list (current-command-line-arguments)))

