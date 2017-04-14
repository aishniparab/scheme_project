#!/afs/cats.ucsc.edu/courses/cmps112-wm/usr/racket/bin/mzscheme -qr
;; $Id: sbi.scm,v 1.3 2016-09-23 18:23:20-07 - - $
;;
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

(define (put-array! name index value)
   (vector-set! (get-variable name) index value) 
)

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
                     (e       2.718281828459045235360287471352662497757247093)
                     (pi      3.141592653589793238462643383279502884197169399)

                   )
)

(for-each
                  (lambda (pair)
                      (put-function! (car pair) (cadr pair)))
                  `(
                      (log10_2 0.301029995663981195213738894724493026768189881)
                      (sqrt_2  1.414213562373095048801688724209698078569671875)
                      (e       2.718281828459045235360287471352662497757247093)
                      (pi      3.141592653589793238462643383279502884197169399)
                      (/ ,/) (abs ,abs)
                      (<= ,<=) (>= , >=) (= ,=) (> ,>) (< , <) (sin ,sin) (cos ,cos) 
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
        (vector-ref (get-variable (car expr)) (handle-expr(cadr expr)))
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

    [(list? expr) 
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
 
         (else (die "I don't want to die."))        
       ))
     (cond
       [(number? (car expr)) 
         (car expr)]
        (else (die "I'm dead."))
     ;end latest cond
     )
     ) ;ends cond and let and if
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
       (define name (car (cadr line))) 
       (define index (handle-expr (cdr (cadr line)))) 
       (define value (handle-expr (cdr(cdr line))))
       (put-array! name index value)      
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
   (define l-expr (handle-expr (cadr relop-expr)))
   (define r-expr (handle-expr (cddr relop-expr)))
   (cond
       [(eqv? relop '=)
         (= l-expr r-expr)]
       [(eqv? relop '<)
         (< l-expr r-expr)]
       [(eqv? relop '>)
         (> l-expr r-expr)]
       [(eqv? relop '<>)
         (<> l-expr r-expr)]
       [(eqv? relop '>=)
         (>= l-expr r-expr)]
       [(eqv? relop '<=)
         (<= l-expr r-expr)]
   )
)

(define (handle-if line)
    (when (handle-relop (cadr line))
       (handle-goto (cddr line))
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
;print 5     (print 5)
(define (handle-print line)
   (if (null? (cdr line))
      (display "\n")
      ;else, get rid of () 
      (when not (=(length(cdr line))0)
         (print-opt (cdr line))
      ) 
   ) (display "\n") ;;this to print line by line
)

(define (handle-input line)
    (put-variable! 'inputcount (+(get-variable 'inputcount) 1))
    (define value (read))
    (put-variable! (cdr line) (+ value 0.0))
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
   (define operator (car line))
   (cond
           [(eqv? operator 'dim)
              (handle-dim line)]
           [(eqv? operator 'let)
              (handle-let line)]
           [(eqv? operator 'goto)
              (handle-goto line)] ;(cadr line))]
           [(eqv? operator 'if)
              (handle-if line)]
           [(eqv? operator 'print)
              (handle-print line)]
           [(eqv? operator 'input)
              (handle-input line)]
         )
)

(define (process-program prgm)
   (when (> (length (car prgm)) 1);skip blank lines
   (when not (null? (cdr prgm)) 
        ;check if theres a label in the line
        ; and send only the operation to the sub function
        (when (symbol? (cadr (car prgm)))
           (process-opt (car(cddr(car prgm))))
        )
        (when not (symbol? (cadr (car prgm)))
           (process-opt (car(cdr(car prgm))))
        )
    ));;end when
    (when not (null? (cdr prgm))
    (when (> (length (cdr prgm)) 0)
        (process-program (cdr prgm))
    ))
)

(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
               ;loading up tables
               (put-variable! 'inputcount 0)
               (read-program program)
               (process-program program)
               (put-variable! 'inputcount -1)
        )
    )
)

(main (vector->list (current-command-line-arguments)))




