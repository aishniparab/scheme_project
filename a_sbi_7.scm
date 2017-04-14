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


(define (expr-list-len1 stmt)
         (if (number? (car stmt))
            (+ (car stmt) 0.0);return the number
            ;else
            (get-variable (car stmt));return val of var
         )
)

;everything here returns void??? -ask this to TA
(define (expr-list-len2 stmt) 

       (display "in list-len2") (display "stmt is") (display stmt)
       (cond
            [(eqv? (car stmt) 'sqrt)
                (sqrt (cadr stmt))]
            [(eqv? (car stmt) 'log)
             ;stmt is log10
	       ;(display "**cadr of stmt is**")
               ;(display (cadr stmt)) ;(log10)
               ;(display (car stmt)) ;10
              ; (display "cdr is ")(display (cdr stmt)) ;log(10)
              ; (display "car(cdr) is")(display (car(cdr stmt)))
               (log (cadr stmt))] ;;cadr of stmt is 10
            [(eqv? (car stmt) 'floor)
                (floor (cadr stmt))]
            [(eqv? (car stmt) 'ceil)
                (ceiling (cadr stmt))]
            [(eqv? (car stmt) 'round)
                (round (cadr stmt))]
            [(eqv? (car stmt) 'abs)
                (abs (cadr stmt))]
            [(eqv? (car stmt) 'atan)
                (atan (cadr stmt))]
            [(eqv? (car stmt) 'asin)
                (asin (cadr stmt))]
            [(eqv? (car stmt) 'acos)
                (acos (cadr stmt))]
            [(eqv? (car stmt) 'tan)
                (tan (cadr stmt))]
            [(eqv? (car stmt) 'cos)
                (cos (cadr stmt))]
            [(eqv? (car stmt) 'sin)
                (sin (cadr stmt))]
            [(eqv? (car stmt) '+)
                (abs(cadr stmt))]
            [(eqv? (car stmt) '-)
                (* (abs(cadr stmt)) -1)]
            [else ;its an array
               (define name (get-variable (car stmt)))
               (define index (handle-expr (cdr stmt)))
               (vector-ref name index)]
        )
)

(define (expr-list-len3 stmt)
   ;(display "**in expr-list-len3**")
   ;(display "stmt is ") (display stmt)

    (define var (car stmt))
         (cond
            [(eqv? var '+)
               ;changed all the right exprs  from cddr to caddr
               (+ (handle-expr (cadr stmt)) (handle-expr (caddr stmt)))]
            [(eqv? var '*)
               (* (handle-expr (cadr stmt)) (handle-expr (caddr stmt)))]
            [(eqv? var '-)
               (- (handle-expr (cadr stmt)) (handle-expr (caddr stmt)))]
            [(eqv? var '/)
               (if (=(handle-expr(caddr stmt)) 0.0)
                   "+nan.0"
                   (/ (handle-expr (cadr stmt)) 
                      (handle-expr (caddr stmt)))
               )
            ]
            [(eqv? var '%)
               (- (handle-expr (cadr stmt)) (* (truncate (/ 
               (handle-expr (cadr stmt)) (handle-expr (caddr stmt))))
               (handle-expr (caddr stmt))))]
            [(eqv? var 'sqrt)
               (sqrt (handle-expr (cdr stmt)))]
            [(eqv? var '^)
               (expt (handle-expr (cadr stmt)) 
               (handle-expr (caddr stmt)))]
            [(eqv? var 'log)
               (log (handle-expr (cdr stmt)))]
            [(eqv? var 'floor)
               (floor (handle-expr (cdr stmt)))]
            [(eqv? var 'ceil)
               (ceiling (handle-expr (cdr stmt)))]
            [(eqv? var 'round)
               (round (handle-expr (cdr stmt)))]
            [(eqv? var 'abs)
               (abs (handle-expr (cdr stmt)))]
            [(eqv? var 'atan)
               (atan (handle-expr (cdr stmt)))]
            [(eqv? var 'asin)
               (asin (handle-expr (cdr stmt)))]
            [(eqv? var 'acos)
               (acos (handle-expr (cdr stmt)))]
            [(eqv? var 'tan)
               (tan (handle-expr (cdr stmt)))]
            [(eqv? var 'cos)
               (cos (handle-expr (cdr stmt)))]
            [(eqv? var 'sin)
               (sin (handle-expr (cdr stmt)))]
        )
)


(define (expr-list stmt)
;(display "in expr-list")
      (when (= (length stmt) 1)
         (expr-list-len1 stmt)
      )
      (when (= (length stmt) 2)
          (expr-list-len2 stmt)
      )
      (when (> (length stmt) 2)
          (expr-list-len3 stmt)
      )
)

;this is like duct taping the program together,
; but ya gotta do what ya gotta do
(define (expr-printvar stmt)
            (if (not (get-variable stmt))
                ""
                (get-variable stmt)
            )
)

(define (expr-unlist stmt)
   ;(display "in un-list")   
         (if (number? stmt)
         (+ stmt 0.0);return the number
         ;else
         (expr-printvar stmt)
      )
)

(define (handle-expr stmt)
   (if(list? stmt)
      (expr-list stmt)
      (expr-unlist stmt)
   )
)

(define (handle-dim line)
    (define name (car(cdr line)))
    (define size (inexact->exact(
         handle-expr (car(cdr(car(cdr line)))))))
    ;(define size (car(cdr(car(cdr line)))))
    (define *vector* (make-vector size))
    (put-variable! name *vector*)
)

(define (let-array line)
       (define name (car (cadr line)))
       (define index (handle-expr (cdr (cadr line))))
       (define value (handle-expr (cdr(cdr line))))
       (put-array! (get-variable name) index value)
)

(define (let-unarray line)
       (define var-name (car(cdr line)))
       (define value (handle-expr (cddr line)))
       (put-variable! var-name value)
)

(define (handle-let line)
    ;when array
    ;(display line)
    ;(let pi (* 4 (atan 1)))
    (if (symbol?(cadr line))
    ;(when (> (length (cadr line)) 1)
    ;   (define name (car (cadr line)))
    ;   (define index (handle-expr (cdr (cadr line))))
    ;   (define value (handle-expr (cdr(cdr line))))
    ;   (put-array! (get-variable name) index value)
       (let-unarray line)
       (let-array line)
    )
    ;when not an array
    ;(when not (> (length (cadr line)) 1)
    ;   (define var-name (car(cdr line)))
    ;   (define value (handle-expr (cddr line)))
    ;   (put-variable! var-name value)
    ;)
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
  ;(display "**in-pntopt args** and args is**")
  ;(display args)
  (when (string? (car args))
     (display (car args))
  )
  (when not(string?(car args))
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
   ) (display "\n") ;;added this to print line by line
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
   ;for some reason, theres a #f at the end of prgm here
   ;i think its because its actually going into handle-goto,
   ; but thats putting a false on the end for some reason,
   ; probably having to do with the
   ;handle-exprs thing similarly
   (when (> (length (car prgm)) 1);skip blank lines
   (when not (null? (cdr prgm)) 
        ;check if theres a label in the line
        ;, and send only the operation to the sub function
        (when (symbol? (cadr (car prgm)))
           ;added a car to this an the one below
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
               (put-variable! 
            'pi 3.141592653589793238462643383279502884197169399)
               (put-variable! 
            'e 2.718281828459045235360287471352662497757247093)
               (put-variable! 'inputcount 0)
               (read-program program)
               (process-program program)
               (put-variable! 'inputcount -1)
        )
    )
)

(main (vector->list (current-command-line-arguments)))




