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

(define (get-label key)
   (hash-ref *label-table* key))
(define (get-function key)
   (hash-ref *function-table* key))
(define (get-variable key)
   (hash-ref *variable-table* key))

;;when want to add to these guys, i dont know how to make a fucntion for it. Need to just say like
;; (hash-set! *-table* key value)


(define *stderr* (current-error-port))
;;printing out to stderr

(define *run-file*
    (let-values
        (((dirpath basepath root?)
            (split-path (find-system-path 'run-file))))
        (path->string basepath))
)
;;complicated way of saying whats the name of the file im running, so if u invoke the prgm bad, it'll print an err


;; gonna be like:   die var  
;; iterate over items in list, and call 'display' on them
(define (die list)
    (for-each (lambda (item) (display item *stderr*)) list)
    (newline *stderr*)
    (exit 1)
)

;; its fine to have a function that has parenthesis in the name
(define (usage-exit)
    (die `("Usage: " ,*run-file* " filename"))
)

;; let is similar to a define, define lasts longer in the context, let is only in the rest of the scope
;;
(define (readlist-from-inputfile filename)
    (let ((inputfile (open-input-file filename)))
         (if (not (input-port? inputfile))
;;the apostrophe says treat me like the thing that i am, strings evaluate like strings, like in this case, its the actual character parenthesis
             (die `(,*run-file* ": " ,filename ": open failed"))
             (let ((program (read inputfile)))
                  (close-input-port inputfile)
                         program))))

;;this takes in 2 parameters, filename and program.
;;%d, %a in printf is like ~d ~a in scheme. ~n == newline
(define (write-program-by-line filename program)
    (printf "==================================================~n")
    (printf "~a: ~s~n" *run-file* filename)
    (printf "==================================================~n")
    (printf "(~n")
;; map function! important, it runs the given function on each of the variables in the given list
;; lambda is the definition of a function
;; each element in this program were gonna call a line, then we're gonna print it.
    (map (lambda (line) (printf "~s~n" line)) program)
    (printf ")~n"))

(define (read-program prgm)
            (when (> (length (car prgm)) 1);not (null? (cdr(car prgm)))
            (when not (null? (cdr prgm))
            (cond
              [(eqv? (caadr (car prgm)) 'goto)
                 (put-function! (caadr(car prgm)) (cadr(cadr(car prgm))))]
              [(eqv? (caadr (car prgm)) 'print)
                 (put-function! (caadr(car prgm)) (cadr(cadr (car prgm))))]
                 ;(display (get-function (caadr(car prgm))))]
              [(eqv? (caadr (car prgm)) 'input)
                 (put-function! (caadr(car prgm)) (cadr(cdr(car prgm))))]
                 ;(display (get-function (caadr(car prgm))))]
              [(symbol?(caadr (car prgm)))
                 (put-label! (caadr(car prgm)) (cadr(cadr(car prgm))))]
                 ;(display (get-label (caadr(car prgm))))]
                 ;check whats to the right of this label
                 ;put whole cond in here?
                 ;example:
                 ;zero: print "zero"
            )));end cond/when
      (when not (null? (cdr prgm))
      (when (> (length (cdr prgm)) 0)
         (read-program (cdr prgm))
      ))
)


(define (process-program prgm)
   (when (> (length (car prgm)) 1);not (null? (cdr(car prgm)))
   (when not (null? (cdr prgm)) 
        (cond 
           (eqv? (caadr (car prgm)) 'dim)
              (handle-dim (car prgm))
           (eqv? (caadr (car prgm)) 'let)
              (handle-let prgm)
           (eqv? (caadr (car prgm)) 'goto)
              (handle-goto prgm)
           (eqv? (caadr (car prgm)) 'if)
              (handle-if prgm)
           (eqv? (caadr (car prgm)) 'print)
              (handle-print prgm)
           (eqv? (caadr (car prgm)) 'input)
              (handle-input prgm)

         )
    ));;end cond/when
    (when not (null? (cdr prgm))
    (when (> (length (cdr prgm)) 0)
        (process-program (cdr prgm))
    ))
)

;; is arglist null or is 2nd element in arglist null if not then do this if so print usage exit
(define (main arglist)
    (if (or (null? arglist) (not (null? (cdr arglist))))
        (usage-exit)
;;ive got a silly basic program file.
;;let wont let you use things youre defining in this let, in this let
;;let* is a little different, gives you a list of things to define, and some of the things in my list might refer to previous things in the list
;;he defines sbprogfile and then uses that definition
        (let* ((sbprogfile (car arglist))
               (program (readlist-from-inputfile sbprogfile)))
;               (write-program-by-line sbprogfile program)
               (read-program program)
               ));(process-program program)))
)

;; runs main with arguments passed in, turns from a vector to a list
(main (vector->list (current-command-line-arguments)))
;; current-command-line-arguments is a keyword.

;;a dim looks like: 3  dim a(100)
;;where 100 is size and a is name, make sure you pass it in like this to handle-dim
(define (handle-dim line)
    ;check these cadadarads!!!
    (define name (car(cdr(cdr line))))
    (define size (cdr(cdr(cdr line))))
    (define *vector* (make-vector size))
    (put-variable name *vector)
)

;a let looks like: 3  let i=1
;or              : 3  let a(10)=1
(define (handle-let line)
    (define name (car(cdr(cdr line))))
    
    ;if not array
    (define value (cdr(cdr(cdr(cdr line)))))
    (put-variable name value)

    ;if array
    (define value (cdr(cdr(cdr(cdr(cdr line))))))
    (define index (car(cdr(cdr(cdr line)))))
    (vector-set! (get-variable name) index value);maybe 'value
)

;looks like:  3  goto loop
(define (handle-goto line)
    (define label (cdr(cdr line)))
    ;returns eveything after the label's spot in the list
    (list-tail ( (get-label label) label))
)

(define (handle-if line)
    
)

(define (handle-print line)
   (display (car(cdr(cdr line))))
)

(define (handle-input line)
    
)

;TODO
;finish the handle functions
;fix up read-program to handle labels
;expression table, do math
