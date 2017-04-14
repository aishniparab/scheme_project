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

;;expecting a procedure that can be applied to arguments?
(define (read-program prgm)
   (if not (null? (prgm))
      (lambda(list) (
         cond(
           (eqv? (cadr (list)) "goto")
              (display "goto")
           (eqv? (cadr (list)) "print")
              (display "print")
           (eqv? (cadr (list)) "input")
              (display "input")
           (symbol? (cadr (list)))
              (display "label")
         )
      )
         (car(prgm))
      )
   )
   (read-program cdr(prgm))
)

;;old read program - before we found the glory of lambda functions
;(define (read-program prgm)
;    (if (not (null? (cdr(prgm)))))
;        (let* (list (car prgm))
;           (cond((eqv? (cadr (list)) "goto")
;              (display "goto")
;              (put-function (cadr(list)) (cddr(list))))
;           ((eqv? (cadr (list)) "print")
;              (display "print")
;              (put-function (cadr(list)) (cddr(list))))
;           ((eqv? (cadr (list)) "input")
;              (display "input")
;              (put-function (cadr(list)) (cddr(list))))
;           ((symbol? (cadr (list)))
;              (display "label")
;              (put-label (cadr(list) prgm))));;So, we want to set the rest of the program as the value here, but this includes the label, is that gonna bork it?
           ;;((pair? (cadr (list)))
;);end cond
        
;        (read-program cdr(prgm))
;)

(define (process-program prgm)
    (if (not (null? (cdr(prgm))))
        (cond ((eqv? ((car(prgm)) "goto"))
           (handle-goto prgm)
        (eqv? ((car(prgm)) "if"))
           (handle-if prgm)
        (eqv? ((car(prgm)) "print"))
           (handle-print prgm)
        (eqv? ((car(prgm)) ""))

    );;end cond/if
    (process-program cdr(prgm))
    )))

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
               (write-program-by-line sbprogfile program)
               (read-program program)
               ));(process-program program)))
)

;; runs main with arguments passed in, turns from a vector to a list
(main (vector->list (current-command-line-arguments)))
;; current-command-line-arguments is a keyword.

;;loop through the whole program once, do this in the main method probably, and just look at each line. Store any labels in the label table (store the entirety of the rest of the program in the hash)
;;Then start actually processing the code from the beginning, if you get a goto, look it up in the hash, and start processing from the data given in in the hash
;;Don't do anyhting with the function table untill you start processing the code, then add any function you've come across to it
;;Same with the variable table, just update it as you go, i think

;;need to create functions for each of the different commands we need to process
(define (handle-goto label)
   ()
)

(define (handle-if . exprs)
   () 
)

(define (handle-print . printables)
;;forgot how to use for-each
   (for-each printable printables
       (display printable))
)

(define (handle-input mem-item)
   ()
)

(define (handle-let var)
   ()
)

(define (handle-dim dimarg)
   (vector-set! dimarg)
)

;;LOOK AT THE EXAMPLE CODE, THERE IS LITEREALLY AN EXPRESSION SIMPLIFIER IN THE EXAMPLES, CAN JUST TAKE SOME OF THAT CODE ROUN'DERE
;;TAKE SUMMA UR CODE N' TRY IT ON MZSCHEME
;;program is just a list, '( 1 2 3 ) for example lists always start with quasiquote '
;;'( '(1 firstline (print "hello')))
;;use (eqv? 'print' cadr(program) or something like that to see if its a print
;;the function table is static. (define fntbl (make-hash) hash-set! then add a lambda fucntion that adds a function to the hash map. lambda function is basically a pointer to the function
;;i dont think this table changes, its filled out as you run through the first pass of the program
;;variable funciton is dynamic, as you see dim and let, you know youre going to be adding/referencing stuff to the table.
;;the lines are lists too, the car of every line is the line we're in. the caar is the first element of the first list
;;in the label table, store the rest of the list from that line
