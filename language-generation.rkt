#lang racket
(require csc151)
(require csc151/rex)
(require rackunit)

;;; (add-successor! successor-table predecessor successor) -> void?
;;;  successor-table: hash? (keys are strings, values are lists of strings)
;;;  predecessor : string?
;;;  successor : string?
;;; Add successor to the list of successors of predecessor.  If
;;; predecessor currently has no successors, creates a new list.
(define add-successor!
  (lambda (successor-table predecessor successor)
    (if (hash-has-key? successor-table predecessor)
        (hash-set! successor-table predecessor (flatten (list (hash-ref successor-table predecessor) successor)))
        (hash-set! successor-table predecessor (flatten (list successor))))))



;;; (get-successors successor-table predecessor) -> listof string?
;;;   successor-table : hash? (keys are strings, values are lists of strings)
;;;   predecessor : string?
;;; Look up all of the successors of predecessors which have been
;;; stored by add-successor.  If there are no such values, returns
;;; the empty list.
(define get-successors
  (lambda (successor-table predecessor)
    (cond [(not (hash-has-key? successor-table predecessor))
           '()]
          [else
           (hash-ref successor-table predecessor)])))

;STAGE ONE TESTING________________________________________________________
(require rackunit)
(define test-table (make-hash))

#|(test-equal? "No successors in initial table."
             (get-successors test-table " upon")
             '())

(add-successor! test-table "once upon" "a")
(test-equal? "Our first successor"
             (get-successors test-table "once upon")
             '("a"))

(add-successor! test-table "once upon" "a")
(test-equal? "The same successor again"
             (get-successors test-table "once upon")
             '("a" "a"))

(test-equal? "No successors for a different predecessor"
             (get-successors test-table "upon a")
             '())

(add-successor! test-table "upon a" "time")
(add-successor! test-table "upon a" "mattress")
(add-successor! test-table "upon a" "time")
(test-equal? "Different successors"
             (get-successors test-table "upon a")
             '("time" "mattress" "time"))|#

; STAGE 2____________________________________________________________________

;;; (extract-words str) ->list?
;;; str : string?
;;; Extract all of the words from str. 
(define extract-words
  (let ([rex-word
         (rex-repeat (rex-any-of (rex-char-range #\a #\z)
                                 (rex-char-range #\A #\Z)
                                 (rex-string "'")
                                 (rex-string "-")))])
    (lambda (str)
      (rex-find-matches rex-word str))))

;;; (add-text-table/helper successor-table lst) -> hash?
;;; successor-table : hash?
;;; lst : list-string?
;;; Helper function add-text-to-table
(define add-text-table/helper
  (lambda (successor-table lst)
    (if (null? (cddr lst))
        void
        (and (add-successor! successor-table
                             (string-append (car lst) " " (cadr lst))
                             (caddr lst))
             (add-text-table/helper successor-table (cdr lst))))))

;;; (add-text-to-table! successor-table fname) -> hash?
;;; succcessor-table:hash?
;;; fname:string?
;;;Takes a file, and tables all the successors
(define add-text-to-table!
  (lambda (successor-table fname)
    (add-text-table/helper successor-table (extract-words (file->string fname)))))

;STAGE THREE______________________________________________________________

;;; (add-word! count-table word) -> hash?
;;; count-table : hash?
;;; word:string?
;;; Adds the word to a given table
(define add-word!
  (lambda (count-table word)
    (hash-set! count-table word (+ (hash-ref count-table word 0) 1))
    count-table))

;;; (add-words! count-table words) -> hash?
;;; count-table : hash?
;;; word:list?
;;; Adds count asscociated with a word on a table
(define add-words!
  (lambda (count-table words)
    (when (not (null? words))
      (add-word! count-table (car words))
      (add-words! count-table (cdr words)))
    count-table))

;;; (counts->probabilities counts) -> hash?
;;; counts : hash?
;;; Turns a hash table of counts to probabilities.
(define counts->probabilities
  (lambda (counts)
    (let ([total (apply + (hash-values counts))])
      (make-hash (map cons (hash-keys counts)
                      (map / (hash-values counts) (make-list (length (hash-values counts)) total)))))))

;;; (dedup lst) -> list?
;;; lst : list?
;;; Removes duplicate elements from a list recursivley
(define (dedup lst)
  (cond
    [(null? lst)
     null]
    [else (cons (list-ref lst 0)
                (dedup (filter (lambda (x) (not (equal? (list-ref lst 0) x)))
                               lst)))]))

;;; (words->probabilites words) -> hash?
;;; words : list of string?
;;; takes a list of words as input and returns a hash table that gives
;;; the probability of each word appearing
(define words->probabilities
  (lambda (words)
    (let ([local-table (make-hash)])
      (add-words! local-table words)
      (make-hash (map cons (dedup words)
                      (hash-values (counts->probabilities local-table)))))))

; STAGE FOOUR______________________________________________________________

;;; (min n lst) -> integer?
;;; sm : integer?
;;; lst : list?
;;; Finds the minimum nuber from a list of numbers
(define min
  (lambda (n lst)
    (if (null? lst)
        n
        (if (< (car lst) n)
            (min (car lst) (cdr lst))
            (min n (cdr lst))))))

;;; (probabilistic-select/helper prob-lst word-lst) -> list?
;;; prob-lst : list?
;;; word-lst : list?
;;; Helper procedure for probabilistic-select
(define probabilistic-select/helper
  (lambda (prob-lst word-lst)
    (if (null? prob-lst)
        null 
        (append (make-list (* (denominator (min 1 prob-lst)) (car prob-lst)) (car word-lst))
                (probabilistic-select/helper (cdr prob-lst) (cdr word-lst))))))

;;; (probabilistic-select probability-table) -> string?
;;; probability-table : hash?
;;; Randomly selects a word from the probability table
(define probabilistic-select
  (lambda (probability-table)
    (let* ([prob-lst (hash-values probability-table)]
           [word-lst (hash-keys probability-table)]
           [rand-# (random (length (probabilistic-select/helper prob-lst word-lst)))])
      (list-ref (probabilistic-select/helper prob-lst word-lst) rand-#))))
          
; STAGE 5________________________________________________________________

;;; (make-text-table successor-table) -> hash?
;;; successor-table : hash?
;;; Takes a table of successor lists created in stage two and returns a new table
(define make-text-table
  (lambda (successor-table)
    (make-hash (map cons
                    (hash-keys successor-table)
                    (map words->probabilities (hash-values successor-table))))))

; STAGE SIX_____________________________________________________________

;;; (generate-text/helpertext-table word1 word2 n) -> list of strings?
;;; text-table : hash?
;;; word1 : string?
;;; word2 : string?
;;; n : integer? (at least 2)
;;; Helper for (generate-text)
(define generate-text/helper
  (lambda (text-table word1 word2 n)
    (if (index-of (hash-keys text-table) (string-append word1 " " word2))
        (let ([word3 (probabilistic-select (hash-ref text-table (string-append word1 " " word2)))])
          (if (zero? (- n 2))
              null
              (cons word3
                    (generate-text/helper text-table word2 word3 (- n 1)))))
        null)))

;;; (generate-text text-table word1 word2 n) -> list of strings?
;;; text-table : hash?
;;; word1 : string?
;;; word2 : string?
;;; n : integer? (at least 2)
;;; Picks words from the Probability table in random according to the input text
(define generate-text
  (lambda (text-table word1 word2 n)
    (if (zero? n)
        null
        (append (list word1) (list word2) (generate-text/helper text-table word1 word2 n)))))

; STAGE SEVEN__________________________________________________________________________________________
(define stage-7-example (make-hash))
(add-text-to-table! stage-7-example "sample.txt")
(string-join (generate-text (make-text-table stage-7-example) "heart" "warmed" 200))
#|"heart warmed How good it is posted with permission of the place where you are redistributing or providing access
 to or distributing this work E Do not unlink or detach or remove the full Project Gutenberg-tm name associated with
 the trademark license including paying royalties for use of anyone anywhere in the official Project Gutenberg-tm work
 in a constant state of change If you do not stop asking questions Yes little Paul Yes replied the light silvery voice
 of the works from print editions not protected by U S copyright law Redistribution is subject to the poor are so tired
 from the person or entity to whom you paid the fee as set forth in paragraphs E or obtain permission for the use of
and all other people who do not ask the question like a big wall and this is not at all easy because the poor are so
 tired from the day s work that they can hardly think and the workers That is why they punish every one who asks Why
You have heard all the tree but all the languages in the collection are in a constant state of change If you paid the
 fee as"|#
