#lang scheme

; I used lists for some procedures so, after every calculation,
; make sure running the program again to reset the lists.
; Otherwise, the results will be wrong. I searched a lot but, couldn't find
; an answer for deleting lists after use.

; You can make every conversion by typing (main base1->base2 n).
; The main method takes bases and number as arguments. For example,(main dec->oct 8).

; You can use numbers for n for every conversion except hexadecimal conversions.
; If you are trying to convert from hexadecimal to any base,
; you should type your number between quotation marks, i.e. "6B".
; Language allows us to enter only one type of data as "one" argument. Since we cannot convert
; string to number, we type numbers as string between quotation marks.
; Procedures handle the string/integer conversion.
; For all other conversions, including any base to hexadecimal conversion,
; you can type just the number.
; Note that, for the same reason, if the result is in base 16,
; it will be printed as string to the console.

; Example inputs and results,
; dec->bin 5 = 101 / bin->dec 101 = 5
; dec->oct 9 = 11 / oct->dec 11 = 9
; dec->hex 26 = "1A" / hex->dec "1A" = 26
; bin->oct 1001 = 11 / oct->bin 11 = 1001
; bin->hex 11010 = "1A" / hex->bin "1A" = 11010;
; oct->hex 32 = "1A" / hex->oct "1A" = 32
; Don't forget to run for every calculation to reset the lists.
; I tried so hard to reset the lists but cannot find any answer for that operation.


(define (dec->bin n)
  (cond [(zero? n) 0]
        [else (+ (* 10 (dec->bin (quotient n 2))) (remainder n 2))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (dec->oct n)
  (cond [(zero? n) 0]
        [else (+ (* 10 (dec->oct (quotient n 8))) (remainder n 8))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (add lst e) 
   (if (null? lst) 
       (list e)
       (cons (car lst)
             (add (cdr lst) e))))

(define lst1 '())

(define (dec->hex n)
  (cond [(zero? n) 0]
        [else (set! lst1 (add lst1 (if (= (remainder n 16) 10)
                                     "A"
                                     (if (= (remainder n 16) 11)
                                         "B"
                                         (if (= (remainder n 16) 12)
                                             "C"
                                             (if (= (remainder n 16) 13)
                                                 "D"
                                                 (if (= (remainder n 16) 14)
                                                     "E"
                                                     (if (= (remainder n 16) 15)
                                                         "F"
                                                         (number->string (remainder n 16))))))))))
              (dec->hex (quotient n 16))])

  (string-join (reverse lst1) ""))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bin->dec n)
  (cond [(zero? n) 0]
        [else (+ (* 2 (bin->dec (quotient n 10))) (remainder n 10))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (oct->dec n)
  (cond [(zero? n) 0]
        [else (+ (* 8 (oct->dec (quotient n 10))) (remainder n 10))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (replace lst new old)
   (cond
         ((list? lst)
          (map
              (lambda (lstt) (replace lstt new old))
              lst))
         (else
              (if (string=? lst old)
                   new
                   lst))))

(define (multiplier lst size)
  (if (null? lst)
      '()
      (cons (* (car lst) (expt 16 (- size 1)))
            (multiplier (cdr lst) (- size 1)))))

(define (sum lst)
  (if
    (null? lst)
    0
    (+ (car lst) (sum (cdr lst)))))

(define (hex->dec n)
  (define lst2 (map string (string->list n)))
  (define lst3 (replace lst2 "10" "A"))
  (define lst4 (replace lst3 "11" "B"))
  (define lst5 (replace lst4 "12" "C"))
  (define lst6 (replace lst5 "13" "D"))
  (define lst7 (replace lst6 "14" "E"))
  (define lst8 (replace lst7 "15" "F"))
  (define lst9 (map string->number lst8))
  (define size (length lst9))
  (define lst10 (multiplier lst9 size))
  (sum lst10))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bin->oct n)
  (cond [(zero? n) 0]
        [else (dec->oct (bin->dec n))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (oct->bin n)
  (cond [(zero? n) 0]
        [else (dec->bin (oct->dec n))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (hex->bin n)
  (cond [(number? n) n]
        [else (dec->bin (hex->dec n))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (hex->oct n)
  (cond [(number? n) n]
        [else (dec->oct (hex->dec n))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (bin->hex n)
  (cond [(zero? n) 0]
        [else (dec->hex (bin->dec n))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (oct->hex n)
  (cond [(zero? n) 0]
        [else (dec->hex (oct->dec n))]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (main base1->base2 num)
  (base1->base2 num))
