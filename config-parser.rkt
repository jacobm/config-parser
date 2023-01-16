#lang racket

(require data/applicative
         data/functor
         data/monad
         megaparsack
         megaparsack/text)

(struct section (name properties) #:transparent)

(define ws/p (hidden/p (do (many/p (satisfy/p char-blank?)) void/p)))
(define newline/p (hidden/p (or/p (do (char/p #\return) (char/p #\newline)) (char/p #\newline))))
(define blank-line/p (hidden/p (do ws/p newline/p void/p)))

; https://docs.racket-lang.org/megaparsack/reference.html#%28def._%28%28lib._megaparsack%2Fmain..rkt%29._lookahead%2Fp%29%29
(define end-of-line/p (lookahead/p (or/p (char/p #\newline) eof/p)))

(define rest-of-line/p
  (or/p (do end-of-line/p (pure ""))
        (do [c <- any-char/p] [cs <- rest-of-line/p] (pure (string-append (string c) cs)))))

(define parse-comment/p (do (char/p #\;) rest-of-line/p))

(define comment-or-blank-lines/p
  (hidden/p (do (many/p (or/p (try/p blank-line/p) (try/p (do parse-comment/p newline/p)))) void/p)))

(define identifier/p (map (compose1 string->symbol list->string) (many+/p (or/p letter/p digit/p))))

(define parse-header/p
  (do (char/p #\[)
      ws/p
      [header-name <- (label/p "header name" identifier/p)]
      ws/p
      (char/p #\])
      (do ws/p (or/p parse-comment/p void/p) void/p)
      (pure header-name)))

(define parse-property/p
  (do [prop-name <- (label/p "property name" identifier/p)]
      ws/p
      (char/p #\=)
      ws/p
      [prop-value <- (label/p "property value" identifier/p)]
      (do ws/p (or/p parse-comment/p void/p) void/p)
      (pure `(,prop-name . ,prop-value))))

(define parse-properties/p
  (do
   [properties
    <-
    (many/p
     (or/p
      (try/p
       (do ws/p [property <- parse-property/p] ws/p (or/p (try/p newline/p) void/p) (pure property)))
      (try/p blank-line/p)
      (try/p (do ws/p parse-comment/p (or/p (try/p newline/p) void/p) void/p))))]
   (pure (filter (compose1 not void?) properties))))

(define parse-section/p
  (do comment-or-blank-lines/p
      [header <- (do ws/p [header <- parse-header/p] ws/p (pure header))]
      comment-or-blank-lines/p
      [properties <- parse-properties/p]
      (pure (section header (filter (compose1 not void?) properties)))))

(define parse-ini/p
  (do [sections <- (many/p parse-section/p)]
      (or/p (try/p comment-or-blank-lines/p) ws/p)
      eof/p
      (pure sections)))

(provide section
         ws/p
         newline/p
         comment-or-blank-lines/p
         parse-header/p
         parse-property/p
         parse-section/p
         parse-properties/p
         parse-ini/p)
