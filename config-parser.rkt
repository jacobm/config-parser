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
(define blank-lines/p (hidden/p (do (many/p (try/p blank-line/p)) void/p)))

(define identifier/p (map (compose1 string->symbol list->string) (many+/p (or/p letter/p digit/p))))

(define parse-header/p
  (do (char/p #\[)
      ws/p
      [header-name <- (label/p "header name" identifier/p)]
      ws/p
      (char/p #\])
      (pure header-name)))

(define parse-property/p
  (do [prop-name <- (label/p "property name" identifier/p)]
      ws/p
      (char/p #\=)
      ws/p
      [prop-value <- (label/p "property value" identifier/p)]
      (pure `(,prop-name . ,prop-value))))

(define parse-properties/p
  (do [properties
       <-
       (many/p
        (or/p (do (noncommittal/p ws/p) [property <- parse-property/p] ws/p (pure property)) ws/p)
        #:sep (char/p #\newline))]
      (pure (filter (compose1 not void?) properties))))

(define parse-section/p
  (do blank-lines/p
      [header <- (do ws/p [header <- parse-header/p] ws/p (pure header))]
      blank-lines/p
      [properties <- parse-properties/p]
      (pure (section header (filter (compose1 not void?) properties)))))

(define parse-ini/p (do [sections <- (many/p parse-section/p)] blank-lines/p eof/p (pure sections)))

(provide section
         ws/p
         newline/p
         blank-lines/p
         parse-header/p
         parse-property/p
         parse-section/p
         parse-ini/p)
