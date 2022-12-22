#lang racket

(require data/applicative
         data/functor
         data/monad
         megaparsack
         megaparsack/text)

(define whitespace/p (label/p "whitespace" (satisfy/p char-blank?)))
(define ws/p (label/p "whitespaces" (many/p whitespace/p)))
(define newline/p
  (label/p "newline" (or/p (do (char/p #\return) (char/p #\newline)) (char/p #\newline))))
(define blank-or-newlines/p (label/p "blank or newlines" (many/p (or/p newline/p ws/p))))

(define identifier/p (map (compose1 string->symbol list->string) (many+/p (or/p letter/p digit/p))))

(define parse-header/p
  (do ws/p
      (char/p #\[)
      ws/p
      [header-name <- (label/p "header name" identifier/p)]
      ws/p
      (char/p #\])
      ws/p
      newline/p
      (pure header-name)))

(define parse-property/p
  (do ws/p
      [prop-name <- (label/p "property name" identifier/p)]
      ws/p
      (char/p #\=)
      ws/p
      [prop-value <- (label/p "property value" identifier/p)]
      ws/p
      newline/p
      (pure `(,prop-name . ,prop-value))))

(provide ws/p
         newline/p
         parse-header/p
         parse-property/p)
