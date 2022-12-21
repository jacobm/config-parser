#lang racket

(require data/applicative
         data/functor
         data/monad
         megaparsack
         megaparsack/text)

(define identifier/p (map (compose1 string->symbol list->string) (many/p (or/p letter/p digit/p))))

(define whitespace/p (label/p "whitespace" (satisfy/p char-blank?)))
(define ws/p (label/p "whitespaces" (many/p whitespace/p)))

(define newline/p (label/p "newlines" (many+/p (or/p (char/p #\return) (char/p #\newline)))))

(define parse-header/p
  (do (char/p #\[) ws/p [header-name <- identifier/p] ws/p (char/p #\]) (pure header-name)))

(define parse-property/p
  (do ws/p
      [prop-name <- identifier/p]
      ws/p
      (char/p #\=)
      ws/p
      [prop-value <- identifier/p]
      ws/p
      (pure `(,prop-name . ,prop-value))))

(provide ws/p newline/p parse-header/p)

