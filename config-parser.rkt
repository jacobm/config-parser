#lang racket

(require data/applicative
         data/functor
         data/monad
         megaparsack
         megaparsack/text)

(struct section (name properties) #:transparent)

(define whitespace/p (hidden/p (satisfy/p char-blank?)))
(define ws/p (hidden/p (do (many/p whitespace/p) (pure (void)))))
(define newline/p (hidden/p (or/p (do (char/p #\return) (char/p #\newline)) (char/p #\newline))))
(define blank-line/p (hidden/p (do ws/p newline/p (pure (void)))))
(define blank-lines/p (hidden/p (do (many/p (try/p blank-line/p)) (pure (void)))))

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


(define parse-configuration/p
  (do blank-lines/p
      [header <- parse-header/p]
      (many/p (try/p blank-line/p))
      [properties <- (many/p (do blank-lines/p parse-property/p))]
      (pure (section header properties))))

(provide section
         ws/p
         newline/p
         blank-lines/p
         parse-header/p
         parse-property/p
         parse-configuration/p)
