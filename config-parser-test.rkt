#lang racket

(require data/either
         megaparsack
         megaparsack/text
         racket/match
         rackunit
         "config-parser.rkt")

(define (check-success parser input expected)
  (let* ([result (parse-string parser input)])
    (match result
      [(failure msg) (fail (parse-error->string msg))]
      [(success value) (check-equal? value expected)])))

(test-case "ws/p should read tabs" (check-success ws/p "\t" '(#\tab)))

(test-case "ws/p should read space" (check-success ws/p " " '(#\space)))

(test-case "ws/p should read not read carriage return" (check-success ws/p "\r" '()))

(test-case "ws/p should read not read newline" (check-success ws/p "\n" '()))
