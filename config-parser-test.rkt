#lang racket

(require data/either
         megaparsack
         megaparsack/text
         racket/match
         rackunit
         rackunit/spec
         "config-parser.rkt")

(define (check-success parser input expected)
  (let ([result (parse-string parser input)])
    (match result
      [(failure msg) (fail (parse-error->string msg))]
      [(success value) (check-equal? value expected)])))

(define (ensure-fail parser input expected)
  (let ([result (parse-string parser input)])
    (match result
      [(success value) (fail (parse-error->string value))]
      [(failure (message _ _ expc)) (check-equal? expected expc)])))

(describe "ws/p"
          (it "must read tabs" (check-success ws/p "\t" '(#\tab)))
          (it "must read space" (check-success ws/p " " '(#\space)))
          (it "must read not read carriage return" (check-success ws/p "\r" '()))
          (it "must read not read newline" (check-success ws/p "\n" '())))

(describe "newline/p"
          (it "must read newline" (check-success newline/p "\n" '(#\newline)))
          (it "must read read carriage return" (check-success newline/p "\r\n" '(#\newline)))
          (it "must not read lone carriage return" (ensure-fail newline/p "\r" '("'\n'")))
          (it "must not read tabs" (ensure-fail newline/p "\t" '("newlines")))
          (it "must not read space" (ensure-fail newline/p " " '("newlines"))))

(describe "parse-header/p"
          (it "must read a header" (check-success parse-header/p "[dingo]" 'dingo))
          (it "must fail on missing [" (ensure-fail parse-header/p "dingo]" '("'['")))
          (it "must fail on missing ]"
              (ensure-fail parse-header/p "[dingo" '("letter" "number" "whitespaces" "']'"))))
