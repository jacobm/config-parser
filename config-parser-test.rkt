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
          (it "must read newline" (check-success newline/p "\n" '#\newline))
          (it "must read read carriage return" (check-success newline/p "\r\n" '#\newline))
          (it "must not read lone carriage return" (ensure-fail newline/p "\r" '("'\n'")))
          (it "must not read tabs" (ensure-fail newline/p "\t" '("newline")))
          (it "must not read space" (ensure-fail newline/p " " '("newline"))))

(describe "parse-header/p"
          (it "must read a header" (check-success parse-header/p "[dingo]\n" 'dingo))
          (it "must read a header allowing for spaces"
              (check-success parse-header/p "  [ \tdingo\t\t]  \n" 'dingo))
          (it "must fail on missing header name"
              (ensure-fail parse-header/p "[]" '("whitespaces" "header name")))
          (it "must fail on missing [" (ensure-fail parse-header/p "dingo]" '("whitespaces" "'['")))
          (it "must fail on missing ]"
              (ensure-fail parse-header/p "[dingo" '("letter" "number" "whitespaces" "']'"))))

(describe "parse-property/p"
          (it "must read a property" (check-success parse-property/p "dingo=hest\n" '(dingo . hest)))
          (it "must read a property"
              (check-success parse-property/p " \t dingo \t\t =   \t hest\t  \n" '(dingo . hest)))
          (it "must fail on missing property name"
              (ensure-fail parse-property/p " = hest\n" '("whitespace" "property name")))
          (it "must fail on missing property value"
              (ensure-fail parse-property/p " dingo = \t \n" '("whitespace" "property value")))
          (it "must fail on missing ="
              (ensure-fail parse-property/p "dingo hest" '("whitespace" "'='"))))
