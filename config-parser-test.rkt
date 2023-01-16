#lang racket
(module+ test
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
            (it "must read tabs" (check-success ws/p "\t" (void)))
            (it "must read space" (check-success ws/p " " (void)))
            (it "must read not read carriage return" (check-success ws/p "\r" (void)))
            (it "must read not read newline" (check-success ws/p "\n" (void))))

  (describe "newline/p"
            (it "must read newline" (check-success newline/p "\n" '#\newline))
            (it "must read read carriage return" (check-success newline/p "\r\n" '#\newline))
            (it "must not read lone carriage return" (ensure-fail newline/p "\r" '("'\n'")))
            (it "must not read tabs" (ensure-fail newline/p "\t" '()))
            (it "must not read space" (ensure-fail newline/p " " '())))

  (describe "comment-or-blank-lines/p"
            (it "must read blanks and newlines" (check-success comment-or-blank-lines/p "" (void)))
            (it "must read blanks and newlines if it can"
                (check-success comment-or-blank-lines/p " \t \n[fisk]\n" (void)))
            (it "must read blanks and newlines"
                (check-success comment-or-blank-lines/p " \t \n \n" (void))))

  (describe
   "parse-header/p"
   (it "must read a header" (check-success parse-header/p "[dingo]" 'dingo))
   (it "must read a header allowing for spaces"
       (check-success parse-header/p "[ \tdingo\t\t]" 'dingo))
   (it "must read a header allowing for comment"
       (check-success parse-header/p "[dingo] ; here is a comment" 'dingo))
   (it "must fail on missing header name" (ensure-fail parse-header/p "[]" '("header name")))
   (it "must fail on missing [" (ensure-fail parse-header/p "dingo]" '("'['")))
   (it "must fail on missing ]" (ensure-fail parse-header/p "[dingo" '("letter" "number" "']'"))))

  (describe "parse-property/p"
            (it "must read a property" (check-success parse-property/p "dingo=hest" '(dingo . hest)))
            (it "must read a property with whitespaces"
                (check-success parse-property/p "dingo \t\t =   \t hest" '(dingo . hest)))
            (it "must read a property allowing for comments"
                (check-success parse-property/p "dingo \t\t =   \t hest ; comment" '(dingo . hest)))
            (it "must fail on missing property name"
                (ensure-fail parse-property/p "= hest" '("property name")))
            (it "must fail on missing property value"
                (ensure-fail parse-property/p "dingo = \t" '("property value")))
            (it "must fail on missing =" (ensure-fail parse-property/p "dingo hest" '("'='"))))

  (describe "parse-properties/p"
            (it "must read a set of properties"
                (check-success parse-properties/p
                               "name=value\ndingo = hest"
                               (list '(name . value) '(dingo . hest))))
            (it "must read a set of properties, comments and newlines"
                (check-success
                 parse-properties/p
                 "

   ;comment

name=value ; comment
   ; comment
dingo = hest

; comment
"
                 (list '(name . value) '(dingo . hest)))))

  (define (check input expected)
    (check-success parse-section/p input expected))

  (describe "parse-section/p"
            (it "must read a section"
                (check " \n [fisk]\nname=value\n  dingo = hest"
                       (section 'fisk (list '(name . value) '(dingo . hest)))))
            (it "must read a empty section" (check " \n [fisk]\n" (section 'fisk '()))))

  (describe
   "parse-ini/p"
   (it "must read a ini file"
       (check-success
        parse-ini/p
        " \n [fisk]\nname=value\n \n  dingo = hest\n \n [section2]\nprop1=value\n prop2 = hest\n"
        (list (section 'fisk '((name . value) (dingo . hest)))
              (section 'section2 '((prop1 . value) (prop2 . hest))))))
   (it
    "must read a ini file with comments"
    (check-success
     parse-ini/p
     "
; comment
[fisk]
name=value ;comment
; comment
dingo = hest ;comment
;comment
[section2]
prop1=value
prop2=hest ;comment
 ; comment"
     (list (section 'fisk '((name . value) (dingo . hest)))
           (section 'section2 '((prop1 . value) (prop2 . hest))))))
   (it "must read a ini file that ends with empty section"
       (check-success
        parse-ini/p
        "
[fisk]
name=value
dingo = hest

[section2]
prop1=value
prop2 = hest
[endsection]"
        (list (section 'fisk '((name . value) (dingo . hest)))
              (section 'section2 '((prop1 . value) (prop2 . hest)))
              (section 'endsection '()))))))
