# config-parser
A parser for INI files in Racket

# Usage

```
(parse-string parse-ini/p "

; this is an important section
[section]
fisk = hest  ; here we assign a value to fisk
fisk2 = hest2
; fisk3 comment
fisk3 = hest3

[section2]
; dingo must be bingo
dingo = bingo

[sectionAtTheEnd]
")
``` 

produces

```
(success 
   (list 
     (section 'section '((fisk . hest) (fisk2 . hest2) (fisk3 . hest3))) 
     (section 'section2 '((dingo . bingo))) 
     (section 'sectionAtTheEnd '())))
```
