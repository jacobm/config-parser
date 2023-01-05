# config-parser
A parser for INI files in Racket

# Usage

```
(parse-string parse-ini/p "


[section]
fisk = hest
   fisk2=   hest2


  fisk3  =hest3

[section2]
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
