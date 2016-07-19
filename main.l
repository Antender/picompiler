(setq inputChar 0)
(de getChar ()
  (setq inputChar (+ inputChar 1))
  (char))

(de expected (s)
 (println)
 (prinl "Error at " inputChar ": " s " expected")
 (bye))

(de letter? (c)
  (or 
    (<= "a" c "z") 
    (<= "A" c "Z")))

(de number? (c)
  (<= "0" c "9"))

(de addop? (c)
  (or (= c "+") (= c "-")))

(de mulop? (c)
  (or (= c "*") (= c "/")))

(de match? (c)
  (if (= (peek) c)
    (getChar)
    (expected c)))

(de pNum ()
  (car (str (pack
    (make 
      (while (number? (peek))
       (link (getChar))))))))

(de pName ()
  (car (str (pack
    (make 
      (while (letter? (peek))
       (link (getChar))))))))

(de pIdent ()  
  (setq astIdent (car (str (pack
    (make 
      (while (letter? (peek))
       (link (getChar))))))))
  (if (= (peek) "(")
    (prog
      (match? "(")
      (match? ")") 
      (list astIdent))
    astIdent))

(de main ()
  (setq ast (pAssignment))
  (prinl "(de pi () 3.14)")
  (println ast)
  (println '(println output))
  (println '(bye))
  (bye))

(de pOperand ()
  (cond
    ((= (peek) "(") 
      (prog2
        (getChar) 
        (use (astTerm astFactor) 
          (pTerm))
        (match? ")"))) 
    ((number? (peek)) (pNum))
    ((letter? (peek)) (pIdent))
    (T (expected "term"))))

(de pTerm ()
  (if (addop? (peek))
    (setq astTerm 0)
    (setq astTerm (pFactor)))
  (while (addop? (peek))
    (case (getChar)
      ("+" (setq astTerm (list '+ astTerm (pFactor))))
      ("-" (setq astTerm (list '- astTerm (pFactor))))
      (T (expected "addop"))
  astTerm
)))

(de pFactor ()
  (setq astFactor (pOperand))
  (while (mulop? (peek))
    (case (peek)
      ("*" (setq astFactor (list '* astFactor (pOp))))
      ("/" (setq astFactor (list '/ astFactor (pOp))))
      (T (expected "mulop"))
    )
  )
  astFactor
)

(de pOp ()
  (getChar)
  (pOperand))

(de pAssignment ()
  (setq astAssignment (pName))
  (match? "=")
  (setq astAssignment (list 'setq astAssignment (pTerm)))
  (match? "\n")
  astAssignment)
