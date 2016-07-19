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

(de pName (n)
  (car (str (pack
    (make 
      (while (letter? (peek))
       (link (getChar))))))))

(de pNum (n)
  (car (str (pack
    (make 
      (while (number? (peek))
       (link (getChar))))))))

(de main ()
  (pTerm)
  (println (list 'println ast))
  (println '(bye))
  (bye))

(de pOperand ()
  (cond
    ((= (peek) "(") 
      (prog2
        (getChar) 
        (use (ast astFactor) 
          (pTerm))
        (match? ")"))) 
    ((number? (peek)) (pNum))
    ((letter? (peek)) (pName))
    (T (expected "term"))))

(de pTerm ()
  (if (addop? (peek))
    (setq ast 0)
    (setq ast (pFactor)))
  (while (addop? (peek))
    (case (getChar)
      ("+" (setq ast (list '+ ast (pFactor))))
      ("-" (setq ast (list '- ast (pFactor))))
      (T (expected "addop"))
  ast
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
