(setq inputChar 0)
(de getChar ()
  (setq inputChar (+ inputChar 1))
  (char))

(de skipws ()
  (while (whitespace? (peek))
    (getChar)))

(de expected (s)
 (prinl "Error at " inputChar ": " s " expected")
 (bye))

(de reduceConst (astReduceConst)
  (if (and (lst? astReduceConst) (num? (cadr astReduceConst)) (num? (caddr astReduceConst)))
    (apply (car astReduceConst) (cdr astReduceConst))
    astReduceConst))

(de whitespace? (c)
  (or (= c " ") (= c "\t")))

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
    (expected c))
  (skipws))

(de pNum ()
  (prog1
    (car (str (pack
      (make 
        (while (number? (peek))
          (link (getChar)))))))
    (skipws)))

(de pName ()
  (prog1
    (car (str (pack
      (make 
        (while (letter? (peek))
          (link (getChar)))))))
    (skipws)))

(de pIdent ()  
  (setq astIdent (car (str (pack
    (make 
      (while (letter? (peek))
       (link (getChar))))))))
  (skipws)
  (if (= (peek) "(")
    (prog
      (match? "(")
      (match? ")") 
      (list astIdent))
    astIdent))

(de main ()
  (prinl "(de pi () 3.14)")
  (println (pBlock))
  (println '(bye))
  (bye))

(de pOperand ()
  (skipws)
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
      (T (expected "addop")))
    (setq astTerm (reduceConst astTerm)))
  astTerm)

(de pFactor ()
  (setq astFactor (pOperand))
  (while (mulop? (peek))
    (case (peek)
      ("*" (setq astFactor (list '* astFactor (pOp))))
      ("/" (setq astFactor (list '/ astFactor (pOp))))
      (T (expected "mulop"))
    )
    (setq astFactor (reduceConst astFactor))
  )
  astFactor
)

(de pOp ()
  (getChar)
  (skipws)
  (pOperand))

(de pExpression ()
  (setq astExpression (pIdent))
  (if (lst? astExpression)
    (prog1
      astExpression
      (skipws)
      (match? "\n"))
    (match? "=")
      (setq astExpression (list 'setq astExpression (pTerm)))
      (skipws)
      (match? "\n")
      astExpression))

(de pBlock ()
  (setq astBlock (make
    (while (<> (peek) "e")
      (link (pExpression))
    )
  ))
  (if (= astBlock NIL)
    NIL
    (cons 'prog astBlock))
)
