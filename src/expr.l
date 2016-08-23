(de pBlock ()
  (use astBlock
		(match? "{")
		(setq astBlock 
			(make
				(while (!= (peek) "}")
					(link (pExp)))))
		(match? "}")
		(if (not astBlock)
			(cons 'prog astBlock))))

(de pExp ()
  
  )

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

