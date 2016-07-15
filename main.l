(de expected (s)
 (println)
 (prinl "Error: " s " expected")
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
    (char)
    (expected c)))

(de pName (n)
  (car (str (char))))

(de pNum (n)
  (car (str (char))))

(de main ()
  (pTerm)
  (println (list 'println ast))
  (println '(bye))
  (bye))

(de pOperand ()
  (cond 
    ((number? (peek)) (pNum))
    ((letter? (peek)) (pName))
    (T (expected "term"))))

(de pTerm ()
  (if (addop? (peek))
    (setq ast 0)
    (setq ast (pFactor)))

  (while (addop? (peek))
    (case (peek)
      ("+" (setq ast (list '+ ast (pFactor))))
      ("-" (setq ast (list '- ast (pFactor))))
      (T (expected "addop"))
  ast
)))

(de pFactor ()
  (setq astFactor (pOp))
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
  (char)
  (pOperand))
