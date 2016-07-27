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

(de match? (c)
  (if (= (peek) c)
    (getChar)
    (expected c))
  (skipws))
