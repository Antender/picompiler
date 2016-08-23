(de whitespace? (c)
  (member c '(" " "\t")))

(de letter? (c)
  (or
    (<= "a" c "z")
    (<= "A" c "Z")))

(de digit? (c)
  (<= "0" c "9"))

(de hexDigit? (c)
  (or
    (<= "A" c "F")
		(<= "a" c "f")
    (digit? c)))

(de addop? (c)
  (member c '("+" "-")))

(de mulop? (c)
  (member c '("*" "/")))

(de alpha? (c)
  (or
	  (letter? c)
		(digit? c)))
