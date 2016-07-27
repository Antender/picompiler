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
