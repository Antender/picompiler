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
