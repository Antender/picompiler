(de pNum ()
  (car (str (pack
    (make
      (while (number? (peek))
        (link (getChar))))))))

(de pName ()
  (car (str (pack
    (make
      (while (letter? (peek))
        (link (getChar)))))))))
