(de pNum ()
  (while (= "0" (peek))
	  (getChar))
	(if (= "x" (peek))
	  (pHexodecimal)
		(pDecimal)))

(de pDecimal ()
  (car (str (pack
    (make
      (while (digit? (peek))
        (link (getChar)))
			(when (= c ".")
			  (link (getChar))
				(while (digit? (peek))
					(link (getChar)))))))))


(de pHexodecimal ()
  (getChar)
  (hex (pack
    (make
      (while (hexDigit? (peek))
        (link (getChar)))))))

(de pIdent ()
  (car (str (pack
    (make
      (while (letter? (peek))
        (link (getChar))))))))

(de pString ()
  (prog2
		(getChar)
  	(pack
	  	(make
		  	(while (<> "\"" (peek))
			  	(link (getChar)))))
		(getChar)))
