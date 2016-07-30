(de clearws ()
  (while (not (eof))
    (setq c (char))
    (case c
      ("\\" 
        (setq escaped (not escaped))
	(prin c))
      ("\"" 
        (when (not escaped) (setq emstr (not emstr)))
	(setq escaped NIL)
	(prin c)) 
      ("\n" 
	(setq escaped NIL)
	(when (or emstr (not nl))
	  (prin c)) 
	(setq nl T))
      ((" " "\t")
        (when emstr (prin c)) 
	(setq escaped NIL))
      (T 
        (setq escaped NIL)
	(setq nl NIL)
        (prin c)))
  )
  (bye))
