(de reduceConst (astReduceConst)
  (if (and (lst? astReduceConst) (num? (cadr astReduceConst)) (num? (caddr astReduceConst)))
    (apply (car astReduceConst) (cdr astReduceConst))
    astReduceConst))
