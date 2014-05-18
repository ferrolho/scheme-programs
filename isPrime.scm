(define isPrime
	(lambda (n)
		(letrec ((aux
			(lambda (i)
				(cond
					((> i (sqrt n)) #t)
					((zero? (remainder n i)) #f)
					(else (aux (add1 i)))))))
		(aux 2))))