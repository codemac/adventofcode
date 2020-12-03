(use-modules (ice-9 rdelim)
	     (srfi srfi-1))

(define (input->list s)
  (map string->number (string-split s char-set:whitespace)))


(define (list-min s)
  (fold min (car s) s))

(define (list-max s)
  (fold max (car s) s))

(define (even-divisor x y)
  (if (equal? (remainder x y) 0)
      (/ x y)
      0))

(define (even-divisors l)
  (let ((run (apply + (map (lambda (x) (even-divisor (car l) x)) (cdr l)))))
    (if (and (equal? run 0)
	     (> (length l) 1))
	(even-divisors (cdr l))
	run)))

(define (even-diff s)
  (let* ((sorted-list (sort s >))
	 (cksum (even-divisors sorted-list)))
    cksum))


(format #t "~s~%"
	(apply + (let loop ((line (read-line)))
		   (if (eof-object? line)
		       '()
		       (cons (even-diff (input->list line))
			     (loop (read-line)))))))
