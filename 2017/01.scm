(use-modules (ice-9 rdelim))

(define (matches-next num)
  (define (matches-next-tco nl acc)
    (cond
     ((= 1 (length nl))
      acc)
     ((equal? (car nl) (cadr nl))
      (matches-next-tco (cdr nl) (append acc (list (car nl)))))
     (else
      (matches-next-tco (cdr nl) acc))))
  (let* ((numl (string->list num))
	 (numlist (append numl (list (car numl)))))
    (matches-next-tco numlist '())))

(define (sum-match l)
  (apply + (map (lambda (x) (string->number (string x))) l)))

(define (display-match l)
  (format #t "~s~%" l))

(let loop ((line (read-line)))
  (if (eof-object? line)
      #f
      (begin
	(display-match (sum-match (matches-next line)))
	(loop (read-line)))))
