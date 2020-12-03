(use-modules (ice-9 rdelim))

(define (matches-half num)
  (let* ((numl (string->list num))
	 (halfl (/ (length numl) 2))
	 (set1 (list-tail numl halfl))
	 (set2 (list-head numl halfl)))
    (map (lambda (x y) (if (equal? x y) x #\0))
	 set1
	 set2)))

(define (sum-match l)
  (* 2 (apply + (map (lambda (x) (string->number (string x))) l))))

(define (display-match l)
  (format #t "~s~%" l))

(let loop ((line (read-line)))
  (if (eof-object? line)
      #f
      (begin
	(display-match (sum-match (matches-half line)))
	(loop (read-line)))))
