(defun mmap (func list)
  (if list
      (cons (funcall func (car list))
            (mmap func (cdr list)))))

(defun filter (func list)
  (if list
      (if (funcall func (car list))
          (cons (car list) (filter func (cdr list)))
          (filter func (cdr list)))))

(defun fold (func list &optional (initial-value nil initial-value-present))
  (if list
      (if initial-value-present
          (fold func (cdr list) (funcall func initial-value (car list)))
          (if (cdr list)
              (fold func (cddr list) (funcall func (first list) (second list)))
              (first list)))
      initial-value))

(defun test ()
  (pprint (mmap (lambda (x) (+ x 100)) (list 1 2 3 4 5 6 7)))
  (pprint (mmap (lambda (x) (+ x 100)) (list)))
  (pprint "---------")
  (pprint (filter (lambda (x) (evenp x)) (list 1 2 3 4 5 6 7)))
  (pprint (filter (lambda (x) (oddp x)) (list 1 2 3 4 5 6 7)))
  (pprint (filter (lambda (x) (evenp x)) (list 1)))
  (pprint (filter (lambda (x) (oddp x)) (list 1)))
  (pprint "---------")
  (pprint (fold (lambda (x y) (+ x y)) (list 1 2 3 4)))
  (pprint (fold (lambda (x y) (+ x y)) (list 1 2 3 4) 7))
  (pprint (fold (lambda (x y) (+ x y)) (list 1)))
  (pprint (fold (lambda (x y) (+ x y)) (list 1) 2))
  (pprint (fold (lambda (x y) (+ x y)) (list) 2))
  (pprint (fold (lambda (x y) (cons x y)) (list 1 2))))
