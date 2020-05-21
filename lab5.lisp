(defun make+ (x1 x2)
  (cond
    ((eql x1 0)
     x2)
    ((eql x2 0)
     x1)
    ((equal x1 x2)
     (make* 2 x1))
    ((and (numberp x1)
          (numberp x2))
     (+ x1 x2))
    (t
     (list '+ x1 x2))))

(defun make- (x1 x2)
  (cond
    ((equal x1 x2)
     0)
    ((eql x2 0)
     x1)
    ((and (numberp x1)
          (numberp x2))
     (- x1 x2))
    (t
     (list '- x1 x2))))

(defun make* (x1 x2)
  (cond
    ((or (eql x1 0)
         (eql x2 0))
     0)
    ((eql x1 1)
     x2)
    ((eql x2 1)
     x1)
    ((and (numberp x1)
          (numberp x2))
     (* x1 x2))
    (t
     (list '* x1 x2))))

(defun make/ (x1 x2)
  (cond
    ((equal x1 x2)
     1)
    ((eql x1 0)
     0)
    ((eql x2 0)
     (error "Div by zero"))
    ((eql x2 1)
     x1)
    ((and (numberp x1)
          (numberp x2))
     (/ x1 x2))
    (t
     (list '/ x1 x2))))

(defun make-expt (x1 x2)
  (cond
    ((eql x1 0)
     1)
    ((eql x2 0)
     1)
    ((eql x2 1)
     x1)
    ((and (numberp x1)
          (numberp x2))
     (expt x1 x2))
    (t
     (list 'expt x1 x2))))

(defun make-cos (x)
  (if (numberp x)
      (cos x)
      (list 'cos x)))

(defun make-sin (x)
  (if (numberp x)
      (sin x)
      (list 'sin x)))

(defun make-log (x)
  (if (numberp x)
      (log x)
      (list 'log x)))

(defun differentiate (var expr)
  (typecase expr
    (number
     0)
    (symbol
     (if (eq expr var)
         1
         0))
    (list
     (let ((x1 (second expr))
           (x2 (third expr)))
       (case (first expr)
         (+
          (make+ (differentiate var x1)
                 (differentiate var x2)))
         (-
          (make- (differentiate var x1)
                 (differentiate var x2)))
         (*
          (make+ (make* (differentiate var x1)
                        x2)
                 (make* x1
                        (differentiate var x2))))
         (/
          (make/ (make- (make* (differentiate var x1)
                               x2)
                        (make* x1
                               (differentiate var x2)))
                 (make* x1
                        x2)))
         (expt
          (if (numberp x2)
              (make* (differentiate var x1)
                     (make* x2
                            (make-expt x1
                                       (1- x2))))
              (make* (make-expt x1 x2)
                     (make+ (make* (differentiate var x1)
                                   (make/ x2 x1))
                            (make* (differentiate var x2)
                                   (make-log x1))))))
         (sin
          (make* (differentiate var x1)
                 (make-cos x1)))
         (cos
          (make* (differentiate var x1)
                 (make- 0 (make-sin x1))))
         (otherwise
          (error "???")))))))
