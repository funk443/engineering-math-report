(defparameter *plot-range* '(-6 . 6))
(defparameter *sampling-rate* 20000)
(defparameter *t* (loop for i from (car *plot-range*) upto (cdr *plot-range*)
                          by (/ *sampling-rate*)
                        collect i))

(declaim (ftype (function (number fixnum) number) inside-sigma)
         (inline inside-sigma))
(defun inside-sigma (tt n)
  (/ (* 2 (- (cos (* n pi)) 1) (cos (* n pi tt)))
     (* (expt pi 2) (expt n 2))))

(declaim (ftype (function (number &optional fixnum) number) f)
         (inline f))
(defun f (tt &optional (n 5))
  (+ 1/2 (loop for i from 1 upto n
               sum (inside-sigma tt i))))

(defparameter *term1* (map 'list (lambda (x) (f x 0)) *t*))
(defparameter *term2* (map 'list (lambda (x) (inside-sigma x 1)) *t*))
(defparameter *term3* (map 'list (lambda (x) (inside-sigma x 3)) *t*))
(defparameter *term4* (map 'list (lambda (x) (inside-sigma x 5)) *t*))
(defparameter *y* (map 'list #'+ *term1* *term2* *term3* *term4*))

(defparameter output-file "plot-data.txt")
(with-open-file (s output-file :direction :output
                               :if-exists :supersede
                               :if-does-not-exist :create)
  (loop initially (format s "# x y term1 term2 term3 term4~%")
        for x in *t*
        for y in *y*
        for term1 in *term1*
        for term2 in *term2*
        for term3 in *term3*
        for term4 in *term4*
        do (format s "~@{~f~^ ~}~%" x y term1 term2 term3 term4)))
