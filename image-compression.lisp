(ql:quickload 'png)

(defun fft (f)
  (let ((n (list-length f)))
    (cond
      ((= n 1) f)

      (t
       (loop with omega = (exp (/ (* -2 pi (complex 0 1)) n))

             and (f-even f-odd) = (loop for i from 0 below n
                                        if (evenp i)
                                          collect (nth i f) into even-terms
                                        else
                                          collect (nth i f) into odd-terms
                                        finally (return (list even-terms
                                                              odd-terms)))

             with y-even = (fft f-even)
             and y-odd = (fft f-odd)

             and y = (make-list n :initial-element 0)

             and middle = (floor n 2)
             for i from 0 below middle
             for even-term = (nth i y-even)
             for odd-term = (* (expt omega i) (nth i y-odd))
             do (setf (nth i y) (+ even-term odd-term)
                      (nth (+ i middle) y) (- even-term odd-term))
             finally (return y))))))

(defun ifft (f)
  (labels ((ifft-core (f)
             (let ((n (list-length f)))
               (cond
                 ((= n 1) f)
                 (t
                  (loop with omega = (exp (/ (* 2 pi (complex 0 1)) n))
                        and (f-even f-odd)
                          = (loop for i from 0 below n
                                  if (evenp i)
                                    collect (nth i f) into even-terms
                                  else
                                    collect (nth i f) into odd-terms
                                  finally (return (list even-terms odd-terms)))
                        with y-even = (ifft-core f-even)
                        and y-odd = (ifft-core f-odd)
                        and y = (make-list n :initial-element 0)
                        and middle = (floor n 2)
                        for i from 0 below middle
                        for even-term = (nth i y-even)
                        for odd-term = (* (expt omega i) (nth i y-odd))
                        do (setf (nth i y) (+ even-term odd-term)
                                 (nth (+ i middle) y) (- even-term odd-term))
                        finally (return y)))))))
    (let ((n (list-length f)))
      (map 'list (lambda (x) (/ x n)) (ifft-core f)))))

(defun transpose (m)
  (apply #'map 'list #'list m))

(defun ft2 (m &optional (f #'fft))
  (transpose (map 'list f (transpose (map 'list f m)))))

(defun rgb-to-greyscale (image)
  (loop with height = (png:image-height image)
        and width = (png:image-width image)
        with new-image = (png:make-image height width 1)
        for h from 0 below height
        do (loop for w from 0 below width
                 do (setf (aref new-image h w 0)
                          (round (loop for i from 0 below 3
                                       for colour = (aref image h w i)
                                       if (= i 0)
                                         sum (* 0.3 colour)
                                       end
                                       if (= i 1)
                                         sum (* 0.59 colour)
                                       end
                                       if (= i 2)
                                         sum (* 0.11 colour)
                                       end))))
        finally (return new-image)))

(defun greyscale-to-list (image)
  (loop with height = (png:image-height image)
        and width = (png:image-width image)
        for h from 0 below height
        collect (loop for w from 0 below width
                      collect (aref image h w 0))))

(defun list-to-greyscale (list)
  (loop with height = (list-length list)
        and width = (list-length (car list))
        with image = (png:make-image height width 1)
        for h from 0 below height
        do (loop for w from 0 below width
                 do (setf (aref image h w 0)
                          (min (floor (nth w (nth h list))) 255)))
        finally (return image)))

(defvar *original-image* (with-open-file (s "test-image.png"
                                            :element-type '(unsigned-byte 8))
                           (png:decode s)))

(defvar *original-image-grey* (rgb-to-greyscale *original-image*))

(defvar *original-image-grey-list* (greyscale-to-list *original-image-grey*))

(defparameter *transformed-list* (ft2 *original-image-grey-list*))

(defparameter *transformed-list-sort*
  (sort (apply #'append (map 'list (lambda (list)
                                     (map 'list #'abs list))
                             *transformed-list*))
        #'<))
(defparameter *threshold*
  (nth (round (* 0.99 (list-length *transformed-list-sort*)))
       *transformed-list-sort*))

(defparameter *filtered-list*
  (map 'list (lambda (list)
               (map 'list (lambda (x) (if (> (abs x) *threshold*) x 0)) list))
       *transformed-list*))

(defparameter *ifft-list* (ft2 *filtered-list* #'ifft))

(defparameter *output-image-list*
  (map 'list (lambda (list) (map 'list #'abs list)) *ifft-list*))

(defparameter *output-image* (list-to-greyscale *output-image-list*))
(with-open-file (s "output.png" :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede
                                :if-does-not-exist :create)
  (png:encode *output-image* s))
