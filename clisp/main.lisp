(defvar *maze-size* 11)

(defstruct maze
  front
  side
  top)

(defun load-walls (filename)
  "Return the `maze' object from walls stored in file `filename'."
  (let ((new-walls (with-open-file (in filename)
                     (with-standard-io-syntax
                       (read in)))))
    (make-maze
     :front (first new-walls)
     :side (second new-walls)
     :top (third new-walls))))

(defun maze-has-wall (maze x y z)
  "Check whether there is a wall at the given coordinates."
  (or
   (aref (maze-front maze)
         y x)
   (aref (maze-side maze)
         y z)
   (aref (maze-top maze)
         (- *maze-size* z 1) x)))

(defun find-path-in-maze (maze start end &key path)
  "Find the path from `start' to `end' in `maze'."
  (if (equal start end)
      (reverse (cons start path))
      (unless (or
               (equal start (second path))
               (apply #'maze-has-wall maze start))
        (destructuring-bind (x y z)
            start
          (or
           (find-path-in-maze maze (list
                                    (1+ x) y z)
                              end
                              :path (cons start path))
           (find-path-in-maze maze (list
                                    x (1+ y) z)
                              end
                              :path (cons start path))
           (find-path-in-maze maze (list
                                    x y (1+ z))
                              end
                              :path (cons start path))
           (find-path-in-maze maze (list
                                    (1- x) y z)
                              end
                              :path (cons start path))
           (find-path-in-maze maze (list
                                    x (1- y) z)
                              end
                              :path (cons start path))
           (find-path-in-maze maze (list
                                    x y (1- z))
                              end
                              :path (cons start path)))))))


(format t "~{~a~%~}"
        ;; Subtract the auxiliary walls' coords to match the results with the C++ version.
        (mapcar #'(lambda (point)
                    (destructuring-bind (x y z)
                        point
                      (list (1- x)
                            (1- y)
                            (1- z))))
                (find-path-in-maze (load-walls "input")
                                   ;; Coords +1 due to the auxiliary walls.
                                   ;; TODO: Add the walls to the C++ version to simplify it?
                                   '(1 5 1)
                                   '(7 3 7))))
