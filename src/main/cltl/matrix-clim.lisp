
(in-package #:clim-user)

;; CLIM-CLX is not loading.
;; Something about presentation types + defmethod
;; Refer to debug output in SLIME.

(define-application-frame frob-frame  ()
  ()
  (:panes
   (matrix (let ((m #((1 2 3 4) (4 3 2 1))))
             (formatting-table ()
               (dotimes (i (array-dimension m 0))
                 (formatting-row ()
                   (dotimes (j (array-dimension m 1))
                     (formatting-cell ()
                       (with-output-as-presentation (t)
                         (princ (the number (aref m i j))
                                t)))))))))))

