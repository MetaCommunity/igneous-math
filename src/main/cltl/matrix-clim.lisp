
(in-package #:clim-user)

;; CLIM-CLX is not loading.
;; Something about presentation types + defmethod
;; Refer to debug output in SLIME.

(defun display-frob (frame pane)
  ;; FIXME: provide the matrix value via an attribute of the PANE
  ;; FIXME: also draw the conventional brackets on the pane
  (declare (ignore frame))
  (let ((*standard-output* pane))
    (let ((m #2A((1 2 3 4) (4 3 2 1) (4 2 3 1) (1 3 2 4))))
      (formatting-table ()
        (dotimes (i (array-dimension m 0))
          (formatting-row ()
            (dotimes (j (array-dimension m 1))
              (formatting-cell ()
                (present  (aref m i j))
                ))))))))


(define-application-frame frob-frame  ()
  ()
  (:panes
   (matrix :application
           :height 90 :width 100
           :display-function #'display-frob))
  (:layouts
   (default (vertically () matrix))))

#|

(let ((f (make-application-frame 'frob-frame
                                 :pretty-name "Frob")))
  (run-frame-top-level f))

|#

