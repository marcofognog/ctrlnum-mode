(defun copy-buff-keep-order ()
  "asfsd"
  (progn
    (setq mybuffs (append mybuffs (seq-difference (buffer-list) mybuffs)))
    (setq mybuffs (seq-filter (lambda (elt) (seq-contains (buffer-list) elt)) mybuffs))
    )
  )

(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(print-elements-of-list mybuffs)
(copy-buff-keep-order)
