(setq mybuffs (list))

(defun copy-buff-keep-order ()
  "asfsd"
  (interactive)
  (progn
    (setq mybuffs (append mybuffs (seq-difference (buffer-list) mybuffs)))
    (setq mybuffs (seq-filter (lambda (elt) (seq-contains (buffer-list) elt)) mybuffs))
    )
  )

(defun ordered-switch-3()
  (interactive)
  (ordered-switch 3)
  )

(defun ordered-switch (num)
  (pop-to-buffer (seq-elt mybuffs num))
  )

(global-set-key (kbd "C-3") 'ordered-switch-3)
(copy-buff-keep-order)
