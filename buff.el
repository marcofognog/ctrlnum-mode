(setq mybuffs (list))

(defun copy-buff-keep-order ()
  "asfsd"
  (interactive)
  (progn
    (setq file-buffs (seq-filter (lambda (elt) (buffer-file-name elt)) (buffer-list)))
    (setq mybuffs (append mybuffs (seq-difference file-buffs mybuffs)))
    (setq mybuffs (seq-filter (lambda (elt) (seq-contains file-buffs elt)) mybuffs))
    )
  )

(defun ordered-switch-1() (interactive) (ordered-switch 0) )
(defun ordered-switch-2() (interactive) (ordered-switch 1) )
(defun ordered-switch-3() (interactive) (ordered-switch 2) )
(defun ordered-switch-4() (interactive) (ordered-switch 3) )
(defun ordered-switch-5() (interactive) (ordered-switch 4) )
(defun ordered-switch-6() (interactive) (ordered-switch 5) )
(defun ordered-switch-7() (interactive) (ordered-switch 6) )
(defun ordered-switch-8() (interactive) (ordered-switch 7) )
(defun ordered-switch-9() (interactive) (ordered-switch 8) )
(defun ordered-switch-0() (interactive) (ordered-switch 9) )


(defun ordered-switch (num)
  (if (< num (length mybuffs))
  (switch-to-buffer (seq-elt mybuffs num))
  nil
  )
)

(global-set-key (kbd "C-1") 'ordered-switch-1)
(global-set-key (kbd "C-2") 'ordered-switch-2)
(global-set-key (kbd "C-3") 'ordered-switch-3)
(global-set-key (kbd "C-4") 'ordered-switch-4)
(global-set-key (kbd "C-5") 'ordered-switch-5)
(global-set-key (kbd "C-6") 'ordered-switch-6)
(global-set-key (kbd "C-7") 'ordered-switch-7)
(global-set-key (kbd "C-8") 'ordered-switch-8)
(global-set-key (kbd "C-9") 'ordered-switch-9)
(global-set-key (kbd "C-0") 'ordered-switch-0)
(copy-buff-keep-order)

(defun print-elements-of-list (list)
       "Print each element of LIST on a line of its own."
       (while list
         (print (car list))
         (setq list (cdr list))))

(print-elements-of-list file-buffs)
