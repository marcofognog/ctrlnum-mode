(setq mybuffs (list))

(defun ctrlnum-update ()
  "Update the ordered file buffer list."
  (interactive)
  (progn
    (setq file-buffs (seq-filter (lambda (elt) (buffer-file-name elt)) (buffer-list)))
    (setq mybuffs (append mybuffs (seq-difference file-buffs mybuffs)))
    (setq mybuffs (seq-filter (lambda (elt) (seq-contains file-buffs elt)) mybuffs))
    (delete-dups mybuffs) ;; why do we need this?
    )
  )

(defun ctrlnum-switch-1() (interactive) (ctrlnum-switch 0) )
(defun ctrlnum-switch-2() (interactive) (ctrlnum-switch 1) )
(defun ctrlnum-switch-3() (interactive) (ctrlnum-switch 2) )
(defun ctrlnum-switch-4() (interactive) (ctrlnum-switch 3) )
(defun ctrlnum-switch-5() (interactive) (ctrlnum-switch 4) )
(defun ctrlnum-switch-6() (interactive) (ctrlnum-switch 5) )
(defun ctrlnum-switch-7() (interactive) (ctrlnum-switch 6) )
(defun ctrlnum-switch-8() (interactive) (ctrlnum-switch 7) )
(defun ctrlnum-switch-9() (interactive) (ctrlnum-switch 8) )
(defun ctrlnum-switch-10() (interactive) (ctrlnum-switch 9) )

(defun ctrlnum-switch (num)
  (if (< num (length mybuffs))
      (switch-to-buffer (seq-elt mybuffs num))
    nil
    )
  (ctrlnum-print-positions)
  )

(defun ctrlnum-build-name(buff)
  "..."
  (progn
    (if (eq (current-buffer) buff) (setq mark "*") (setq mark "") )
    (concat (number-to-string (+ 1 (cl-position buff mybuffs))) "." mark (file-name-nondirectory (buffer-file-name buff)))
    )
  )

(defun ctrlnum-print-positions()
  "print file positions"
  (interactive)
  (setq tabstring (mapconcat 'identity (mapcar 'ctrlnum-build-name mybuffs) " "))
  (message tabstring)
  )

(defun ctrlnum-next()
  "switch to the next buffer in the ordered list"
  (interactive)
  (ctrlnum-switch (ctrlnum-next-buffer))
  )

(defun ctrlnum-previous()
  "switch to the previous buffer in the ordered list"
  (interactive)
  (ctrlnum-switch (ctrlnum-previous-buffer))
  )

(defun ctrlnum-next-buffer()
  (+ 1 (cl-position (current-buffer) mybuffs))
  )

(defun ctrlnum-previous-buffer()
  (- 1 (cl-position (current-buffer) mybuffs))
  )

(defun ctrlnum-switch-order-list-back()
  (progn
    (setq posi (cl-position (current-buffer) mybuffs))
    (setq first-half (seq-take mybuffs (- posi 1)))
    (setq last-half (seq-drop mybuffs (+ posi 1)))
    (setq first-half-p (append first-half (list (current-buffer))))
    (setq last-half-p (cons (nth (ctrlnum-previous-buffer) mybuffs) last-half))
    (append first-half-p last-half-p)
    )
  )

(defun ctrlnum-switch-order-list-forward()
  (progn
    (setq posi (cl-position (current-buffer) mybuffs))
    (setq first-half (seq-take mybuffs posi))
    (setq last-half (seq-drop mybuffs (+ posi 2)))
    (setq first-half-p (append first-half (list (nth (ctrlnum-next-buffer) mybuffs))))
    (setq last-half-p (cons (current-buffer) last-half))
    (append first-half-p last-half-p)
    )
  )

(defun ctrlnum-switch-order-next()
  "switch order with next buffer"
  (interactive)
    (setq mybuffs (ctrlnum-switch-order-list-forward))
  )

(defun ctrlnum-switch-order-prev()
  "switch order with previous buffer"
  (interactive)
    (setq mybuffs (ctrlnum-switch-order-list-back))
  )


(defun print-elements-of-list (list)
  "Print each element of LIST on a line of its own."
  (while list
    (print (car list))
    (setq list (cdr list))))

(ctrlnum-update)
(add-hook 'buffer-list-update-hook 'ctrlnum-update)

;; overwritting default maps (left-scroll)
(global-set-key (kbd "C-<next>") 'ctrlnum-next)
(global-set-key (kbd "C-<prior>") 'ctrlnum-previous)

(define-minor-mode ctrlnum-mode
  "Google Chrome's tab swicthing style for buffers"
  :lighter " ctrlnum"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-1") 'ctrlnum-switch-1)
            (define-key map (kbd "C-2") 'ctrlnum-switch-2)
            (define-key map (kbd "C-3") 'ctrlnum-switch-3)
            (define-key map (kbd "C-4") 'ctrlnum-switch-4)
            (define-key map (kbd "C-5") 'ctrlnum-switch-5)
            (define-key map (kbd "C-6") 'ctrlnum-switch-6)
            (define-key map (kbd "C-7") 'ctrlnum-switch-7)
            (define-key map (kbd "C-8") 'ctrlnum-switch-8)
            (define-key map (kbd "C-9") 'ctrlnum-switch-9)
            (define-key map (kbd "C-0") 'ctrlnum-switch-10)
            (define-key map (kbd "C-c u") 'ctrlnum-update)
            map))

