;;; ctrlnum.el --- Tab switching style for file buffers with C-[0..9] keys like popular brosers.

;; Copyright (C) 2017 Marco A. F. Nogueira

;; Author: Marco A. F. Nogueira <marcofognog@gmail.com>
;; Version: 1.0
;; Package-Requires ((cl-lib "0.5"))
;; Keywords: convenience, buffer, tabs
;; URL: https://github.com/marcofognog/ctrlnum.el

;; This file is NOT part of GNU Emacs.

;;; License:
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Conveniently switch between file buffers just like you would with
;; Google Chrome's tabs default key bindings:
;;
;; C-1 switches to the first file buffer
;; C-2 switches to the second file buffer
;; ...
;; C-0 switches to the 10th file buffer
;;
;; C-<prior> switch to the previous buffer
;; C-<next> switch to the next buffer
;;
;; C-S-<prior> in buffer causes that buffer to switch place with the previous buffer
;; C-S-<next> the same as the above, but in the other direction
;;
;; Because the native Emacs buffer-list is kept intact, you can still use all
;; the other buffer switching tactics you already have.

;;; Code:

(setq ctrlnum-ordered-buffers (list))

(defun ctrlnum-update ()
  "Update the ordered file buffer list."
  (interactive)
  (setq ctrlnum-ordered-buffers (ctrlnum-sync-list ctrlnum-ordered-buffers (buffer-list))))

(defun ctrlnum-sync-list (ordered original)
  (progn
    (setq file-buffs (seq-filter (lambda (elt) (buffer-file-name elt)) original))
    (setq ordered (append ordered (seq-difference file-buffs ordered)))
    (setq ordered (seq-filter (lambda (elt) (seq-contains file-buffs elt)) ordered))
    (delete-dups ordered)
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
  "Make the switch to a buffer.  Argument NUM : the number in the ordered list to swtich to."
  (if (< num (length ctrlnum-ordered-buffers))
      (switch-to-buffer (seq-elt ctrlnum-ordered-buffers num))
    nil)
  (ctrlnum-print-positions))

(defun ctrlnum-build-name(buff)
  (progn
    (if (eq (current-buffer) buff) (setq mark "*") (setq mark "") )
    (concat (number-to-string (+ 1 (cl-position buff ctrlnum-ordered-buffers)))
            "."
            mark
            (file-name-nondirectory (buffer-file-name buff)))))

(defun ctrlnum-print-positions()
  (setq tabstring (mapconcat 'identity
                             (mapcar 'ctrlnum-build-name ctrlnum-ordered-buffers)
                             " "))
  (message tabstring))

(defun ctrlnum-next()
  "Switch to the next buffer in the ordered file buffer list"
  (interactive)
  (ctrlnum-switch (ctrlnum-next-buffer)))

(defun ctrlnum-previous()
  "Switch to the previous buffer in the ordered file buffer list"
  (interactive)
  (ctrlnum-switch (ctrlnum-previous-buffer)))

(defun ctrlnum-next-buffer()
  (+ 1 (cl-position (current-buffer) ctrlnum-ordered-buffers)))

(defun ctrlnum-previous-buffer()
  (- 1 (cl-position (current-buffer) ctrlnum-ordered-buffers)))

(defun ctrlnum-switch-order-list-back()
  (progn
    (setq posi (cl-position (current-buffer) ctrlnum-ordered-buffers))
    (setq first-half (seq-take ctrlnum-ordered-buffers (- posi 1)))
    (setq last-half (seq-drop ctrlnum-ordered-buffers (+ posi 1)))
    (setq first-half-p (append first-half (list (current-buffer))))
    (setq last-half-p (cons (nth (ctrlnum-previous-buffer) ctrlnum-ordered-buffers) last-half))
    (append first-half-p last-half-p)))

(defun ctrlnum-switch-order-list-forward()
  (progn
    (setq posi (cl-position (current-buffer) ctrlnum-ordered-buffers))
    (setq first-half (seq-take ctrlnum-ordered-buffers posi))
    (setq last-half (seq-drop ctrlnum-ordered-buffers (+ posi 2)))
    (setq first-half-p (append first-half (list (nth (ctrlnum-next-buffer) ctrlnum-ordered-buffers))))
    (setq last-half-p (cons (current-buffer) last-half))
    (append first-half-p last-half-p)))

(defun ctrlnum-switch-order-next()
  "Rearrange buffer position with the next buffer in the list"
  (interactive)
  (progn
    (setq ctrlnum-ordered-buffers (ctrlnum-switch-order-list-forward))
    (ctrlnum-update)
    (ctrlnum-print-positions)))

(defun ctrlnum-switch-order-prev()
  "Rearrange buffer position with the previous buffer in the list"
  (interactive)
  (progn
    (setq ctrlnum-ordered-buffers (ctrlnum-switch-order-list-back))
    (ctrlnum-update)
    (ctrlnum-print-positions)))

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
            map))

(ctrlnum-update)
(add-hook 'buffer-list-update-hook 'ctrlnum-update)

(ert-deftest ctrlnum-sync-list-empty-test ()
  (setq buffer-list-ordered (list))
  (setq buffer-list-origin (list (find-file "README.md")))
  (setq result (ctrlnum-sync-list buffer-list-ordered buffer-list-origin))
  (setq expected 1)
  (should (= (safe-length result) expected))
  )

(ert-deftest ctrlnum-sync-list-with-buffer-test ()
  (setq buffer-list-ordered (list (generate-new-buffer "bla")))
  (setq buffer-list-origin (list (find-file "README.md")))
  (setq result (ctrlnum-sync-list buffer-list-ordered buffer-list-origin))
  (setq expected 1)
  (should (= (safe-length result) expected))
  )

(ert-deftest ctrlnum-sync-list-with-does-not-duplicate-test ()
  (setq buffer-list-ordered (list (find-file "README.md")))
  (setq buffer-list-origin (list (find-file "README.md")))
  (setq result (ctrlnum-sync-list buffer-list-ordered buffer-list-origin))
  (setq expected 1)
  (should (= (safe-length result) expected))
  )

(provide 'ctrlnum)

;;; ctrlnum.el ends here
