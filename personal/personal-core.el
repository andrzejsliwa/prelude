;;; package -- personal core functions
;;; Commentary:
;;; Code:

(defun personal-kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'."
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))


(defun personal-split-window-vertically-and-other-window ()
  "Split window vertically and go to it."
  (interactive)
  (split-window-vertically)
  (other-window 1))


(defun personal-split-window-horizontally-and-other-window ()
  "Split window horizontally and go to it."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(defun personal-indent-buffer ()
  "Indent the currently visited buffer."
  (interactive)
  (indent-region (point-min) (point-max)))

(defun personal-indent-region-or-buffer ()
  "Indent a region if selected, otherwise the whole buffer."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (indent-region (region-beginning) (region-end))
          (message "Indented selected region."))
      (progn
        (personal-indent-buffer)
        (message "Indented buffer.")))))


(defadvice keyboard-escape-quit (around my-keyboard-escape-quit activate)
  "Stops hidding windows on escape."
  (let (orig-one-window-p)
    (fset 'orig-one-window-p (symbol-function 'one-window-p))
    (fset 'one-window-p (lambda (&optional nomini all-frames) t))
    (unwind-protect
        ad-do-it
      (fset 'one-window-p (symbol-function 'orig-one-window-p)))))

(provide 'personal-core)
;;; personal-core.el ends here
