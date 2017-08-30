;;; package --- personal configuration for linum
;;; Commentary:
;;; Code:

(prelude-require-package 'linum-off)

(global-linum-mode)
(require 'linum-off)
(setq linum-disabled-modes-list
      '(erlang-shell eshell-mode wl-summary-mode
                     compilation-mode org-mode text-mode dired-mode))

(unless window-system
  (add-hook 'linum-before-numbering-hook
            (lambda ()
              (setq-local linum-format-fmt
                          (let ((w (length (number-to-string
                                            (count-lines (point-min) (point-max))))))
                            (concat "%" (number-to-string w) "d"))))))

(defun linum-format-func (line)
  (concat
   (propertize (format linum-format-fmt line) 'face 'linum)
   (propertize " " 'face 'mode-line-buffer-id)))
(unless window-system
  (setq linum-format 'linum-format-func))

(provide 'personal-linum)
;;; personal-linum.el ends here
