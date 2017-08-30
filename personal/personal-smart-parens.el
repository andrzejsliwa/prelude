;;; package --- personal configuration for smart-parens
;;; Commentary:
;;; Code:

(add-hook 'scheme-mode-hook 'smartparens-strict-mode)
(add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
(add-hook 'lisp-mode-hook 'smartparens-strict-mode)

(provide 'personal-smart-parens)
;;; personal-smart-parens.el ends here
