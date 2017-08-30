;;; package -- personal configuration for emf
;;; Commentary:
;;;   package URL: https://github.com/chrisbarrett/emacs-refactor
;;; Code:

(prelude-require-package 'emr)
(add-hook 'prog-mode-hook 'emr-initialize)

(define-key prog-mode-map (kbd "M-RET") 'emr-show-refactor-menu)

(provide 'personal-emf)
;;; personal-emf.el ends here
