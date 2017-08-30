;;; package -- personal configuration for multiterm
;;; Commentary:
;;; Code:

(prelude-require-package 'multi-term)
(require 'multi-term)
;; (setq multi-term-program-switches "--login")
;; (setq multi-term-program "/usr/local/bin/fish")

(add-hook 'term-mode-hook (lambda()
                            (define-key term-raw-map (kbd "C-y") 'term-paste)
                            (define-key term-raw-map (kbd "s-v") 'term-paste)
                            (yas-minor-mode -1)))

(global-set-key (kbd "s-m") 'multi-term)

(provide 'personal-mutliterm)
;;; personal-multiterm.el ends here
