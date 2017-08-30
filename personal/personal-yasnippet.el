;;; package -- personal configuration for yasnippet
;;; Commentary:
;;;   package URL: https://github.com/capitaomorte/yasnippet
;;; Code:

(prelude-require-package 'yasnippet)
(require 'yasnippet)
(add-to-list 'yas-snippet-dirs (expand-file-name "snippets" prelude-dir))
(push '(company-semantic :with company-yasnippet) company-backends)
(yas-global-mode 1)

(global-set-key (kbd "C-c s n") 'yas-new-snippet)
(global-set-key (kbd "C-c s i") 'yas-insert-snippet)
(global-set-key (kbd "C-c s v") 'yas-visit-snippet-file)

(provide 'personal-yasnippet)
;;; personal-yasnippet.el ends here
