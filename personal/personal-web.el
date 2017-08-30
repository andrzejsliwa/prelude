;;; package --- personal web mode configuration
;;; Commentary:
;;; Code:

;; connect it to specific file types
(dolist (r '("\\.erb$"
             "\\.scss$"
             "\\.hbs$"
             "\\.html.eex$"
             "\\.html?\\'"))
  (add-to-list 'auto-mode-alist (cons r 'web-mode)))

(require 'web-mode)
;; configure identation
(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(provide 'personal-web)
;;; personal-web.el ends here
