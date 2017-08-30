;;; package -- personal configuration for deft
;;; Commentary:
;;;   package URL: look in vendor
;;; Code:

(require 'deft)
(setq deft-auto-save-interval 10.0)
(setq deft-extension "org")
(setq deft-text-mode 'org-mode)
(setq deft-directory "~/Dropbox/Notes")
(setq org-src-fontify-natively t)
(setq deft-recursive t)

(key-chord-define-global "dk" 'deft)

(provide 'personal-deft)
;;; personal-deft.el ends here
