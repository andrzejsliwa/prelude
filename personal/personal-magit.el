;;; package -- personal configuration for magit
;;; Commentary:
;;;    package URL: http://melpa.org/#/magit
;;; Code:

(require 'magit)
(setq magit-restore-window-configuration t)

(prelude-require-package 'magit-gh-pulls)
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

(defvar magit-hub-executable (when (executable-find "hub") "hub"))

(defadvice magit-git-command (around use-hub activate)
  "Use `hub' instead of `git' if available."
  (let ((magit-git-executable
         (or magit-hub-executable magit-git-executable)))
    ad-do-it))



(setq magit-fetch-arguments (quote ("--prune")))
(setq magit-merge-arguments (quote ("--no-ff")))
(setq magit-pull-arguments (quote ("--rebase")))
(setq magit-push-arguments (quote ("--set-upstream")))
(setq magit-log-arguments (quote ("--graph" "--color" "--decorate" "-n256")))


(provide 'personal-magit)
;;; personal-magit.el ends here
