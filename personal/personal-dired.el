;;; package -- personal dired customization
;;; Commentary:
;;; Code:

;; (prelude-require-packages '(dired-details))

;; enable dired-details extension
(require 'dired-details)
(dired-details-install)

;; dired - reuse current buffer by pressing 'a'
(put 'dired-find-alternate-file 'disabled nil)

;; if there is a dired buffer displayed in the next window, use its
;; current subdir, instead of the current subdir of this dired buffer
(setq dired-dwim-target t)

;; always delete and copy recursively
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

;; disable dired hidden string
(setq-default dired-details-hidden-string "")

;; define hidden extensions
(setq-default dired-omit-extensions
              '(".pyc" ".beam" ".class" ".o" "~" ".dvi" ".aux" ".elc" ".iml"))

;; enable omit mode always
(defun turn-on-omit-mode ()
  "Hide files with defined extensions in dired mode"
  (require 'dired-x)
  (dired-omit-mode 1))
(add-hook 'dired-mode-hook 'turn-on-omit-mode)

;; use gnu ls
;; $ brew install coreutils
(require 'ls-lisp)
(setq ls-lisp-use-insert-directory-program t)
(setq insert-directory-program "/usr/local/bin/gls")
;; sort - directories on top
(setq dired-listing-switches "-aBhl  --group-directories-first")

(require 'dired)
(require 'dash)
;; move files between split panes
(setq dired-dwim-target t)

;; reload dired after making changes
(--each '(dired-do-rename
          dired-do-copy
          dired-create-directory
          wdired-abort-changes)
  (eval `(defadvice ,it (after revert-buffer activate)
           (revert-buffer))))

(define-key dired-mode-map (kbd "C-l") 'dired-up-directory)

(provide 'personal-dired)
;;; personal-dired.el ends here
