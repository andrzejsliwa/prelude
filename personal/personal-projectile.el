;;; package -- personal projectile customization
;;; Commentary:
;;; Code:

(require 'projectile)

(defun projectile-dired-and-find-file (&optional arg)
  "Open `dired' at the root of the project and Jump to a file"
  (interactive "P")
  (projectile-dired)
  (projectile-find-file arg))

(setq projectile-use-native-indexing t)
(setq projectile-switch-project-action 'projectile-dired-and-find-file)
(setq projectile-require-project-root nil)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-files
      (append '("*.o"
                "*.beam")
              projectile-globally-ignored-files))


(provide 'personal-projectile)
;;; personal-projectile.el ends here
