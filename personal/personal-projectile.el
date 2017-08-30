;;; package -- personal projectile customization
;;; Commentary:
;;; Code:

(require 'projectile)

(setq projectile-use-native-indexing t)
(setq projectile-switch-project-action 'projectile-dired)
(setq projectile-require-project-root nil)
(setq projectile-enable-caching t)
(setq projectile-globally-ignored-files
      (append '("*.o"
                "*.beam")
              projectile-globally-ignored-files))


(provide 'personal-projectile)
;;; personal-projectile.el ends here
