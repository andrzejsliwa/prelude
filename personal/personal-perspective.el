;;; package -- personal configuration for perspective
;;; Commentary:
;;;   package URL: https://github.com/nex3/perspective-el
;;;                https://github.com/bbatsov/projectile
;;; Code:

(prelude-require-packages '(perspective projectile persp-projectile))

(projectile-global-mode)
(require 'perspective)
(require 'persp-projectile)

(setq persp-initial-frame-name "none")
(persp-mode)
(global-set-key (kbd "M-q") 'persp-next)

(defun refresh-persp ()
  "refresh perspective"
  (let ((project-name (projectile-project-name)))
    (when (projectile-project-p)
      (persp-switch project-name))))

(add-hook 'dired-after-readin-hook #'refresh-persp)

(provide 'personal-perspective)
;;; personal-perspective.el ends here
