;;; package -- personal configuration for perspective
;;; Commentary:
;;;   package URL: https://github.com/nex3/perspective-el
;;;                https://github.com/bbatsov/projectile
;;; Code:

(prelude-require-packages '(perspective projectile persp-projectile))

(require 'perspective)
(require 'persp-projectile)

(persp-mode)
(setq persp-initial-frame-name "none")
(global-set-key (kbd "M-q") 'persp-next)

(provide 'personal-perspective)
;;; personal-perspective.el ends here
