;;; package -- personal configuration for zoom-window
;;; Commentary:
;;; Code:

(prelude-require-package 'zoom-window)
(require 'zoom-window)
(setq zoom-window-mode-line-color "DeepSkyBlue")

(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)

(provide 'personal-zoom-window)
;;; personal-zoom-window.el ends here
