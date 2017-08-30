;;; package -- personal smart mode line customization
;;; Commentary:
;;; Code:

(prelude-require-package 'smart-mode-line)

(sml/setup)


(require 'smart-mode-line-dark-theme)
(custom-theme-set-faces
 'smart-mode-line-dark
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil)))
 '(mode-line-inactive ((t :foreground "gray60" :background "#3a3a3a" :inverse-video nil)))
 '(mode-line     ((t :foreground "gray60" :background "black" :inverse-video nil)))
 '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "White")))
 '(sml/filename  ((t :inherit sml/read-only :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "#c1f161")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue"))))

(custom-set-faces
 '(persp-selected-face ((t (:inherit sml/filename :foreground "DeepSkyBlue")))))


(setq sml/theme 'respectful)

(provide 'personal-smart-mode-line)
;;; personal-smart-mode-line.el ends here
