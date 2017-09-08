;;; package --- user interface customizations
;;; Commentary:
;;; Code:

;; disable ring bell
(setq ring-bell-function 'ignore)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))
        (:eval (if (buffer-modified-p)
                   " ⁕"))
        " - Emacs")
      )

(set-fringe-mode '(nil . 0))
(set-display-table-slot standard-display-table 0 ?\ )

;;(toggle-scroll-bar -1)
(tool-bar-mode -1)
(setq beacon-mode nil)
(blink-cursor-mode 1)
(prelude-require-package 'symon)
(require 'symon)
(setq symon-sparkline-type 'plain)
(symon-mode 1)

(setq cursor-in-non-selected-windows nil)
(set-cursor-color "#F92672")
(setq-default cursor-type 'bar)

(prelude-require-package 'sublimity)
(require 'sublimity)
;;(require 'sublimity-map)
(require 'sublimity-scroll)
;;(require 'sublimity-attractive)
(sublimity-mode 1)

(add-hook 'after-make-frame-functions
          '(lambda (frame)
             (modify-frame-parameters frame
                                      '((vertical-scroll-bars . nil)
                                        (horizontal-scroll-bars . nil)))))
(custom-theme-set-faces
 'smart-mode-line-dark
 '(mode-line-buffer-id ((t :inherit sml/filename :foreground nil :background nil)))
 '(mode-line-inactive ((t :foreground "gray60" :background nil :inverse-video nil)))
 '(mode-line     ((t :foreground "gray60" :background nil :inverse-video nil)))
 '(sml/global    ((t :foreground "gray50" :inverse-video nil)))
 '(sml/modes     ((t :inherit sml/global :foreground "White")))
 '(sml/filename  ((t :inherit sml/read-only :weight bold)))
 '(sml/prefix    ((t :inherit sml/global :foreground "#c1f161")))
 '(sml/read-only ((t :inherit sml/not-modified :foreground "DeepSkyBlue")))
 '(persp-selected-face ((t :inherit sml/read-only)))
 '(helm-candidate-number ((t :foreground nil :background nil :inherit sml/filename))))

(provide 'personal-ui)
;;; personal-ui ends here
