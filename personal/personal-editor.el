;;; package -- personal editor customization
;;; Commentary:
;;; Code:

;; hide menu
(menu-bar-mode -1)

;; enable mouse in GUI and terminal
(require 'mouse)
(xterm-mouse-mode t)

(set-default 'truncate-lines -1)

(global-hl-line-mode -1)

;; enable guru mode
(setq guru-warn-only nil)

;; default tab width
(setq-default tab-width 4)
;; hide cursor in other windows
(setq cursor-in-non-selected-windows nil)
;; disable lockfiles
(setq create-lockfiles  nil)

(prelude-require-package 'control-mode)
(require 'control-mode)
(control-mode-default-setup)

(add-hook 'control-mode-hook
          (lambda ()
            (set-face-background 'mode-line (if control-mode
                                               "#562aa6"
                                               nil
                                               ))))

(prelude-require-packages '(tldr))
(require 'tldr)


(provide 'personal-editor)
;;; personal-editor.el ends here
