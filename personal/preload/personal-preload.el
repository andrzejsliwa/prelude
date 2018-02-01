;;; package --- personal preload
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes" prelude-dir))
;(toggle-debug-on-error t)
(setq prelude-theme 'dark-material)

(if (memq window-system '(mac ns))
    (progn (toggle-scroll-bar -1)
           (setq ns-use-native-fullscreen nil)
           (setq mac-command-modifier 'super)
           (setq mac-option-modifier 'meta)
           (set-face-attribute 'default nil :font "Fira Code-18")))

(custom-set-variables
 '(helm-follow-mode-persistent t))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'personal-preload)
;;; personal-preload.el ends here
