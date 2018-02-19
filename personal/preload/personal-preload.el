;;; package --- personal preload
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes" prelude-dir))
(toggle-debug-on-error nil)
(setq prelude-theme 'dark-material)

(if (memq window-system '(mac ns))
    (progn (toggle-scroll-bar -1)
           (setq ns-use-native-fullscreen nil)
           (setq mac-command-modifier 'super)
           (setq mac-option-modifier 'meta)
           (set-face-attribute 'default nil :font "Anonymous Pro-20")))

(set-language-environment "UTF-8")
;;(mac-toggle-tab-bar)
;;(mac-auto-operator-composition-mode t)
(custom-set-variables
 '(helm-follow-mode-persistent t))

(require 'server)
(unless (server-running-p)
  (server-start))

(provide 'personal-preload)
;;; personal-preload.el ends here
