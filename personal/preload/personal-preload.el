;;; package --- personal preload
;;; Commentary:
;;; Code:

(add-to-list 'custom-theme-load-path (expand-file-name "themes" prelude-dir))

(setq prelude-theme 'monokai)

(if (memq window-system '(mac ns))
    (progn (toggle-scroll-bar -1)
           (setq ns-use-native-fullscreen nil)
           (setq mac-command-modifier 'super)
           (setq mac-option-modifier 'meta)
           (set-face-attribute 'default nil :font "Inconsolata Awesome-20")))

(server-force-delete)
(server-start)


(provide 'personal-preload)
;;; personal-preload.el ends here
