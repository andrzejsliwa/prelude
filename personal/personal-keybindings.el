;;; package -- personal keybindings customization
;;; Commentary:
;;; Code:

(global-set-key
 (kbd "C-x 2") 'personal-split-window-vertically-and-other-window)
(global-set-key
 (kbd "C-x 3") 'personal-split-window-horizontally-and-other-window)
(global-unset-key (kbd "C-x 4"))
(global-unset-key (kbd "C-x 5"))
(global-set-key (kbd "C-x 5") 'transpose-frame)
(global-set-key (kbd "C-x 4") 'balance-windows)
(global-set-key (kbd "C-q") 'bury-buffer)
(global-set-key (kbd "C-c G") 'magit-file-popup)
(global-set-key (kbd "C-c M-g") 'magit-dispatch-popup)

(global-set-key (kbd "M-=") 'er/expand-region)
(global-set-key (kbd "M--") 'er/contract-region)
(global-set-key (kbd "C-c C-b") 'projectile-compile-project)
(global-set-key (kbd "C-w") 'personal-kill-region-or-backward-kill-word)
(global-set-key (kbd "M-i") 'crux-cleanup-buffer-or-region)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)

(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "C-x m") 'browse-url-at-point)
(global-set-key (kbd "M-m") 'multi-term)

(global-set-key (kbd "<s-return>") 'toggle-frame-fullscreen)
(global-set-key (kbd "C-=") 'increase-default-font-height)
(global-set-key (kbd "C--") 'decrease-default-font-height)

(global-set-key (kbd "s-d") 'helm-projectile-find-dir)
(global-set-key (kbd "s-r") 'helm-recentf)
(global-set-key (kbd "s-t") 'helm-projectile-find-file)
(global-set-key (kbd "s-f") 'helm-projectile-ag)
(global-set-key (kbd "s-p") 'helm-projectile-switch-project)

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)

(key-chord-define-global ";f" 'helm-projectile-find-file)
(key-chord-define-global ";r" 'helm-projectile-recentf)
(key-chord-define-global ";p" 'helm-projectile-switch-project)
(key-chord-define-global ";d" 'projectile-dired)
(key-chord-define-global ";g" 'magit-status)
(key-chord-define-global ";s" 'helm-projectile-ag)
(key-chord-define-global ";n" 'ace-window)
(key-chord-define-global ";m" 'helm-mini)
(key-chord-define-global ";b" 'ace-jump-buffer)
(key-chord-define-global ";l" 'persp-switch)
(key-chord-define-global ";u" 'undo)
(key-chord-define-global ";w" 'save-buffer)
(key-chord-define-global ";W" 'write-file)
(key-chord-define-global ";q" 'bury-buffer)
(key-chord-define-global ";Q" 'crux-switch-to-previous-buffer)
(key-chord-define-global ";;" 'avy-goto-word-1)
(key-chord-define-global ";0" 'delete-window)
(key-chord-define-global ";1" 'delete-other-windows)
(key-chord-define-global ";2" 'personal-split-window-vertically-and-other-window)
(key-chord-define-global ";3" 'personal-split-window-horizontally-and-other-window)
(key-chord-define-global ";4" 'balance-windows)
(key-chord-define-global ";5" 'transpose-frame)
(key-chord-define-global ";c" (key-binding (kbd "C-c")))
(key-chord-define-global ";x" 'save-buffers-kill-terminal)
(key-chord-unset-global "jk")
(key-chord-define-global "jk" 'sr-speedbar-toggle)

(global-set-key
 (kbd "C-c v")
 (lambda ()
   (interactive)
   (set-face-attribute 'default nil :font "C64 Pro Mono-14")))

(global-set-key
 (kbd "C-c x")
 (lambda ()
   (interactive)
   (set-face-attribute 'default nil :font "Anonymous Pro-20")))

(provide 'personal-keybindings)
;;; personal-keybindings.el ends here
