;;; package -- org-mode customizations
;;; Commentary:
;;; Code:

(setq
 ;; Activate speed keys
 org-use-speed-commands t
 ;; Close item with timestamp
 org-log-done 'time
 ;; Don't use `outline-path-completion'. `ido-imenu' is better
 org-goto-interface 'outline
 ;; hide leading stars
 org-hide-leading-stars t
 ;; indent mode
 org-startup-indented t
 )

(provide 'personal-org)
;;; personal-org.el ends here
