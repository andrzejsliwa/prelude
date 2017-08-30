;;; package -- personal configuration for swiper
;;; Commentary:
;;;   package URL: https://github.com/stanaka/dash-at-point
;;; Code:

(prelude-require-package 'swiper-helm)

;(global-set-key "\C-s" 'swiper-helm)
;(global-set-key "\C-r" 'swiper-helm)
(setq swiper-helm-display-function 'helm-default-display-buffer)

(provide 'personal-swiper)
;;; personal-swiper.el ends here
