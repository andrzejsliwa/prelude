;;; package -- personal configuration for dash
;;; Commentary:
;;;   package URL: https://github.com/stanaka/dash-at-point
;;; Code:

(prelude-require-package 'dash-at-point)
(require 'dash-at-point)

(global-set-key "\C-cd" 'dash-at-point)
(global-set-key "\C-ce" 'dash-at-point-with-docset)

(provide 'personal-dash)
;;; personal-dash.el ends here
