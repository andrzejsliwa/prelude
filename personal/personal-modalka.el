;;; personal-modalka.el --- modelka config.
;;
;; Copyright © 2011-2014 Andrzej Sliwa
;;
;; Author: Andrzej Sliwa <andrzej.sliwa@i-tool.eu>
;; URL: https://github.com/andrzejsliwa/repo
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; comments

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

;; (prelude-require-packages '(modalka))
;; (add-hook 'after-init-hook 'myinit-modalka)

;; (defun myinit-modalka ()
;;   "My init."

;;   (define-key myinit-map (kbd "C-v") 'my-modalka-enable)

;;   (myinit-add-mode-to-hooks (lambda () (modalka-mode 1))
;;                               myinit-modal-modes-hooks)
;;   (add-hook 'modalka-mode-hook 'my-on-modalka-enable)

;;   (myinit-after-load 'modalka
;;     (setcar (cdr (assq 'modalka-mode minor-mode-alist)) " M")

;;     ;; (modalka-define-kbd "SPC" "C-v") ;DEL ESC RET TAB
;;     (define-key modalka-mode-map (kbd "SPC") (lambda ()
;;                                                (interactive)
;;                                                (modalka-mode -1)
;;                                                (my-on-modalka-disable)))

;;     ;; Nested prefixed namespaces.
;;     (define-key modalka-mode-map (kbd "c") mode-specific-map)
;;     (define-key modalka-mode-map (kbd "x") ctl-x-map)

;;     ;; First row.
;;     (modalka-define-kbd "`" "C-`")
;;     (modalka-define-kbd "1" "C-1")
;;     (modalka-define-kbd "2" "C-2")
;;     (modalka-define-kbd "3" "C-3")
;;     (modalka-define-kbd "4" "C-4")
;;     (modalka-define-kbd "5" "C-5")
;;     (modalka-define-kbd "6" "C-6")
;;     (modalka-define-kbd "7" "C-7")
;;     (modalka-define-kbd "8" "C-8")
;;     (modalka-define-kbd "9" "C-9")
;;     (modalka-define-kbd "0" "C-0")
;;     (modalka-define-kbd "-" "C--")
;;     (modalka-define-kbd "=" "C-=")

;;     ;; First row shifted.
;;     (modalka-define-kbd "~" "C-~")
;;     (modalka-define-kbd "!" "C-!")
;;     (modalka-define-kbd "@" "C-@")
;;     (modalka-define-kbd "#" "C-#")
;;     (modalka-define-kbd "$" "C-$")
;;     (modalka-define-kbd "%" "C-%")
;;     (modalka-define-kbd "^" "C-^")
;;     (modalka-define-kbd "&" "C-&")
;;     (modalka-define-kbd "*" "C-*")
;;     (modalka-define-kbd "(" "C-(")
;;     (modalka-define-kbd ")" "C-)")
;;     (modalka-define-kbd "_" "C-_")
;;     (modalka-define-kbd "+" "C-+")

;;     ;; Second row.
;;     (modalka-define-kbd "[" "C-[")
;;     (modalka-define-kbd "]" "C-]")
;;     (modalka-define-kbd "\\" "C-\\")

;;     ;; Second row shifted.
;;     (modalka-define-kbd "{" "C-{")
;;     (modalka-define-kbd "}" "C-}")
;;     (modalka-define-kbd "|" "C-|")

;;     ;; Third row.
;;     (modalka-define-kbd ";" "C-;")
;;     (modalka-define-kbd "'" "C-'")

;;     ;; Third row shifted.
;;     (modalka-define-kbd ":" "C-:")
;;     (modalka-define-kbd "\"" "C-\"")

;;     ;; Fourth row.
;;     (modalka-define-kbd "," "C-,")
;;     (modalka-define-kbd "." "C-.")
;;     (modalka-define-kbd "/" "C-/")

;;     ;; Fourth row shifted.
;;     (modalka-define-kbd "<" "C-<")
;;     (modalka-define-kbd ">" "C->")
;;     (modalka-define-kbd "?" "C-?")

;;     ;; Alphabet. Prefix keymaps `x' and `c' is an exception case.
;;     ;; `z' not used in Emacs.
;;     (modalka-define-kbd "a" "C-a")
;;     (modalka-define-kbd "b" "C-b")
;;     (modalka-define-kbd "d" "C-d")
;;     (modalka-define-kbd "e" "C-e")
;;     (modalka-define-kbd "f" "C-f")
;;     (modalka-define-kbd "g" "C-g")
;;     (modalka-define-kbd "h" "C-h")
;;     (modalka-define-kbd "i" "C-i")
;;     (modalka-define-kbd "j" "C-j")
;;     (modalka-define-kbd "k" "C-k")
;;     (modalka-define-kbd "l" "C-l")
;;     (modalka-define-kbd "m" "C-m")
;;     (modalka-define-kbd "n" "C-n")
;;     (modalka-define-kbd "o" "C-o")
;;     (modalka-define-kbd "p" "C-p")
;;     (modalka-define-kbd "q" "C-q")
;;     (modalka-define-kbd "r" "C-r")
;;     (modalka-define-kbd "s" "C-s")
;;     (modalka-define-kbd "t" "C-t")
;;     (modalka-define-kbd "u" "C-u")
;;     (modalka-define-kbd "v" "C-v")
;;     (modalka-define-kbd "w" "C-w")
;;     (modalka-define-kbd "y" "C-y")

;;     ;; Alphabet shifted. Prefix keymaps `X' and `C' is an exception case.
;;     ;; `Z' not used in Emacs.
;;     (modalka-define-kbd "A" "C-S-a")
;;     (modalka-define-kbd "B" "C-S-b")
;;     (modalka-define-kbd "D" "C-S-d")
;;     (modalka-define-kbd "E" "C-S-e")
;;     (modalka-define-kbd "F" "C-S-f")
;;     (modalka-define-kbd "G" "C-S-g")
;;     (modalka-define-kbd "H" "C-S-h")
;;     (modalka-define-kbd "I" "C-S-i")
;;     (modalka-define-kbd "J" "C-S-j")
;;     (modalka-define-kbd "K" "C-S-k")
;;     (modalka-define-kbd "L" "C-S-l")
;;     (modalka-define-kbd "M" "C-S-m")
;;     (modalka-define-kbd "N" "C-S-n")
;;     (modalka-define-kbd "O" "C-S-o")
;;     (modalka-define-kbd "P" "C-S-p")
;;     (modalka-define-kbd "Q" "C-S-q")
;;     (modalka-define-kbd "R" "C-S-r")
;;     (modalka-define-kbd "S" "C-S-s")
;;     (modalka-define-kbd "T" "C-S-t")
;;     (modalka-define-kbd "U" "C-S-u")
;;     (modalka-define-kbd "V" "C-S-v")
;;     (modalka-define-kbd "W" "C-S-w")
;;     (modalka-define-kbd "Y" "C-S-y")))

;; (defun my-modalka-enable ()
;;   "Enable `modalka-mode'."
;;   (interactive)

;;   (if (minibufferp)
;;       (keyboard-escape-quit)

;;     (when (and (buffer-file-name))
;;       ;; (buffer-modified-p)
;;       ;; (y-or-n-p (format "Save file %s? " (buffer-file-name)))
;;       (call-interactively 'save-buffer))

;;     (modalka-mode t)
;;     (keyboard-quit)))

;; (defun my-on-modalka-enable ()
;;   "Callback on modalka enable."
;;   (hl-line-mode -1))

;; (defun my-on-modalka-disable ()
;;   "Callback on modalka disable."
;;   (when (and (not (minibufferp))
;;              (member major-mode myinit-modal-modes))
;;     (hl-line-mode 1)))

(provide 'personal-modalka)
;;; personal-modalka.el ends here
