;;; personal-elixir.el --- elixir basic setup.
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
(prelude-require-packages '(alchemist elixir-mode flycheck-elixir flycheck-mix))
(require 'elixir-mode)
(require 'flycheck-mix)
(require 'alchemist)

(defun insert-and-indent-line-above ()
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(defun custom-erlang-mode-hook ()
  (define-key erlang-mode-map (kbd "M-,") 'alchemist-goto-jump-back))

(add-hook 'erlang-mode-hook 'custom-erlang-mode-hook)

(defun personal-elixir-mode-hook ()
  (define-key elixir-mode-map (kbd "C-j") 'insert-and-indent-line-above)
  (alchemist-mode)
  (flycheck-mix-setup)
  (flycheck-mode))

(add-hook 'elixir-mode-hook 'personal-elixir-mode-hook)

(defun my-elixir-do-end-close-action (id action context)
  (when (eq action 'insert)
    (newline-and-indent)
    (forward-line -1)
    (indent-according-to-mode)))

(sp-with-modes '(elixir-mode)
  (sp-local-pair "fn" "end"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "do" "end"
                 :when '(("SPC" "RET"))
                 :post-handlers '(sp-ruby-def-post-handler)
                 :actions '(insert navigate)))

(sp-with-modes '(web-mode)
  (sp-local-pair "<%" "%>"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "<%=" "%>"
                 :when '(("SPC" "RET"))
                 :actions '(insert navigate))
  (sp-local-pair "<" nil
                 :when '(("SPC" "RET"))))

(setq alchemist-goto-erlang-source-dir "~/elixir/otp")
(setq alchemist-goto-elixir-source-dir "~/elixir/elixir")

(provide 'personal-elixir)
;;; personal-elixir.el ends here
