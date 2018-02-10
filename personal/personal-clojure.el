;;; package --- personal clojure configuration
;;; Commentary:
;;; Code:

(prelude-require-packages '(clojure-mode
                            clojure-mode-extra-font-locking
                            cider
                            clj-refactor
                            clojure-cheatsheet
                            cider-decompile))
;(dolist (r '("\\.cljs$" "\\.cljx$" "\\.edn$"))
;  (add-to-list 'auto-mode-alist (cons r 'clojure-mode)))

;; cider
(require 'cider)

(setq cider-repl-result-prefix ";; => ")
(setq cider-repl-pop-to-buffer-on-connect 'display-only)
(setq cider-prompt-for-symbol nil)
(setq cider-repl-use-pretty-printing t)
(setq cider-repl-print-length 50)
(setq cider-repl-wrap-history t)
(setq cider-repl-history-size 3000)
(setq cider-repl-history-file "~/.cider-history")
(setq cider-repl-display-help-banner nil)

(add-hook 'cider-repl-mode-hook #'eldoc-mode)
(define-key cider-repl-mode-map (kbd "RET") #'cider-repl-newline-and-indent)
(define-key cider-repl-mode-map (kbd "C-<return>") #'cider-repl-return)

;; company mode for completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)


(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

;(eval-after-load 'find-file-in-project
;  '(add-to-list 'ffip-patterns "*.clj"))

(require 'clj-refactor)
(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (setq cljr-warn-on-eval nil)
  (yas-minor-mode 1) ; for adding require/use/import
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "C-c C-n") #'cider-pprint-eval-last-sexp-to-comment)
     (define-key clojure-mode-map (kbd "C-c C-h") #'clojure-cheatsheet)))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; (defun tdd-test ()
;;   "Thin wrapper around `cider-test-run-tests'."
;;   (when (cider-connected-p)
;;     (cider-test-run-tests nil)))

;; (define-minor-mode tdd-mode
;;   "Run all tests whenever a file is saved."
;;   nil " TDD" nil
;;   (if tdd-mode
;;       (add-hook 'after-save-hook #'tdd-test nil 'local)
;;     (remove-hook 'after-save-hook #'tdd-test 'local)))

;; (add-hook 'cider-mode-hook #'tdd-mode)
;; (add-hook 'cider-repl-mode-hook #'tdd-mode)

(custom-set-faces
 '(cider-test-failure-face ((t (:foraground "#f92672" :background nil))))
 '(cider-test-success-face ((t (:foreground "#a6e22e" :background nil)))))

(eval-after-load 'clojure-mode
  (font-lock-add-keywords
   'clojure-mode `(("(\\(fn\\)[\[[:space:]]"
                    (0 (progn (compose-region (match-beginning 1)
                                              (match-end 1) "λ")
                              nil))))))

(eval-after-load 'clojure-mode
  (font-lock-add-keywords
   'clojurescript-mode `(("(\\(fn\\)[\[[:space:]]"
                          (0 (progn (compose-region (match-beginning 1)
                                                    (match-end 1) "λ")
                                    nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\)("
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "ƒ")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojurescript-mode `(("\\(#\\)("
                           (0 (progn (compose-region (match-beginning 1)
                                                     (match-end 1) "ƒ")
                                     nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojure-mode `(("\\(#\\){"
                     (0 (progn (compose-region (match-beginning 1)
                                               (match-end 1) "∈")
                               nil))))))

(eval-after-load 'clojure-mode
  '(font-lock-add-keywords
    'clojurescript-mode `(("\\(#\\){"
                           (0 (progn (compose-region (match-beginning 1)
                                                     (match-end 1) "∈")
                                     nil))))))

(provide 'personal-clojure)
;;; personal-clojure.el ends here
