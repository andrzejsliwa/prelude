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
(require 'clojure-mode-extra-font-locking)

;; (setq cider-repl-popup-stacktraces nil)
;; (setq cider-test-show-report-on-success t)
;; (setq cider-auto-select-error-buffer t)
;; (setq cider-repl-display-help-banner nil)

(setq cider-repl-history-file "~/.cider-history")

;; nice pretty printing
;; (setq cider-repl-use-pretty-printing t)

;; nicer font lock in REPL
;; (setq cider-repl-use-clojure-font-lock t)

;; result prefix for the REPL
;; (setq cider-repl-result-prefix ";; => ")

;; never ending REPL history
;; (setq cider-repl-wrap-history t)

;; looong history
;; (setq cider-repl-history-size 3000)

;; eldoc for clojure
;; (add-hook 'cider-mode-hook #'eldoc-mode)

;; error buffer not popping up
;; (setq cider-show-error-buffer t)

;; company mode for completion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)


(add-hook 'cider-repl-mode-hook 'subword-mode)
(add-hook 'cider-repl-mode-hook 'smartparens-mode)
(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'clojure-mode-hook #'smartparens-strict-mode)

(eval-after-load 'find-file-in-project
  '(add-to-list 'ffip-patterns "*.clj"))

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
