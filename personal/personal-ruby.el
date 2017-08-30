;;; package - personal ruby config
;;; Commentary:
;;; Code:

(prelude-require-packages '(rvm robe))

(require 'rvm)
(rvm-use-default)

(require 'robe)
(add-hook 'enh-ruby-mode-hook 'robe-mode)

(prelude-require-package 'ruby-refactor)
(require 'ruby-refactor)

(defadvice inf-ruby-console-auto (before activate-rvm-for-robe activate)
  (rvm-activate-corresponding-ruby))

(push 'company-robe company-backends)

(prelude-require-package 'enh-ruby-mode)
(require 'enh-ruby-mode)
(add-to-list 'auto-mode-alist '("\\.rb$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rake$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . enh-ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . enh-ruby-mode))

(add-to-list 'interpreter-mode-alist '("ruby" . enh-ruby-mode))

(prelude-require-package 'projectile-rails)
(add-hook 'projectile-mode-hook 'projectile-rails-on)


(modify-syntax-entry ?@ "w" enh-ruby-mode-syntax-table)
(modify-syntax-entry ?$ "w" enh-ruby-mode-syntax-table)
(modify-syntax-entry ?_ "w" enh-ruby-mode-syntax-table)
(modify-syntax-entry ?! "w" enh-ruby-mode-syntax-table)
(modify-syntax-entry ?? "w" enh-ruby-mode-syntax-table)

(provide 'personal-ruby)
;;; personal-ruby.el ends here
