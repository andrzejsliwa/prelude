;;; package --- personal plantuml configuration
;;; Commentary:
;;; Code:

(prelude-require-package 'plantuml-mode)

(dolist (r '("\\.plu$"
             "\\.uml$"
             "\\.pla$"))
  (add-to-list 'auto-mode-alist (cons r 'plantuml-mode)))

(setq plantuml-jar-path (expand-file-name "vendor/plantuml.jar" prelude-dir))

(eval-after-load "plantuml-mode"
  '(progn
     (defun plantuml-compile ()
       "Run plantuml over current file and open the result png."
       (interactive)
       (let ((file buffer-file-name))
         (shell-command (concat "java -jar '" plantuml-jar-path
                                "' '" file "' -tpng"))
         (shell-command (concat "open -a Preview " (concat (file-name-directory file)
                                                           (file-name-sans-extension
                                                            (file-name-nondirectory file))
                                                           ".png")))))
     (let ((map (make-sparse-keymap)))
       (define-key map "\C-c\C-c" 'plantuml-compile)
       (setq plantuml-mode-map map))))

(provide 'personal-plantuml)
;;; personal-plantuml.el ends here
