;;; package -- personal configuration for perspective
;;; Commentary:
;;;   package URL: https://github.com/nex3/perspective-el
;;;                https://github.com/bbatsov/projectile
;;; Code:

(prelude-require-packages '(perspective projectile persp-projectile))



(projectile-global-mode)
(require 'perspective)
(require 'persp-projectile)

;;(setq persp-initial-frame-name "none")
(persp-mode)
(global-set-key (kbd "M-q") 'persp-next)

(defun projectile-persp-switch-project (project-to-switch)
  "Switch to a project or perspective we have visited before.
If the perspective of corresponding project does not exist, this
function will call `persp-switch' to create one and switch to
that before `projectile-switch-project' invokes
`projectile-switch-project-action'.

Otherwise, this function calls `persp-switch' to switch to an
existing perspective of the project unless we're already in that
perspective."
  (interactive (list (projectile-completing-read "Switch to project: "
                                                 (projectile-relevant-known-projects))))
  (if project-to-switch
      (let* ((name (file-name-nondirectory (directory-file-name project-to-switch)))
             (persp (gethash name perspectives-hash)))
        (cond
         ;; project-specific perspective already exists
         ((and persp (not (equal persp persp-curr)))
          (persp-switch name))
         ;; project-specific perspective doesn't exist
         ((not persp)
          (let ((frame (selected-frame)))
            (persp-switch name)
            (projectile-switch-project-by-name project-to-switch)
            ;; Clean up if we switched to a new frame. `helm' for one allows finding
            ;; files in new frames so this is a real possibility.
            (when (not (equal frame (selected-frame)))
              (with-selected-frame frame
                (persp-kill name)))))))))

(define-key projectile-mode-map [remap projectile-switch-project] 'projectile-persp-switch-project)

(defun refresh-persp ()
  "refresh perspective"
  (let ((project-name (projectile-project-name)))
    (when (projectile-project-p)
      (persp-switch project-name))))

(add-hook 'dired-after-readin-hook #'refresh-persp)

(provide 'personal-perspective)
;;; personal-perspective.el ends here
