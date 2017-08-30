;;; package -- personal configuration for sql
;;; Commentary:
;;; Code:

(add-hook 'sql-interactive-mode-hook
          (function (lambda ()
                      (toggle-truncate-lines t)
                      (setq comint-output-filter-functions 'comint-truncate-buffer
                            comint-buffer-maximum-size 5000
                            comint-scroll-show-maximum-output t
                            comint-input-ring-size 500))))


(defun sql/connect ()
  (interactive)
  (load-library (expand-file-name "secure-storage.el.gpg" prelude-dir))

  (let*((connection (nth 0 (helm-comp-read "Select server: " (mapcar (lambda (item)
                                                                       (list
                                                                        (symbol-name (nth 0 item))
                                                                        (nth 0 item)))
                                                                     sql-connection-alist)))))
    (sql/run-connect connection)
    ))

(defun sql/run-connect (connection)
  "Connect to the input server using sql-connection-alist."
  (interactive)
  (let*((connection-info (assoc connection sql-connection-alist))
        (connection-product (nth 1 (nth 1 (assoc 'sql-product connection-info)))))
    (setq sql-connection-alist (assq-delete-all connection sql-connection-alist))
    (add-to-list 'sql-connection-alist connection-info)
    (setq sql-product connection-product)

    (if current-prefix-arg
        (sql-connect connection connection)
      (sql-connect connection)
      ))
  )

(provide 'personal-sql)
;;; personal-sql.el ends here
