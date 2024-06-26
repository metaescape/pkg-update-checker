;;; pkg-update-checker.el --- Asynchronously check and notify about package updates

;; Author: metaescape <metaescape@foxmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "27.2") (async "1.9"))
;; Keywords: package-update-notification
;; URL: https://github.com/metaescape/pkg-update-checker

;;; Commentary:

;; This package provides a simple way to asynchronously check for package
;; updates in Emacs and notify the user via a buffer and system notifications.
;; It uses the `async` package to perform checks without blocking Emacs.

;;; Code:

(require 'async)
(require 'package)
(require 'server)

(defvar pkg-update-checker-interval-hour 24
  "Interval in hours between package update checks. Default is every 24 hours.")

(define-derived-mode pkg-list-mode special-mode "*Upgradable Packages*"
  "Special mode for a custom package list buffer."
  (use-local-map (copy-keymap special-mode-map))
  (define-key pkg-list-mode-map (kbd "r") 'async-check-and-notify-upgradable-packages)
  )

(defun create-pkg-list-buffer (pkg-info)
  "Create and display a buffer listing packages with their paths, allowing description on click.
PKG-INFO is a list of lists, each containing a package name and its path."
  (let ((buffer (get-buffer "*Upgradable Packages*")))
    (when buffer
      (kill-buffer buffer)))
  (with-current-buffer (get-buffer-create "*Upgradable Packages*")
    ;;(read-only-mode -1)
    (erase-buffer)
    (goto-char (point-min))
    (insert (format "%-40s %s\n" "click to backup" "goto backup version"))
    (insert (make-string 80 ?-))
    (insert "\n")
    (dolist (pkg pkg-info)
      (let* ((pkg-name (car pkg))
             (pkg-version (cadr pkg))
             (pkg-path (caddr pkg))
             (pkg-name-version (format "%s%s" (symbol-name pkg-name) pkg-version))
             (len (length pkg-name-version))
             (backup-dir (concat (file-name-directory  pkg-path) "backups/"))
             (backup-file-name (file-name-nondirectory pkg-path)))

        (unless (file-directory-p backup-dir)
          (make-directory backup-dir t))
        (message pkg-path)
        (message backup-dir)
        (insert-button
         pkg-name-version
         'action `(lambda (x)
                    (when (file-directory-p ,pkg-path)
                      (copy-directory ,pkg-path ,backup-dir t)
                      (message (format "%s backuped" ,backup-file-name))
                      (package-upgrade (intern ,(symbol-name pkg-name)))))
         'follow-link t
         'help-echo "Click to see package description")
        (insert (make-string (- 40 len) ? ))
        (insert-button
         backup-file-name
         'action `(lambda (x)
                    (progn
                      (find-file ,backup-dir)
                      (re-search-forward
                       (regexp-quote ,backup-file-name))))
         'follow-link t
         'help-echo "Click to go to old path")
        (insert "\n")))
    (pkg-list-mode)
    (display-buffer (current-buffer))))

(defmacro check-upgradable-packages-macro ()
  "A simple test function that returns a string."
  `(progn
     (defun check-upgradable-packages-function ()
       "return a list of upgradable packages in the following format: 
((package-name version path-of-old-version), (), ...)
"
       (require 'package)
       (require 'cl-lib)
       (setq package-archives (quote ,package-archives))
       (package-initialize)
       (package-refresh-contents) ;; updates soucelist
       (let ((upgradable-packages '()))
         (dolist (pkg-desc package-alist)
           (let* ((pkg (car pkg-desc))
                  (current-pkg-desc (cadr pkg-desc))
                  (archive-pkg-desc (cadr (assq pkg package-archive-contents))))
             (when (and archive-pkg-desc
                        (version-list-< (package-desc-version current-pkg-desc)
                                        (package-desc-version archive-pkg-desc)))
               (let ((archive-version (package-desc-version archive-pkg-desc))
                     (current-path (package-desc-dir current-pkg-desc)))
                 (push (list pkg archive-version current-path) upgradable-packages)))))
         (nreverse upgradable-packages)))))

(defun parse-upgradable-packages-and-notify (upgradable-packages)
    (when upgradable-packages
      (create-pkg-list-buffer upgradable-packages))
    (let ((msg
           (if upgradable-packages
               (format "The following packages have updates available:\n %s"
                       (mapconcat (lambda (pkg) (symbol-name (car pkg)))
                                  upgradable-packages "\n"))
             "🎉All packages are newest"
             )))
      (require 'notifications)
      (notifications-notify
       :title "package-update-notification"
       :body msg
       :timeout (* org-show-notification-timeout 1000)
       :urgency 'low)))

(defun async-check-and-notify-upgradable-packages ()
  "Asynchronously check for upgradable packages and notify the user."
  (interactive)
  (async-start
   ;; call in child process, so this should be a macro
   `(lambda ()
      ,(macroexpand '(check-upgradable-packages-macro))
      ;; 调用函数
      (check-upgradable-packages-function))
   ;; call after await, a normal function
   (lambda (upgradable-packages)
     (parse-upgradable-packages-and-notify upgradable-packages))))

;;;###autoload
(defun start-pkg-update-checker-timer ()
  "Start a timer to check for package updates using the user-configured interval."
  (if server-mode
      (progn 
        (run-with-timer
         10
         (* pkg-update-checker-interval-hour 3600)
         'async-check-and-notify-upgradable-packages)
        (message "Package updater timer started with interval %s hours."
                 pkg-update-checker-interval-hour))
    (message "Not in server-mode, No need to start pkg-update-checker-timer" )))

(provide 'pkg-update-checker)
;;; pkg-update-checker.el ends here
(async-check-and-notify-upgradable-packages)
