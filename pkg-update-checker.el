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
  (use-local-map (copy-keymap special-mode-map)))

(defun create-pkg-list-buffer (pkg-names)
  "Create and display a buffer listing packages, allowing description on click.
PKG-NAMES is a list of package names (symbols or strings)."
  (with-current-buffer (get-buffer-create "*Upgradable Packages*")
    (read-only-mode -1)
    (erase-buffer)
    (goto-char (point-min))
    (dolist (pkg pkg-names)
      (let ((pkg-str (if (symbolp pkg) (symbol-name pkg) pkg)))
        (insert-button pkg-str
                       'action `(lambda (x) (describe-package (intern ,pkg-str)))
                       'follow-link t
                       'help-echo "Click to see package description")
        (insert "\n")))
    (pkg-list-mode)
    (display-buffer (current-buffer))))

;; (create-pkg-list-buffer '(org-roam magit flycheck)) 
;; (create-pkg-list-buffer '("dash" "magit" "flycheck"))

(defun async-check-and-notify-upgradable-packages ()
  "Asynchronously check for upgradable packages and notify the user."
  (async-start
   `(lambda ()
      ;; 明确加载 pkg-update-checker.el 文件
      (require 'package)
      (package-initialize)
      (package-refresh-contents) ;; updates soucelist
      (let ((upgradable-packages
             (cl-remove-if-not
              (lambda (p)
                (let ((new-version-available
                       (ignore-errors (package-desc-vers
                                       (cadr
                                        (assq p package-archive-contents))))))
                  (and new-version-available
                       (version-list-< (package-desc-version (cadr (assq p package-alist))) new-version-available))))
              (mapcar #'car package-alist))))
        upgradable-packages))
   (lambda (upgradable-packages)
     (if upgradable-packages
         (progn 
           (create-pkg-list-buffer upgradable-packages)
           (let ((msg (format "The following packages have updates available:\n %s"
                              (mapconcat 'symbol-name upgradable-packages "\n"))))
             (start-process "package-update-notification"
                            nil
                            "notify-send"
                            "Emacs Package Updates Available" msg)))
       ;;else
       (start-process "package-update-notification"
                      nil
                      "notify-send"
                      "From Emacs Pkg-Update-checker"
                      "🎉All packages are newest")))))

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
