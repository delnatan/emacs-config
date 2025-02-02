;;; venv-manager.el --- Manage Python virtual environments -*- lexical-binding: t -*-

(require 'pythonic)

(defgroup venv-manager nil
  "Python virtual environment manager for Emacs."
  :group 'python)

(defcustom venv-manager-directories
  (list "~/.virtualenvs")
  "List of directories containing Python virtual environments."
  :type '(repeat directory)
  :group 'venv-manager)

(defcustom venv-manager-preactivate-hook nil
  "Hook run before a virtual environment is activated."
  :type 'hook
  :group 'venv-manager)

(defcustom venv-manager-postactivate-hook nil
  "Hook run after a virtual environment is activated."
  :type 'hook
  :group 'venv-manager)

(defcustom venv-manager-predeactivate-hook nil
  "Hook run before a virtual environment is deactivated."
  :type 'hook
  :group 'venv-manager)

(defcustom venv-manager-postdeactivate-hook nil
  "Hook run after a virtual environment is deactivated."
  :type 'hook
  :group 'venv-manager)

(defvar venv-manager-current-venv nil
  "Path to currently activated virtual environment.")

(defvar eshell-path-env)

(defun venv-manager--get-bin-path (venv-path)
  "Get the bin directory path for VENV-PATH."
  (expand-file-name "bin" venv-path))

(defun venv-manager--update-paths (venv-path)
  "Update PATH-like variables to include VENV-PATH bin directory."
  (let* ((bin-path (venv-manager--get-bin-path venv-path))
         (path-separator (if (eq system-type 'windows-nt) ";" ":"))
         (path-entries (split-string (getenv "PATH") path-separator)))
    ;; Update exec-path
    (setq exec-path (cons bin-path (delete bin-path exec-path)))
    ;; Update PATH environment variable
    (setenv "PATH" (concat bin-path path-separator
                           (string-join (delete bin-path path-entries) path-separator)))
    ;; Update eshell-path-env
    (setq eshell-path-env (getenv "PATH"))
    ;; Set VIRTUAL_ENV environment variable
    (setenv "VIRTUAL_ENV" venv-path)))

(defun venv-manager--restore-paths (venv-path)
  "Remove VENV-PATH bin directory from PATH-like variables."
  (let* ((bin-path (venv-manager--get-bin-path venv-path))
         (path-separator (if (eq system-type 'windows-nt) ";" ":"))
         (path-entries (split-string (getenv "PATH") path-separator)))
    ;; Remove from exec-path
    (setq exec-path (delete bin-path exec-path))
    ;; Remove from PATH environment variable
    (setenv "PATH" (string-join (delete bin-path path-entries) path-separator))
    ;; Update eshell-path-env
    (setq eshell-path-env (getenv "PATH"))
    ;; Unset VIRTUAL_ENV
    (setenv "VIRTUAL_ENV" nil)))

(defun venv-manager--list-venvs ()
  "Get list of available virtual environments.
Returns an alist where key is the venv name and value is the full path."
  (let ((venvs '()))
    (dolist (dir venv-manager-directories)
      (when (file-directory-p (expand-file-name dir))
        (dolist (venv (directory-files (expand-file-name dir)))
          (let ((venv-path (expand-file-name venv dir)))
            (when (and (file-directory-p venv-path)
                       (file-exists-p (expand-file-name "bin/python" venv-path)))
              (push (cons venv venv-path) venvs))))))
    venvs))

;;;###autoload
(defun activate-venv (&optional venv-name)
  "Activate a Python virtual environment using pythonic.
If VENV-NAME is provided, activate that environment directly.
VENV-NAME can be either the name of the environment or its full path."
  (interactive)
  (let* ((venvs (venv-manager--list-venvs))
         (venv-path
          (cond
           ((and venv-name (file-directory-p venv-name)) venv-name)
           (venv-name (or (cdr (assoc venv-name venvs))
                          (error "Environment %s not found" venv-name)))
           (t (cdr (assoc (completing-read "Choose virtual environment: " venvs nil t)
                          venvs))))))
    
    ;; Deactivate current venv if one is active
    (when venv-manager-current-venv
      (deactivate-venv))
    
    ;; Run pre-activation hooks
    (run-hooks 'venv-manager-preactivate-hook)
    
    ;; Set the new environment
    (setq venv-manager-current-venv venv-path)
    (setq python-shell-virtualenv-root venv-path)
    
    ;; Update all path-related variables
    (venv-manager--update-paths venv-path)
    
    ;; Use pythonic for activation
    (pythonic-activate venv-path)
    
    ;; Run post-activation hooks
    (run-hooks 'venv-manager-postactivate-hook)
    
    (message "Activated virtual environment: %s" 
             (if venv-name venv-name (file-name-nondirectory venv-path)))))

;;;###autoload
(defun deactivate-venv ()
  "Deactivate the current virtual environment."
  (interactive)
  (when venv-manager-current-venv
    ;; Run pre-deactivation hooks
    (run-hooks 'venv-manager-predeactivate-hook)
    
    ;; Restore paths
    (venv-manager--restore-paths venv-manager-current-venv)
    
    ;; Use pythonic for deactivation
    (pythonic-deactivate)
    
    ;; Clear variables
    (setq python-shell-virtualenv-root nil)
    (setq venv-manager-current-venv nil)
    
    ;; Run post-deactivation hooks
    (run-hooks 'venv-manager-postdeactivate-hook)
    
    (message "Deactivated virtual environment")))

(provide 'venv-manager)
;;; venv-manager.el ends here
