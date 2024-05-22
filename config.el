(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)
(setq custom-safe-themes t)
(column-number-mode)

;; use visual-line-mode only when programming or in org-mode
(add-hook 'prog-mode-hook 'visual-line-mode)
(add-hook 'org-mode-hook 'visual-line-mode)

;; use smooth scrolling like most modern apps
(pixel-scroll-precision-mode)
;; use y/n for yes-or-no
;; since Emacs 29 `yes-or-no-p` will use `y-or-n-p`
(setopt use-short-answers t) 

;; I only want to show line numbers in programming mode.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; keep things tidy
(use-package no-littering
  :straight t)

;; for programming set fill to 80 columns and display column indicator
(setq-default fill-column 79)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; enable recent-file mode
(recentf-mode 1)
(setq recentf-max-saved-items 30)

;; Open recent files 
(global-set-key (kbd "C-c r") 'recentf-open-files)

;; map C-x C-b to `ibuffer`
(define-key ctl-x-map "\C-b" 'ibuffer)

;; set dired to group directories first
(setq dired-listing-switches "-al --group-directories-first")

;; refresh dired if file is changed on disk
(setq dired-auto-revert-buffer t)

;; enable side scrolling with mouse wheel
(setq mouse-wheel-tilt-scroll t)
;; reverse mouse direction for a more natural swipe feel
(setq mouse-wheel-flip-direction t)

;; create a new frame with *scratch* buffer
(defun new-frame ()
  (interactive)
  (select-frame (make-frame))
  (switch-to-buffer "*scratch*"))
(global-set-key (kbd "M-n") 'new-frame)
(global-set-key (kbd "M-`") 'other-frame)

;; Kill current buffer (instead of asking first buffer name)
(global-set-key (kbd "C-x k") 'kill-current-buffer)

;; unbind annoying text-scaling with Ctrl-mouse wheel
(global-unset-key (kbd "C-<wheel-up>"))
(global-unset-key (kbd "C-<wheel-down>"))
(global-unset-key (kbd "C-<mouse-5>"))
(global-unset-key (kbd "C-<mouse-4>"))

;; Close frame if not the last, kill emacs else

(defun de/delete-frame-or-kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'de/delete-frame-or-kill-emacs)

;; Enable ibuffer
(require 'ibuffer)

;; Define custom groups for ibuffer
;; add commonly used modes below
(setq ibuffer-saved-filter-groups
      '(("default"
         ("Dired" (mode . dired-mode))
         ("Python" (or (mode . python-mode)
                       (mode . python-ts-mode)))
         ("Org" (mode . org-mode))
	 ("Jupyter" (mode . jupyter-repl-mode))
         ("Emacs" (or (name . "^\\*scratch\\*$")
                      (name . "^\\*Messages\\*$")))
         ("Help" (or (name . "^\\*Help\\*$")
                     (name . "^\\*Apropos\\*$")
                     (name . "^\\*info\\*$"))))))

;; Set the default filter group
(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Enable ibuffer auto-mode
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)

(use-package multiple-cursors
  :straight t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  )

(setq backup-directory-alist '(("" . "~/.emacs.d/bak")))

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/bak/autosaves" t)))
(setq backup-by-copying t ; use copying to create backup files
      delete-old-versions t ; delete excess backup files
      kept-new-versions 4
      kept-old-versions 2
      version-control t)

(use-package vertico
  :straight t
  :init (vertico-mode))

(use-package marginalia
  :straight t
  (:keymaps 'minibuffer-local-map
	    "M-A" 'marginalia-cycle)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; this was taken from https://www.emacswiki.org/emacs/DiredOmitMode
(defun dired-dotfiles-toggle ()
  "Show/hide dot-files"
  (interactive)
  (when (equal major-mode 'dired-sidebar-mode)
    (if (or (not (boundp 'dired-dotfiles-show-p)) dired-dotfiles-show-p) ; if currently showing
        (progn 
          (set (make-local-variable 'dired-dotfiles-show-p) nil)
          (message "h")
          (dired-mark-files-regexp "^\\\.")
          (dired-do-kill-lines))
      (progn (revert-buffer) ; otherwise just revert to re-show
             (set (make-local-variable 'dired-dotfiles-show-p) t)))))

;; add dired-sidebar selector
(defun ibuffer-mark-dired-sidebar-buffers ()
  "Mark all `dired-sidebar' buffers."
  (interactive)
  (ibuffer-mark-on-buffer
   (lambda (buf) (eq (buffer-local-value 'major-mode buf) 'dired-sidebar-mode))))

;; for better quality icons, Emacs should be installed with imagemagick support
(use-package vscode-icon
  :straight t
  :commands (vscode-icon-for-file))

(use-package dired-sidebar
  :straight (:type git :host github :repo "jojojames/dired-sidebar")
  :bind (("C-x C-n" . dired-sidebar-toggle-sidebar)
         :map dired-mode-map
         ("<backtab>" . dired-dotfiles-toggle))
  :commands (dired-sidebar-toggle-sidebar)
  :init
  (add-hook 'dired-sidebar-mode-hook
            (lambda ()
              (unless (file-remote-p default-directory)
                (auto-revert-mode))))
  :config
  (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
  (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
  (setq dired-sidebar-subtree-line-prefix "__")
  (setq dired-sidebar-theme 'vscode)
  (setq dired-sidebar-use-term-integration t))

(use-package ibuffer
  :straight (:type built-in)
  :config
  ;; define keymap to select all dired-sidebar modes while in ibuffer
  (define-key ibuffer-mode-map (kbd "* |") 'ibuffer-mark-dired-sidebar-buffers))

;; ;; define custom function to trigger show/hide in 'outline-minor-mode'
(defun de/hide_all ()
  (interactive)
  (if outline-minor-mode
      (progn (outline-hide-body)
             (outline-hide-sublevels 1))
    (message "Outline minor mode is not enabled.")))

(add-hook 'prog-mode-hook 'outline-minor-mode)

;; remap some of the terrible default keybindings
(let ((kmap outline-minor-mode-map))
  (define-key kmap (kbd "M-<up>") 'outline-move-subtree-up)
  (define-key kmap (kbd "M-<down>") 'outline-move-subtree-down)
  (define-key kmap (kbd "<backtab>") 'outline-cycle)
  (define-key kmap (kbd "C-s-h") 'de/hide_all)
  (define-key kmap (kbd "C-s-s") 'outline-show-all))

(defun de/scroll-half-page-down ()
  (interactive)
  (move-to-window-line-top-bottom)
  (move-to-window-line-top-bottom)
  (recenter-top-bottom))

(defun de/scroll-half-page-up ()
  (interactive)
  (move-to-window-line-top-bottom)
  (recenter-top-bottom)
  (recenter-top-bottom))

(global-set-key (kbd "C-v") 'de/scroll-half-page-down)
(global-set-key (kbd "M-v") 'de/scroll-half-page-up)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
	(c "https://github.com/tree-sitter/tree-sitter-c")
	(cmake "https://github.com/uyha/tree-sitter-cmake")
	(css "https://github.com/tree-sitter/tree-sitter-css")
	(elisp "https://github.com/Wilfred/tree-sitter-elisp")
	(html "https://github.com/tree-sitter/tree-sitter-html")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
	(json "https://github.com/tree-sitter/tree-sitter-json")
	(make "https://github.com/alemuller/tree-sitter-make")
	(markdown "https://github.com/ikatyang/tree-sitter-markdown")
	(python "https://github.com/tree-sitter/tree-sitter-python")
	(toml "https://github.com/tree-sitter/tree-sitter-toml")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq eglot-report-progress nil)

(use-package eglot
  :straight (:type built-in)
  :defer t
  :bind (:map eglot-mode-map
	      ("C-c C-d" . eldoc)
	      ("C-c C-f" . eglot-format-buffer))
  :hook ((python-base-mode . eglot-ensure)
	 (python-base-mode . hs-minor-mode))
  :custom (eglot-autoshutdown t))

(use-package eglot-booster
  :defer t
  :straight (eglot-booster :type git :host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

(use-package treesit
  :mode (("\\.py\\'" . python-ts-mode)
	 ("\\.c\\'" . c-ts-mode)
	 ("\\.yaml\\'" . yaml-ts-mode)
	 ("\\.h\\'" . c-ts-mode))
  :straight (:type built-in))

(use-package 
  avy
  :straight t)

(global-set-key (kbd "C-:") 'avy-goto-char)

(use-package apheleia
  :straight t
  :config
  ;; customize ruff
  (setf (alist-get 'ruff apheleia-formatters)
        '("ruff" "format" "--silent"
	  "--line-length" "79"
	  "--stdin-filename" filepath "-"))

  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(ruff-isort ruff))

  :hook (prog-mode . apheleia-mode)
  )

(require 'apheleia)

(use-package corfu
  :straight t
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-cycle t) ;; allow cycling through candidates
  (corfu-auto t) ;; enable auto completion
  (corfu-quit-no-match 'separator) ;; or t
  (corfu-auto-delay 0.1)
  (corfu-echo-documentation nil)
  (corfu-popupinfo-delay '(0.3 . 0.15))
  :init
  (global-corfu-mode)
  (corfu-popupinfo-mode))

;; add corfu extension
(use-package cape
  :straight t
  :bind (("C-c p p" . completion-at-point)
	 ("C-c p \\" . cape-tex)
	 ("C-c p _" . cape-tex)
	 ("C-c p ^" . cape-tex)
	 ("C-c p f" . cape-file)
	 ("C-c p d" . cape-dabbrev)
	 ("C-c p s" . cape-elisp-symbol)
	 ("C-c p e" . cape-elisp-block))
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

;; use treesitter
(use-package python
  :config
  (define-key python-ts-mode-map (kbd "s-[") 'python-indent-shift-left)
  (define-key python-ts-mode-map (kbd "s-]") 'python-indent-shift-right)
  )

(use-package micromamba
  :straight t
  :config
  (defun change-inferior-python ()
    (when (executable-find "ipython3")
      (setq python-shell-interpreter "ipython3"
    	    python-shell-interpreter-args "--simple-prompt")))
  :hook
  (micromamba-postactivate-hook . change-inferior-python)
  )

;; set 'utils' to be the default Python environment
(when (functionp 'micromamba-activate)
  (micromamba-activate "utils"))

(defun de/restart-python ()
  "Clear current inferior python buffer and restart process"
  (interactive)
  (progn (with-current-buffer "*Python*" (comint-clear-buffer))
	 (python-shell-restart)))

;; custom function to kill current cell
(defun de/kill-cell ()
  "code-cells mode custom function to kill current cell"
  (interactive)
  (let ((beg (car (code-cells--bounds)))
	(end (cadr (code-cells--bounds))))
    (kill-region beg end)))

(use-package code-cells
  :straight t
  :defer t
  :hook ((python-ts-mode . code-cells-mode-maybe))
  :config
  (add-to-list 'code-cells-eval-region-commands
	       '(python-ts-mode . python-shell-send-region) t)
  :bind
  (:map
   code-cells-mode-map
   ("M-p" . code-cells-backward-cell)
   ("M-n" . code-cells-forward-cell)
   ("C-c r p" . de/restart-python)
   ("C-c d d" . de/kill-cell)
   ("M-S-<up>" . code-cells-move-cell-up)
   ("M-S-<down>" . code-cells-move-cell-down)
   ("C-c x ;" . code-cells-comment-or-uncomment)
   ("C-c C-c" . code-cells-eval)))

(use-package jupyter
  :straight t (jupyter :type git :host github :repo "emacs-jupyter/jupyter")
  :defer t
  :custom
  ;; (jupyter-eval-use-overlays t)
  (jupyter-repl-echo-eval-p t)
  :bind
  (:map jupyter-repl-mode-map
	("C-c C-k" . jupyter-repl-clear-cells)))

(use-package gnuplot
  :defer t
  :straight t)

;; enable languages for org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (awk . t)
   (sed . t)
   (shell . t)
   (gnuplot . t)
   (python . t)
   (jupyter . t)))

(org-babel-jupyter-override-src-block "python")

;; patch for correct handling of 'python' org source blocks
(add-to-list 'org-src-lang-modes '("python" . python-ts))

(defun de/insert-org-jupyter-kernel-spec ()
  "Interactively insert a Jupyter kernel spec at the beginning of an Org document.
Ensure 'jupyter' is available, or interactively activate it using 'micromamba-activate'."
  (interactive)
  (unless (executable-find "jupyter")
    (call-interactively 'micromamba-activate)) ;; Call `micromamba-activate` interactively to ensure prompt.
  ;; Ensure 'jupyter' is available after activation attempt.
  (if (executable-find "jupyter")
      (let* ((kernelspec (jupyter-completing-read-kernelspec))
             (kernel-name (jupyter-kernelspec-name kernelspec))
             (kernel-display-name (plist-get (jupyter-kernelspec-plist kernelspec) :display_name))
             (insertion-point (point-min))
             (properties (format "#+PROPERTY: header-args:python :session py
#+PROPERTY: header-args:python+ :async yes
#+PROPERTY: header-args:python+ :eval never-export
#+PROPERTY: header-args:python+ :kernel %s\n"  kernel-name)))
        (save-excursion
          (goto-char insertion-point)
          (insert properties)
          (message "Inserted Jupyter kernel spec for '%s'." kernel-display-name)))
    (message "Jupyter is not available. Please ensure it is installed and try again.")))

(defun de/org-jupyter-setup ()
  (define-key org-mode-map (kbd "C-c j") 'de/insert-org-jupyter-kernel-spec))

(add-hook 'org-mode-hook 'de/org-jupyter-setup())

(defun patch/display-ansi-colors ()
  "Fixes kernel output in emacs-jupyter"
  (ansi-color-apply-on-region (point-min) (point-max)))
(add-hook 'org-mode-hook
	  (lambda ()
	    (add-hook 'org-babel-after-execute-hook #'patch/display-ansi-colors)))

(defun de/python-with-jupyter-repl
    (kernel-name &optional repl-name filename)
  "Choose jupyter kernel to open/start new Python file associated to it
"
  ;; ~interactive~ form only constructs a list of elements that
  ;; correspond directly to the arguments of the function
  (interactive
   (let ((file (read-file-name "Open Python file: " nil nil nil)))
     (list
      ;; first argument, kernel-name
      (jupyter-kernelspec-name
       (jupyter-completing-read-kernelspec nil current-prefix-arg))
      ;; second argument, repl-name
      (if current-prefix-arg ;; if user supplies REPL name, use it
	  (read-string "REPL name: ")
	(file-name-base file)) ;; otherwise, use base filename
      ;; third argument, filename
      file)))
  
  ;; this means you can interactively choose what gets passed as the
  ;; arguments for the function

  ;; open or create the Python file
  (find-file filename)

  ;; start the jupyter REPL and store the client symbol
  (let ((client (jupyter-run-repl kernel-name repl-name)))
    ;; wait for REPL to start and then associate the buffer
    (sleep-for 1.0)
    (jupyter-repl-associate-buffer client)))

(use-package nano-theme
  :straight (nano-theme :type git :host github :repo "rougier/nano-theme"))

;; setup customization of nano colors via advice
(defun de/customize-nano-themes ()
  (set-face-attribute 'show-paren-match nil :background "#96ddcf"))

(defun de/advise-nano-themes ()
  "Add advice to nano theme functions to set show-paren-match face."
  (advice-add 'nano-dark :after #'de/customize-nano-themes)
  (advice-add 'nano-light :after #'de/customize-nano-themes))

(de/advise-nano-themes)


(require 'nano-theme)
(nano-mode)

;; use the 'light scheme by default, switch by calling `nano-theme-toggle`
(load-theme 'nano t)

(use-package nano-modeline
  :straight (nano-modeline :type git :host github :repo "rougier/nano-modeline")
  :hook
  (text-mode-hook nano-modeline-text-mode)
  (prog-mode-hook nano-modeline-prog-mode)
  (org-mode-hook nano-modeline-org-mode))

;; set nano-modeline as default
(require 'nano-modeline)
(nano-modeline-text-mode t)

;; hide the default modeline
(setq-default mode-line-format nil)

;; set customization on emacs startup
(add-hook 'emacs-startup-hook #'de/customize-nano-themes)

;; call these after init to avoid orderof-execution problems
(add-hook 'after-init-hook
          (lambda ()
            (menu-bar-mode -1)
            (tool-bar-mode -1)
            (scroll-bar-mode -1)))

;; Set default frame size
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 30))

(use-package rainbow-mode
  :straight t
  :hook (org-mode prog-mode))

;; add emacs ~app~ folder to load-path
(add-to-list 'load-path "~/Apps/emacs/notes-list")  
(add-to-list 'load-path "~/Apps/emacs/svg-tag-mode")
(use-package svg-lib
  :defer t
  :straight t)
(use-package stripes
  :defer t
  :straight t)

(require 'notes-list)

(defun de/insert-org-note-tags ()
  "Inserts predefined org-mode tags at the beginning of the document."
  (interactive)
  (goto-char (point-min)) ; Move to the beginning of the buffer
  (insert "#+TITLE: note title\n")
  (insert (format "#+DATE: <%s>\n" (format-time-string "%Y-%m-%d %a")))
  (insert "#+FILETAGS: note\n")
  (insert "#+SUMMARY: my note\n")
  (insert "#+ICON: material/notebook\n\n"))

(use-package org
  :config
  (add-hook 'org-mode-hook 'org-indent-mode)
  (setq org-confirm-babel-evaluate nil)
  (setq org-display-inline-images t)
  (setq org-startup-with-inline-images t)
  ;; I disabled this to make underscores appear proper
  ;; (setq org-pretty-entities t)
  )

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; shortcut to insert source block
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sp" . "src python"))

;; LaTeX preview rendering default to SVG instead of PNG
(setq org-preview-latex-default-process 'dvisvgm)

(setq org-latex-preview-image-directory (expand-file-name "~/.emacs.d/tmp"))
(setq org-latex-preview-ltxpng-directory (expand-file-name "~/.emacs.d/tmp"))
(setq temporary-file-directory (file-truename "~/.emacs.d/tmp"))

(use-package htmlize
  :straight t)

(defun de/my-org-inline-css-hook (exporter)
  "Insert custom inline css"
  (when (eq exporter 'html)
    (let* ((dir (ignore-errors (file-name-directory (buffer-file-name))))
           (path (concat dir "style.css"))
           (homestyle (or (null dir) (null (file-exists-p path))))
           (final (if homestyle "~/Apps/emacs-config/custom/notebook.css" path))) ;; <- set your own style file path
      (setq org-html-head-include-default-style nil)
      (setq org-html-head (concat
                           "<style type=\"text/css\">\n"
                           "<!--/*--><![CDATA[/*><!--*/\n"
                           (with-temp-buffer
                             (insert-file-contents final)
                             (buffer-string))
                           "/*]]>*/-->\n"
                           "</style>\n")))))

(add-hook 'org-export-before-processing-hook 'de/my-org-inline-css-hook)

(use-package org-fragtog
  :after org
  :hook
  (org-mode . org-fragtog-mode)
  :custom
  (org-startup-with-latex-preview t)
  (org-format-latex-options
   (plist-put org-format-latex-options :scale 2)
   (plist-put org-format-latex-options :foreground 'auto)
   (plist-put org-format-latex-options :background 'auto)))

(defun de/reload-emacs-config()
  (interactive)
  "convenient function to reload config file"
  (org-babel-load-file "~/Apps/emacs-config/config.org"))

(defun de/move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (set-mark (point))
      (insert text)
      (exchange-point-and-mark)
      (setq deactivate-mark nil)))
   (t
    (let ((column (current-column)))
      (beginning-of-line)
      (when (or (> arg 0) (not (bobp)))
        (forward-line)
        (when (or (< arg 0) (not (eobp)))
          (transpose-lines arg))
        (forward-line -1))
      (move-to-column column t)))))

(defun de/move-text-up (arg)
  "Move region (if selected) or current line up by ARG lines."
  (interactive "*p")
  (de/move-text-internal (- (or arg 1))))

(defun de/move-text-down (arg)
  "Move region (if selected) or current line down by ARG lines."
  (interactive "*p")
  (de/move-text-internal (or arg 1)))

(global-set-key (kbd "s-<up>") 'de/move-text-up)
(global-set-key (kbd "s-<down>") 'de/move-text-down)

(global-set-key (kbd "s-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "s-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "s-C-<down>") 'shrink-window)
(global-set-key (kbd "s-C-<up>") 'enlarge-window)

;; (global-set-key (kbd "M-o") 'other-window)
(use-package ace-window
  :straight t
  :bind
  (("M-o" . ace-window)))

(load "/Users/delnatan/Apps/emacs-config/custom/DE_fun01.el" t nil t)

;; setup org-agenda keybinding to `C-c a`
(global-set-key (kbd "C-c a") 'org-agenda)

(setq org-agenda-files
      '(
	"/Users/delnatan/Library/CloudStorage/Dropbox/org/todo.org"
	"/Users/delnatan/Documents/org/tasks.org"
	"/Users/delnatan/StarrLuxtonLab/org/schedules.org"
	"/Users/delnatan/Library/CloudStorage/Dropbox/org/notes/random_notes.org"
	"/Users/delnatan/Documents/org/meetings.org"
	"/Users/delnatan/Documents/org/events.org"
	)
      )

;; set =C-c c= to do org-capture
(define-key global-map (kbd "C-c c") 'org-capture)

;; set templates
(setq org-capture-templates
      '(("t" "TODO" entry (file+headline "~/Documents/org/tasks.org" "Tasks")
  	 "* TODO %U %? \n  %i\n")
	("n" "Note" entry (file+headline "~/Documents/org/notes.org" "Notes")
	 "* %^{TITLE} :NOTE:\n#+DATE: %<%Y-%m-%d %a>\n#+FILETAGS: note\n#+SUMMARY: %^{SUMMARY}\n#+ICON: material/notebook\n%?\n")
	("m" "Meeting" entry (file+headline "~/Documents/org/meetings.org" "Meetings")
	 "* Meeting with %? :MEETING:\nSCHEDULED: %^T\n-  Location: %^{Location}\n-  Participants: %^{Participants}\n- Agenda:\n  -  %^{Agenda}\n")
	("e" "Event" entry (file+headline "~/Documents/org/events.org" "Events" )
	 "* %? :EVENT:\nSCHEDULED: %^T\n-  Location: %^{Location}\n-  %i\n")))

;; configure refile targets
(setq org-refile-targets '((nil :maxlevel . 3)
			   (org-agenda-files :maxlevel . 3)))

(use-package scad-mode
  :defer t
  :straight (scad-mode :type git :host github :repo "openscad/emacs-scad-mode"))

(use-package csv-mode
  :defer t
  :straight (csv-mode :type git :host github :repo "emacsmirror/csv-mode"))

(use-package ellama
  :defer t
  :init
  (setopt ellama-keymap-prefix "C-c e")
  (require 'llm-ollama)
  (setopt ellama-provider
	  (make-llm-ollama
	   :chat-model "llama3:instruct"
	   :embedding-model "llama3:latest")))
