(set-default-coding-systems 'utf-8)
(setq inhibit-startup-message t)
(setq custom-safe-themes t)
(global-visual-line-mode)
(column-number-mode)
;; use y/n for yes-or-no
(setopt use-short-answers t) ; since Emacs 29 `yes-or-no-p` will use `y-or-n-p`
 
;; I only want to show line numbers in programming mode.
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; keep things tidy
(use-package no-littering
  :straight t)

;; for programming set fill to 80 columns and display column indicator
(setq-default fill-column 80)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)

;; enable recent-file mode
(recentf-mode 1)
(setq recentf-max-saved-items 50)
;; Open recent files 
(global-set-key (kbd "C-c r") 'recentf-open-files)

;; map C-x C-b to `ibuffer`
(define-key ctl-x-map "\C-b" 'ibuffer)

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

;; Close frame if not the last, kill emacs else
;; took this from nano-emacs `nano-bindings.el`
(defun de/delete-frame-or-kill-emacs ()
  "Delete frame or kill Emacs if there is only one frame."
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-terminal)))
(global-set-key (kbd "C-x C-c") 'de/delete-frame-or-kill-emacs)

(use-package multiple-cursors
  :straight t
  :bind (("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))
  )

(use-package which-key
  :straight (which-key :type git :host github :repo "justbur/emacs-which-key"))
(which-key-mode)

(setq backup-directory-alist '(("" . "~/.emacs.d/bak")))

(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/bak/autosaves" t)))
(setq backup-by-copying t ; use copying to create backup files
      delete-old-versions t ; delete excess backup files
      kept-new-versions 4
      kept-old-versions 2
      version-control t)

(use-package vterm
  :straight t)

(use-package vertico
  :init
  (vertico-mode))

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

(use-package treesit
  :straight (:type built-in)
  :config
  (use-package combobulate
    :preface
    (setq combobulate-key-prefix "C-c o")))

(use-package apheleia
  :straight t
  :config
  (setf (alist-get 'yapf apheleia-formatters)
	'("black" "--line-length" "79" "-"))
  (setf (alist-get 'isort apheleia-formatters)
        '("isort" "--stdout" "-"))
  (setf (alist-get 'python-ts-mode apheleia-mode-alist)
        '(isort yapf))
  :hook (prog-mode . apheleia-mode)
  )

(require 'apheleia)

(use-package corfu
  :straight t
  :custom
  (tab-always-indent 'complete)
  (completion-cycle-threshold nil)
  (corfu-auto t)
  (corfu-quit-no-match 'separator) ;; or t
  (corfu-auto-delay 0.15)
  (corfu-echo-documentation nil)
  :config
  (setq corfu-popinfo-delay nil)
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
  :mode ("\\.py\\'" . python-ts-mode)
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
  "Clear current inferiorpython buffer and restart process"
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
  :init
  (add-hook 'python-mode-hook 'code-cells-mode-maybe)
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
  :straight t
  :config
  (setq jupyter-eval-use-overlays t))

;; enable languages for org-babel
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (awk . t)
   (sed . t)
   (shell . t)
   (python . t)
   (jupyter . t)))

(org-babel-jupyter-override-src-block "python")

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
#+PROPERTY: header-args:python+ :kernel %s\n"  kernel-name)))
        (save-excursion
          (goto-char insertion-point)
          (insert properties)
          (message "Inserted Jupyter kernel spec for '%s'." kernel-display-name)))
    (message "Jupyter is not available. Please ensure it is installed and try again.")))

(defun de/org-jupyter-setup ()
  (define-key org-mode-map (kbd "C-c j") 'de/insert-org-jupyter-kernel-spec))

(add-hook 'org-mode-hook 'de/org-jupyter-setup())

(straight-use-package
 '(nano :type git :host github :repo "rougier/nano-emacs"))

;; (setq nano-font-family-monospaced "Iosevka")

(require 'nano-layout)
(require 'nano-faces)
(require 'nano-theme)
(nano-faces)
(require 'nano-theme-dark)
(require 'nano-theme-light)
(nano-theme-set-light)
(call-interactively 'nano-refresh-theme)
(require 'nano-modeline)

(require 'color)

;; for light color
(if (equal nano-theme-var "dark")
    (set-face-attribute 'org-block nil :background
			(color-lighten-name
			 (face-attribute 'default :background) 15))
  (set-face-attribute 'org-block nil :background
		      (color-darken-name
		       (face-attribute 'default :background) 3)))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Set default frame size
(add-to-list 'default-frame-alist '(width . 80))
(add-to-list 'default-frame-alist '(height . 30))

;; add emacs ~app~ folder to load-path
(add-to-list 'load-path "~/Apps/emacs/notes-list")  
(add-to-list 'load-path "~/Apps/emacs/svg-tag-mode")
(use-package svg-lib
  :straight t)
(use-package stripes
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
  (setq org-confirm-babel-evaluate nil)
  (setq org-display-inline-images t)
  (setq org-startup-with-inline-images t)
  (setq org-pretty-entities t)
  )

(add-hook 'org-babel-after-execute-hook 'org-redisplay-inline-images)

;; shortcut to insert source block
(add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))
(add-to-list 'org-structure-template-alist '("sp" . "src python"))

;; LaTeX preview rendering default to SVG instead of PNG
(setq org-preview-latex-default-process 'dvisvgm)

(use-package org-modern
  :ensure t
  :custom
  ;; adds extra indentation
  (org-modern-hide-stars nil)
  (org-modern-table nil)
  (org-modern-list 
   '(;; (?- . "-")
     (?* . "•")
     (?+ . "‣")))
  ;;or other chars; so top bracket is drawn promptly
  (org-modern-block-name '("" . ""))
  :hook
  (org-mode . org-modern-mode)
  (org-agenda-finalize . org-modern-agenda))

;; for nicely-aligned bullet stars
(use-package org-bullets
  :straight t
  :hook (org-mode . org-bullets-mode))

(use-package org-modern-indent
  :straight (org-modern-indent :type git :host github :repo "jdtsmith/org-modern-indent")
  :config ; add late to hook
  (add-hook 'org-mode-hook #'org-modern-indent-mode 90))

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

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key (kbd "M-o") 'other-window)

(load "/Users/delnatan/Apps/emacs-config/custom/DE_fun01" t nil t)
