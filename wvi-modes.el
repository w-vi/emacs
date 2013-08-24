(provide 'wvi-modes)

;; MODULE

;;PATHS
(add-to-list 'load-path "~/emacs/site-lisp")
(add-to-list 'load-path "~/emacs/yasnippet")
(add-to-list 'load-path "~/emacs/auto-complete")
(add-to-list 'load-path "~/emacs/python-mode")
(add-to-list 'load-path "~/emacs/tabbar")

;;WINNER MODE
(when (fboundp 'winner-mode)
  (winner-mode t))

;;IDO-MODE
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode t)


;;ELECTRIC-PAIR
(when (fboundp 'electric-pair-mode)
      (electric-pair-mode t))

;;UNDO TREE
(require 'undo-tree)
(global-undo-tree-mode)

;; TAB BAR
(require 'tabbar-cfg)
(tabbar-mode)

;; HIGHLIGHT CURRENT LINE 
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line t)
(setq hl-line-face (quote highlight))

;; FCI - Fill Column Indicator
(setq fci-rule-column 120)
(require 'fill-column-indicator)
(define-globalized-minor-mode
 global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode t)

;; YASNIPPET
(require 'yasnippet)
(setq yas-trigger-key nil)
(yas-reload-all)
;; This is where your snippets will lie.
(setq yas-root-directory '("~/emacs/yasnippet/snippets"))
(mapc 'yas-load-directory yas-root-directory)
(add-hook 'c-mode-common-hook #'(lambda () (yas-minor-mode)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (yas-minor-mode)))
(add-hook 'python-mode-hook #'(lambda () (yas-minor-mode)))
(local-set-key "\C-c\C-e" 'yas-expand)

;; AUTOCOMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs/ac-dict")
(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary 
			   ac-source-words-in-same-mode-buffers ))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")


;; PYTHON
(setq py-install-directory "~/emacs/python-mode")
(add-to-list 'load-path py-install-directory)
(require 'python-mode) 
; use IPython
(setq-default py-shell-name "ipython")
(setq-default py-which-bufname "IPython")
; use the wx backend, for both mayavi and matplotlib
(setq py-python-command-args 
  '("--gui=wx" "--pylab=wx" "-colors" "Linux"))
(setq py-force-py-shell-name-p t)
; switch to the interpreter after executing code
(setq py-shell-switch-buffers-on-execute-p t)
(setq py-switch-buffers-on-execute-p t)
; don't split windows
(setq py-split-windows-on-execute-p nil)
; try to automagically figure out indentation
(setq py-smart-indentation t)
; pymacs
(add-to-list 'load-path "~/emacs/pymacs")
(autoload 'pymacs-apply "pymacs")
(autoload 'pymacs-call "pymacs")
(autoload 'pymacs-eval "pymacs" nil t)
(autoload 'pymacs-exec "pymacs" nil t)
(autoload 'pymacs-load "pymacs" nil t)
(autoload 'pymacs-autoload "pymacs")
(setq py-load-pymacs-p t)
