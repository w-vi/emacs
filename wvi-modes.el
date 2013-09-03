(provide 'wvi-modes)

;; MODULE

;;PATHS
(add-to-list 'load-path "~/emacs/site-lisp")
(add-to-list 'load-path "~/emacs/yasnippet")
(add-to-list 'load-path "~/emacs/auto-complete")
(add-to-list 'load-path "~/emacs/emacs-jedi")
(add-to-list 'load-path "~/emacs/tabbar")
(add-to-list 'load-path "~/emacs/expand-region")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/org")

;; SHELL MODE ... make it nice
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

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

;; HIDE/SHOW
(add-hook 'c-mode-common-hook #'(lambda () (hs-minor-mode)))
(add-hook 'python-mode-hook #'(lambda () (hs-minor-mode)))
(add-hook 'lisp-mode-hook #'(lambda () (hs-minor-mode)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (hs-minor-mode)))

;;ORG-MODE
(require 'org)
(setq org-indent-mode t)
;;abbrev and flyspell in org-mode
(add-hook 'org-mode-hook #'(lambda ()(abbrev-mode t)(flyspell-mode t)))
(require 'ox-dkw)

;; HIGHLIGHT CURRENT LINE 
(global-hl-line-mode t)
(setq highlight-current-line-globally t)
(setq highlight-current-line-high-faces nil)
(setq highlight-current-line-whole-line t)
(setq hl-line-face (quote highlight))

;; SMEX - M-x ido boost
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; EXPAND REGION
(require 'expand-region)
(global-set-key [f4] 'er/expand-region)
(global-set-key [(control f4)] 'er/contract-region)


;; YASNIPPET
(require 'yasnippet)
(setq yas-trigger-key nil)
(yas-reload-all)
;; This is where your snippets will lie.
(setq yas-snippet-dirs '("~/emacs/yasnippet/snippets"))
(mapc 'yas-load-directory yas-snippet-dirs)
(add-hook 'c-mode-common-hook #'(lambda () (yas-minor-mode)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (yas-minor-mode)))
(add-hook 'python-mode-hook #'(lambda () (yas-minor-mode)))
(add-hook 'org-mode-hook #'(lambda () (yas-minor-mode)))
(global-set-key [(control f2)] 'yas-insert-snippet)
(global-set-key [f2] 'yas-expand)

;; AUTOCOMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/emacs/ac-dict")
(setq-default ac-sources '(ac-source-abbrev ac-source-dictionary 
			   ac-source-words-in-same-mode-buffers ))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(global-auto-complete-mode t)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;READLINE-COMPLETE for getting autocompletition in shell buffer
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
(require 'readline-complete)
(add-to-list 'ac-modes 'shell-mode)
(add-hook 'shell-mode-hook 'ac-rlc-setup-sources)


;; PYTHON
;;set ipython as default python shell
(setq
 python-shell-interpreter "ipython"
 python-shell-interpreter-args ""
 python-shell-prompt-regexp "In \\[[0-9]+\\]: "
 python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
 python-shell-completion-setup-code
   "from IPython.core.completerlib import module_completion"
 python-shell-completion-module-string-code
   "';'.join(module_completion('''%s'''))\n"
 python-shell-completion-string-code
   "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")
(require 'jedi)
(add-hook 'python-mode-hook 'jedi:ac-setup)

