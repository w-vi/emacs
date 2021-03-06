(provide 'wvi-modes)

;; MODULE

;;PATHS
(add-to-list 'load-path "~/emacs/site-lisp")
(add-to-list 'load-path "~/emacs/tabbar")
(add-to-list 'load-path "~/emacs/expand-region")


(require 'package)
(package-initialize)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository
(setq package-enable-at-startup nil)
(package-initialize)

;; SHELL MODE ... make it nice
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;;WINNER MODE
(when (fboundp 'winner-mode)
  (winner-mode t))

;;GGTAGS
(autoload
  'ggtags-mode
  "ggtags"
  "Emacs gnu global tags minor mode"
  t)

;; ACE JUMP MODE
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

;;IDO-MODE
(require 'flx-ido)
(ido-mode t)
(ido-everywhere t)
(flx-ido-mode t)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

;;PROJECTILE
(require 'projectile)
; projectile everywhere
(projectile-global-mode)


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
;;(add-hook 'c-mode-common-hook #'(lambda () (hs-minor-mode)))
;;(add-hook 'python-mode-hook #'(lambda () (hs-minor-mode)))
;;(add-hook 'lisp-mode-hook #'(lambda () (hs-minor-mode)))
;;(add-hook 'emacs-lisp-mode-hook #'(lambda () (hs-minor-mode)))

;;ORG-MODE
(require 'org)
(setq org-indent-mode t)
;;abbrev and flyspell in org-mode
(add-hook 'org-mode-hook #'(lambda ()(abbrev-mode t)(flyspell-mode t)))
(require 'ox-wk)
(require 'ox-md)
(require 'ox-odt)
(setq org-export-default-language "en"
      org-export-html-extension "html"
      org-export-with-timestamps nil
      org-export-with-section-numbers nil
      org-export-with-tags 'not-in-toc
      org-export-skip-text-before-1st-heading nil
      org-export-with-sub-superscripts '{}
      org-export-with-LaTeX-fragments t
      org-export-with-archived-trees nil
      org-export-highlight-first-table-line t
      org-export-latex-listings-w-names nil
      org-export-html-style-include-default nil
      org-export-htmlize-output-type 'css
      org-startup-folded nil
      org-export-allow-BIND t
      org-publish-list-skipped-files t
      org-publish-use-timestamps-flag nil
      org-export-babel-evaluate t
      org-confirm-babel-evaluate nil)

;this line activates ditaa
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)))

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

;;THING AT POINT EDIT
(require 'thing-edit)

;; HIGHLIGHT INDENT
(require 'highlight-indentation)

;; HIGLIGHT LISP stuff
(require 'highlight-quoted)
(require 'highlight-defined)
(add-hook 'emacs-lisp-mode-hook
          #'(lambda ()(highlight-quoted-mode t)(highlight-defined-mode t)))

;; Dockerfile
(require 'dockerfile-mode)
(add-to-list 'auto-mode-alist '("Dockerfile\\'" . dockerfile-mode))

;; YASNIPPET
(require 'yasnippet)
(setq yas-trigger-key [f2])
(yas-reload-all)
;; This is where your snippets will lie.
(setq yas-snippet-dirs '("~/emacs/yasnippet/snippets"))
(mapc 'yas-load-directory yas-snippet-dirs)
;;(add-hook 'c-mode-common-hook #'(lambda () (yas-minor-mode)))
;;(add-hook 'emacs-lisp-mode-hook #'(lambda () (yas-minor-mode)))
;;(add-hook 'python-mode-hook #'(lambda () (yas-minor-mode)))
(add-hook 'org-mode-hook #'(lambda () (yas-minor-mode)))
(global-set-key [(control f2)] 'yas-insert-snippet)
;;(global-set-key [f2] 'yas-expand)

(add-hook 'emacs-lisp-mode-hook 'progmodes-hooks)

;; Company
(add-hook 'after-init-hook 'global-company-mode)

;; AUTOCOMPLETE
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/emacs/ac-dict")
;; (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary
;;                            ac-source-words-in-same-mode-buffers ))
;; (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;; (global-auto-complete-mode t)
;; (ac-set-trigger-key "TAB")
;; (ac-set-trigger-key "<tab>")

;;READLINE-COMPLETE for getting autocompletition in shell buffer
(setq explicit-shell-file-name "bash")
(setq explicit-bash-args '("-c" "export EMACS=; stty echo; bash"))
(setq comint-process-echoes t)
(require 'readline-complete)
;;(add-to-list 'ac-modes 'shell-mode)
;;(add-hook 'shell-mode-hook 'ac-rlc-setup-sources)


;; VLF setup
(require 'vlf-setup)
(custom-set-variables
 '(vlf-application 'dont-ask))

;; MAGIT
(require 'magit-gh-pulls)
(add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)

;; PACKAGE_LINT
(require 'package-lint)

;; PROGRAMMING STUFF
;;C style conventions
(setq c-default-style "k&r"
      c-basic-offset 4)
;spaces instead of TAB in C/C++ mode
(setq c-mode-hook (function
                   (lambda () (setq indent-tabs-mode nil)
                     (setq c-indent-level 4)
                     (c-toggle-auto-state 1)
                     (c-toggle-auto-hungry-state 1))))

; style I want to use in c++ mode
(c-add-style "my-style"
             '("stroustrup"
               (indent-tabs-mode . nil)        ; use spaces rather than tabs
               (c-basic-offset . 4)            ; indent by four spaces
               (c-offsets-alist . ((inline-open . 0)  ; custom indentation rules
                                   (brace-list-open . 0)
                                   (statement-case-open . +)))))
(defun my-c++-mode-hook ()
  (c-set-style "my-style")        ; use my-style defined above
  (auto-fill-mode)
  (c-toggle-auto-hungry-state 1))

(add-hook 'c++-mode-hook 'my-c++-mode-hook)
;.h are most of the time C files in my case so use that as default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c-mode))
(add-hook 'c-mode-common-hook 'progmodes-hooks)

; add man pages refernce on [C-h d] key
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook
            (lambda ()(local-set-key (kbd "C-h d")
                                     (lambda ()
                                       (interactive)
                                       (manual-entry (current-word)))))))
;;MAKEFILE
(add-hook 'makefile-mode-hook 'indent-tabs-mode)
(add-to-list 'auto-mode-alist '("\\.am\\'" . makefile-automake-mode))

;; GO-LANG
(require 'go-mode)
(require 'company-go)
(add-hook 'go-mode-hook
  (lambda ()
    (setq-default)
    (setq gofmt-command "goimports")
    (setq tab-width 4)
    (setq indent-tabs-mode t)
    (setq show-trailing-whitespace nil)
    (local-set-key (kbd "M-.") 'godef-jump)
    ; Go doc
    (local-set-key (kbd "C-c C-g") 'godoc)
    ; Go oracle
    (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet"))
    (add-hook 'before-save-hook 'gofmt-before-save)
    (set (make-local-variable 'company-backends) '(company-go))
    (company-mode)))
(add-hook 'go-mode-hook 'progmodes-hooks)

;; WEB MODE web-mode.org
(require 'web-mode)
(add-hook 'web-mode-hook 'progmodes-hooks)


;; JAVASCRIPT MODE
(autoload 'js2-mode "js2-mode")
(eval-after-load "js2-mode"
  '(progn
     (setq js2-missing-semi-one-line-override t)
     (setq-default js2-basic-offset 2)))
(setq js-indent-level 2)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-hook 'js2-mode-hook 'progmodes-hooks)

;;MARKDOWN MODE
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; PYTHON
(elpy-enable)
;; (load-library "python-import-add")
(defun wvi-python-hook ()
  (ggtags-mode 1)
  (yas-minor-mode-on)
  (hs-minor-mode)
  (setq python-indent-offset 4))

(add-hook 'python-mode-hook 'wvi-python-hook)

;; ;;set ipython as default python shell
;; (setq
;;  python-shell-interpreter "ipython"
;;  python-shell-interpreter-args ""
;;  python-shell-prompt-regexp "In \\[[0-9]+\\]: "
;;  python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: "
;;  python-shell-completion-setup-code
;;    "from IPython.core.completerlib import module_completion"
;;  python-shell-completion-module-string-code
;;    "';'.join(module_completion('''%s'''))\n"
;;  python-shell-completion-string-code
;;    "';'.join(get_ipython().Completer.all_completions('''%s'''))\n")

