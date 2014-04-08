;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Startup and Behavior Controls 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(setq load-path (cons "~/emacs" load-path)) 

(setq custom-file "~/emacs/custom-geewa.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Machine Specific Configuration Section
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-font "Menlo")

;;Bug off with  new frames 
(setq ns-pop-up-frames nil)

(setq shell-command-switch "-ic")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/org")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Load the real init
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wvi-init)

(setq cedet-root-path (file-name-as-directory "/usr/share/emacs/site-lisp/cedet/"))
(load-file (concat cedet-root-path "cedet-devel-load.el"))
(add-to-list 'load-path (concat cedet-root-path "contrib"))

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(setq-default ecb-tip-of-the-day nil)
(require 'ecb)
 
;; select which submodes we want to activate
(add-to-list 'semantic-default-submodes 'global-semantic-mru-bookmark-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-idle-scheduler-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(add-to-list 'semantic-default-submodes 'global-semantic-highlight-func-mode)
(add-to-list 'semantic-default-submodes 'global-semanticdb-minor-mode)

(defun wvi-semantic-hook ()
  (semantic-add-system-include "~/flascc/sdk/usr/include" 'c-mode)
  (semantic-add-system-include "~/flascc/sdk/usr/include" 'c++-mode))
(add-hook 'semantic-init-hooks 'wvi-semantic-hook)
 
;; Activate semantic
(semantic-mode 1)

(require 'semantic/ia)
(require 'semantic/bovine/gcc) ; or depending on you compiler
; (require 'semantic/bovine/clang)

(semanticdb-enable-gnu-global-databases 'c-mode t)
(semanticdb-enable-gnu-global-databases 'c++-mode t)
 
;; SRecode
(global-srecode-minor-mode 1)

(defun wvi-cedet-hook ()
    (add-to-list 'ac-sources 'ac-source-semantic)
)

(add-hook 'c-mode-common-hook 'wvi-cedet-hook)
(add-hook 'lisp-mode-hook 'alexott/cedet-hook)
(add-hook 'scheme-mode-hook 'wvi-cedet-hook)
(add-hook 'emacs-lisp-mode-hook 'wvi-cedet-hook)

;; ipython path
(setq python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/2.7/bin/ipython" )

;; Agenda files on this machine
(setq org-agenda-files (append "~/geewa/notification/todo.org"))

;; FLYSPELL ISPELL -- cocoAspell dicts
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-really-aspell  t)
(setq ispell-dictionary-alist
      '((nil
	 "[A-Za-z]" "[^A-Za-z]" "[']" nil
	 ("-B" "-d" "english" "--dict-dir"
	  "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
	 nil iso-8859-1)))

(global-set-key (kbd "<f12>") 'ispell-word)
(global-set-key (kbd "C-S-<f12>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f12>") 'flyspell-buffer)
(global-set-key (kbd "C-<f12>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f12>") 'flyspell-check-next-highlighted-word)

(add-to-list 'load-path "~/src/org-mode/lisp")
(add-to-list 'load-path "~/src/org-mode/contrib/lisp")

;; ACTION SCRIPT MODE
(add-to-list 'load-path "~/.emacs.d/site-lisp")

(autoload 'actionscript-mode "actionscript-mode" "Major mode for actionscript." t)
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))
