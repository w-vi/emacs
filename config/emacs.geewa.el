;;@author wvi
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

;; on OSX I want to have same path as in shell
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; clang auto-complete
(require 'auto-complete-clang-async)

(defun ac-cc-mode-setup ()
  (setq ac-sources '(ac-source-clang-async ac-source-yasnippet ac-source-words-in-same-mode-buffers ac-source-gtags))
  (setq ac-clang-flags
        (mapcar (lambda (item)(concat "-I " item))
                (split-string
                 "
 /Users/wvi/src/gclx/gclx/include/
 /Users/wvi/src/gclx/gclx/src/
 /Users/wvi/src/gclx/gcln/include/
 /Users/wvi/src/gclx/gcln/src/
 /Users/wvi/src/gclx/external/libuv/include/
 /Users/wvi/src/gclx/external/zlib/
 /Users/wvi/src/gclx/gclp/include/
"
                 )))
  (setq ac-clang-flags (append '("-DGCLX_NATIVE") ac-clang-flags))
  (ac-clang-launch-completion-process))
(add-hook 'c-mode-common-hook 'ac-cc-mode-setup)

(setq load-path (cons "~/src/python/py3gyp/tools/emacs" load-path))
(require 'gyp)

;; ipython path
(setq python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/2.7/bin/ipython" )

;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

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

