;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Startup and Behavior Controls 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(setq load-path (cons "~/emacs" load-path)) 
(setq custom-file "~/emacs/custom.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Machine Specific Configuration Section
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; go FULLSCREEN
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)

(when (eq window-system 'x)
  (toggle-fullscreen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Load the real init
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wvi-init)



;; SLIME SETUP
(add-to-list 'load-path "~/emacs/slime/")  ; your SLIME directory
(setq inferior-lisp-program "/usr/bin/sbcl") ; your Lisp system
(require 'slime-autoloads)
(slime-setup '(slime-fancy)) ; almost everything

;; SLIME AUTO-COMPLETE
(require 'ac-slime)
(add-hook 'slime-mode-hook 'set-up-slime-ac)
(add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'slime-repl-mode))

(defun wvi-cedet-hook ()
  (setq ac-sources (append '(ac-source-semantic) ac-sources))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (semantic-mode 1))
(add-hook 'c-mode-common-hook 'wvi-cedet-hook)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(setq-default ecb-tip-of-the-day nil)
(require 'ecb)

;; TRAMP
(require 'tramp)

;;FLYSPELL

;;PHP-MODE
(require 'php-mode)
;;GEBEN
(add-to-list 'load-path "~/emacs/geben")
(autoload 'geben "geben" "DBGp protocol frontend, a script debugger" t)


;;BISON/FLEX
;;;;  these are the lines i use to set up correct auto-ing
(require 'bison-mode)
(require 'flex-mode)


(add-to-list 'load-path "~/src/elisp/keyfreq")
(require 'keyfreq)
(keyfreq-mode 1)
(keyfreq-autosave-mode 1)
