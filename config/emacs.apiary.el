;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Startup and Behavior Controls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(setq load-path (cons "~/emacs" load-path))
(setq custom-file "~/emacs/custom.apaiary.el")


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


;; start server if not runing
(load "server")
(unless (server-running-p) (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load the real init
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wvi-init)

(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq fci-rule-width 5)
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'makefile-mode-hook 'fci-mode)
(add-hook 'autoconf-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'fci-mode)
(add-hook 'rst-mode-hook 'fci-mode)

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
(setq tramp-default-method "ssh")

;; GYP
(setq load-path (cons "~/src/gyp/tools/emacs" load-path))
(require 'gyp)
