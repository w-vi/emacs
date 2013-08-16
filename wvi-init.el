(provide 'wvi-init)

;;; GENERAL SETTINGS
; to undo, do M-x tool-bar-mode; 
(tool-bar-mode -1)
(blink-cursor-mode -1) ;; blinking cursor is evil

;; don't show the startup screen
(setq inhibit-startup-screen t)

;; line number in all files, all the time
(global-linum-mode t)

; make sure transient mark mode is enabled (it should be by default,
; but just in case)
(transient-mark-mode t)

; turn on mouse wheel support for scrolling
(require 'mwheel)
(mouse-wheel-mode t)

;;Bug off with  new frames 
(setq ns-pop-up-frames nil)

;; Default Emacs does not scroll pages smoothly with down arrow key.
; It tries to jump a page-worth.
; See this for advice on preventing that 
; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t)
(setq scroll-conservatively 20)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position 1)

;; programming conveniences:
;(show-paren-mode t) ; light-up matching parens
(global-font-lock-mode t) ; turn on syntax highlight
(setq text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))

;; THEME                                                                     
(add-to-list 'custom-theme-load-path "~/emacs/themes/")
(load-theme 'zenburn t)
(setq linum-format
      (lambda (line)
	(propertize (format
		     (let ((w (length (number-to-string
				       (count-lines (point-min) (point-max))))))
		       (concat " %" (number-to-string w) "d"))
		     line)
		    'face 'linum)))

;; ALIASES
(defalias 'yes-or-no-p 'y-or-n-p)

;; PROGRAMMING STUFF
;;C style conventions
(setq c-default-style "bsd"
c-basic-offset 4)

;;spaces instead of TAB in C/C++ mode
(setq c-mode-hook (function 
		   (lambda () (setq indent-tabs-mode nil)
		     (setq c-indent-level 4))))
(setq c++-mode-hook (function 
		     (lambda ()(setq indent-tabs-mode nil)
		       (setq c-indent-level 4))))

(require 'wvi-functions)
(require 'wvi-modes)

;; KEY BINDINGS
;; C-c C-d for line duplication
(global-set-key "\C-c\C-d" 'duplicate-line)
;; C-c k for killing the rest of the line 
(global-set-key "\C-ck" 'kill-line)
;; C-k for killing the whole line
(global-set-key "\C-k" 'kill-whole-line)
;; C-x C-o find the other file, useful for c/c++
(global-set-key "\C-x\C-o" 'ff-find-other-file)
;; C-q go back to mark, ie point where jumped elsewhere
(global-set-key "\C-q" 'pop-global-mark)
;; Override default tags finding
(global-set-key "\M-?" 'etags-select-find-tag-at-point)
;; Override default tags finding
(global-set-key "\M-." 'etags-select-find-tag)


; man page lookup (by default, f1 is help, but I already know how to
; bring that up using C-h)
(define-key global-map [f1]
(lambda () (interactive) (manual-entry (current-word))))

; F2 to spawn another frame
(define-key global-map [f2] (lambda () (interactive) (make-frame)))

; F3 to kill the other window
(define-key global-map [f3] (lambda () (interactive) (delete-other-windows)))

; F4 for open file C-x C-f is too long
(define-key global-map [f4] 
  (lambda (f) (interactive "FFind File:") (switch-to-buffer (find-file f))))

; F5 for dired buffer of the current directory in the other window
(define-key global-map [f5]
  (lambda () (interactive) (dired-other-window default-directory)))

; F6 list buffers
(define-key global-map [f6]  
  (lambda () 
    (interactive) (list-buffers) (switch-to-buffer-other-frame "*Buffer List*")))

; F7 show bookmarks
(define-key global-map [f7] 
  (lambda () 
    (interactive) (list-bookmarks) (switch-to-buffer-other-window  "*Bookmark List*")))

;; F8 set bookmark
;;(define-key global-map [f8] (lambda () (interactive) (xcode-build)))

; F9 to kill buffer
(define-key global-map [f9] (lambda () (interactive) (kill-buffer (current-buffer))))

; F10 put back the windows (winner mode undo)
(define-key global-map [f10] (lambda () (interactive) (winner-undo)))

; F11 undo-tree-undo 
(define-key global-map [f11] (lambda () (interactive) (undo-tree-undo)))

; F12 undo-tree-redo
(define-key global-map [f12] (lambda () (interactive) (undo-tree-redo)))

