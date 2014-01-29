(provide 'wvi-init)


;;; GENERAL SETTINGS
; to undo, do M-x tool-bar-mode;
(tool-bar-mode -1)
(blink-cursor-mode -1) ;; blinking cursor is evil
(scroll-bar-mode -1) ;; scrollbar is pretty useless

;;Save me buffers and stuff in the rare occasion that I close emacs
(desktop-save-mode t)

; don't show the startup screen
(setq inhibit-startup-screen t)

; line and column number in all files, all the time
; and please higlight those bloody parenthesis
(global-linum-mode t)
(column-number-mode t)
(show-paren-mode t)

; make sure transient mark mode is enabled (it should be by default,
; but just in case)
(transient-mark-mode t)

; turn on mouse wheel support for scrolling
; some people get confused when the mouse dosn't work
(require 'mwheel)
(mouse-wheel-mode t)

; Bug off with  new frames
(setq ns-pop-up-frames nil)

; Default Emacs does not scroll pages smoothly with down arrow key.
; It tries to jump a page-worth.
; See this for advice on preventing that
; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t)
(setq scroll-conservatively 20)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position 1)

; programming conveniences:
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

;;AUTOBACKUP
(setq backup-directory-alist 
      '(("." . "~/.emacs.d/backups/")))
; always use copying to create backup files (don't clobber symlinks)
(setq backup-by-copying t)
; make numeric backup versions
(setq version-control t)
; number of oldest versions to keep when a new numbered backup is made
(setq kept-old-versions 2)  ; 2
; number of newest versions to keep when a new numbered backup is made
(setq kept-new-versions 20)  ; 2
; delete excess backup versions silently
(setq delete-old-versions t)
;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; AUTOSAVES
(setq auto-save-file-name-transforms
          `((".*" "~/.emacs.d/backups/" t)))

;; ABBREVS-FILE
(setq abbrev-file-name "~/emacs/abbrev_defs")

;; PROGRAMMING STUFF
;;C style conventions
(setq c-default-style "bsd"
      c-basic-offset 4)
;spaces instead of TAB in C/C++ mode
(setq c-mode-hook (function
		   (lambda () (setq indent-tabs-mode nil)
		     (setq c-indent-level 4))))

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

;.h are most of the C++ files in my case so use that as default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(require 'wvi-functions)
(require 'wvi-modes)

;; KEY BINDINGS

;; C-c C-d for line duplication
(global-set-key "\C-c\C-d" 'duplicate-line)
(global-set-key "\C-cd" 'duplicate-line)
;;C-c c for copy line
(global-set-key "\C-cc" 'copy-line-or-region)
;; C-c k for killing the rest of the line
(global-set-key "\C-ck" 'kill-line)
;; C-k for killing the whole line
(global-set-key "\C-k" 'kill-whole-line)
;; <Super>TAB for switching windows
(global-set-key (kbd "s-s") 'other-window)
;; C-x C-o find the other file, useful for c/c++
(global-set-key "\C-x\C-o" 'ff-find-other-file)
;; C-q go back to mark, ie point where jumped elsewhere
(global-set-key "\C-cq" 'pop-global-mark)


;;GO TO CHAR
(global-set-key (kbd "C-c f") 'iy-go-up-to-char)
(global-set-key (kbd "C-c F") 'iy-go-up-to-char-backward)
(global-set-key (kbd "C-c ;") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "C-c ,") 'iy-go-to-or-up-to-continue-backward)

;; THING AT POINT EDIT
(global-set-key "\C-cw" 'thing-copy-word)
(global-set-key "\C-cW" 'thing-paste-word)
(global-set-key "\C-cs" 'thing-copy-symbol)
(global-set-key "\C-cS" 'thing-paste-symbol)
(global-set-key "\C-cv" 'thing-copy-sentence)
(global-set-key "\C-cV" 'thing-paste-sentence)


;; HIDE-SHOW
(global-set-key "\C-ch" 'hs-hide-block)
(global-set-key "\C-ce" 'hs-show-block)
(global-set-key "\C-cH" 'hs-hide-all)
(global-set-key "\C-cE" 'hs-show-all)

;; TABBAR
(global-set-key [f1] 'tabbar-forward)
(global-set-key [(control f1)] 'tabbar-backward)
(global-set-key (kbd "C-S-p") 'tabbar-backward-group)
(global-set-key (kbd "C-S-n") 'tabbar-forward-group)

; F2 YASNIPPET insert and expand

; F3 macrobindings
(global-set-key [f3] 'start-kbd-macro)
(global-set-key [(control f3)] 'end-kbd-macro)
(global-set-key [(control shift f3)] 'name-last-kbd-macro)

; F4 expand/contract-region

; F5 for dired buffer of the current directory in the other window
(define-key global-map [f5]
  (lambda () (interactive) (dired-other-window default-directory)))

; F6 list buffers
(define-key global-map [f6]
  (lambda ()
    (interactive) (list-buffers) (switch-to-buffer-other-window "*Buffer List*")))

; F7 show bookmarks
(define-key global-map [f7]
  (lambda ()
    (interactive) (list-bookmarks) (switch-to-buffer-other-window  "*Bookmark List*")))

;; F8 ?

; F9 to kill buffer
(define-key global-map [f9] (lambda () (interactive) (kill-buffer (current-buffer))))

; F10 put back the windows (winner mode undo)
; C-F10 winner redo
(define-key global-map [f10] (lambda () (interactive) (winner-undo)))
(define-key global-map [(control f10)] (lambda () (interactive) (winner-redo)))

; F11 undo-tree-undo
; C-F11 undo-tree-redo
(define-key global-map [f11] (lambda () (interactive) (undo-tree-undo)))
(define-key global-map [(control f11)] (lambda () (interactive) (undo-tree-redo)))

; F12 ?