;;{{{ For Chinese...
  (defun xrn-load-gbk ()
    "Load mult-gbk for win32/linux platform"
    (require 'mule-gbk)
    (setq w32-charset-info-alist
          (cons '("gbk" w32-charset-gb2312 . 936) w32-charset-info-alist))
    (set-w32-system-coding-system 'chinese-gbk)
    (set-selection-coding-system 'chinese-gbk)
    (set-keyboard-coding-system 'chinese-gbk)
    (set-language-environment 'chinese-gbk)
    (setq locale-coding-system 'chinese-gbk)
    (setq current-language-environment "Chinese-GBK")
  
    ; -*-Courier New-normal-r-*-*-11-*-*-*-c-*-fontset-chinese,
    (create-fontset-from-fontset-spec
     "-*-DejaVu Sans Mono-normal-r-*-*-14-*-*-*-c-*-fontset-chinese,
    chinese-gbk:-*-ËÎÌå-normal-r-normal-normal-*-*-96-96-c-*-gbk" t)
    (set-default-font "fontset-chinese")
  
    ; fill mode for chinese
    ;(setq sentence-end-double-space t)
    ;(setq scalable-fonts-allowed t)
    ;(put-charset-property 'chinese-cns11643-5 'nospace-between-words t)
    ;(put-charset-property 'chinese-cns11643-6 'nospace-between-words t)
    ;(put-charset-property 'chinese-cns11643-7 'nospace-between-words t)
  
    (setq sentence-end "\\([£¥£¬¡££¡£¿¡¢£»£º¡ª¡¤¡®¡¯¡°¡±¡«£¢£§£à¡´¡µ¡²¡³¡¶¡·¡¼¡½¡¾¡¿£Û£Ý£¨£©£û£ý]\\|¡­¡­\\|[ . ?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"))
  
  ; we only load mule-gbk for non-mac platform.
  ; (if (not (string-match "darwin" (emacs-version)))
  ;    (xrn-load-gbk))
;;}}}

;;{{{ global font setup
  ;(set-default-font "Courier New:pixelsize=14")
  (set-default-font "Consolas:pixelsize=18")
  (set-fontset-font (frame-parameter nil 'font)
  		  'han (font-spec :family "SimSun" :size 20))
  (set-fontset-font (frame-parameter nil 'font)
  		  'symbol (font-spec :family "SimSun" :size 20))
  (set-fontset-font (frame-parameter nil 'font)
  		  'cjk-misc (font-spec :family "SimSun" :size 20))
  (set-fontset-font (frame-parameter nil 'font)
  		  'bopomofo (font-spec :family "SimSun" :size 20))
;;}}}

;;{{{ Global Settings...
  (if (string-match "Aquamacs" (emacs-version))
      (setq cursor-type 'box)) ; block, box

  (setq-default ispell-program-name "aspell")
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; disable the welcome screen
  ;(setq inhibit-startup-message t)

  ;; Maximum buffer size (in characters) for line number display If
  ;; the buffer is bigger than this, the line number does not appear in
  ;; the mode line:
  (setq line-number-display-limit 1000000)
  (setq line-number-mode t) 
  ;(column-number-mode t)

  (tool-bar-mode 1)

  (transient-mark-mode 1)

  (setq auto-save-interval 300)

  ;; default tab width
  (setq default-tab-width 4)

  ;; kill the whole line when point in the begin
  (setq-default kill-whole-line t)

  ;; paren matching
  (show-paren-mode t)
  ; (setq show-paren-style 'parentheses) ; 'expression, 'mixed

  ;; Toggle Delete Selection mode.
  ;; When ON, typed text replaces the selection if the selection is active.
  ;; When OFF, typed text is just inserted at point.
  (delete-selection-mode 1)

  ;; Indentation can insert tabs if this is non-nil:
  ;; Setting this variable automatically makes it local to the current buffer.
  (setq-default indent-tabs-mode nil) 

  ;; avoid jump when scrolling
  (set-scroll-bar-mode nil)
  (setq scroll-step 1
        scroll-margin 2
        scroll-conservatively 1000)

  ;; Show buffer name in title
  (setq frame-title-format "%f")

  ;; auto show image
  (auto-image-file-mode)

  ;; auto fill mode
  (setq-default auto-fill-function 'do-auto-fill)
  (setq default-fill-column 80)

  ;; do not backup files
  (setq make-backup-file nil)

  ;; syntax on
  (global-font-lock-mode t)
  (setq font-lock-maximum-decoration t)
  (setq lazy-lock-defer-on-scrolling t)
  (setq font-lock-support-mode 'jit-lock-mode)

  ;; Non-nil means try to flash the frame to represent a bell.
  ; (setq visible-bell t)

  ;; Show date and time
  ;; refer to the Linux man page for "strftime - format date and time"
  (setq display-time-format "[%A %b%d] [%p %H:%M]")
  (setq display-time-day-and-date t)
  ; (setq display-time-24hr-format t)
  (display-time-mode 1)

  ;; Stop startup picture
  (setq inhibit-startup-message t)

  ;; reuse the dired buffer for new directory
  (put 'dired-find-alternate-file 'disabled nil)
;;}}}

;;{{{ short cuts

  ; redo, uses the custom redo.el
  (global-set-key [(f3)] 'undo)
  (global-set-key [(f4)] 'redo)            

  ;; S+<arrow> to select
  ; (pc-selection-mode)

  ; easy buffer cycle, pc-selection-mode would define M-right as move right
  (global-set-key [C-M-right] 'next-buffer)
  (global-set-key [C-M-left] 'previous-buffer)

  ; move cursor in different windows
  (windmove-default-keybindings 'meta) ; +left/right/up/down

  ; set mark instead of C-@
  (global-set-key [?\C-`] 'set-mark-command)

  ; kill this buffer without promption
  ; (global-set-key "\C-xk" 'kill-this-buffer)
  (global-set-key (kbd "<C-f4>") 'kill-this-buffer)

  ; make RET more smart
  ;(global-set-key [(control m)] 'align-newline-and-indent)


  ; hight light current line
  ;(define-key global-map (kbd "C-c l") 'hl-line-mode) 
  (global-set-key [(f5)] 'hl-line-mode)

  (global-set-key [(f6)] 'outline-minor-mode)
  (global-set-key [(f7)] 'auto-fill-mode)
  ; (f9) for linum-mode toggle
  (global-set-key [(f11)] 'ispell-word)
  (global-set-key [(f12)] 'view-mode)

  ; goto line
  (global-set-key [(meta g)] 'goto-line)

  (global-set-key [(shift tab)] 'ido-switch-buffer)
  (global-set-key [(control tab)] 'other-window)

  (global-set-key [?\C-1] 'delete-other-windows)
  (global-set-key [?\C-0] 'delete-window)
  (global-set-key [?\C-2] 'split-window-vertically)
  (global-set-key [?\C-3] 'split-window-horizontally)
;;}}}

;;{{{ Personal Information
  (setq user-full-name "Ruini Xue")
  (setq user-mail-address "xueruini@gmail.com")
;;}}}

