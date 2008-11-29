;;{{{
  (icomplete-mode t)      ;incremental minibufer completion (like iswitchb)
;;}}}

;;{{{ Goto last change
  (require 'goto-last-change)
  ;; C-x C-\ :jump to the last change point
  (global-set-key (kbd "<C-f5>") 'goto-last-change)
;;}}}

;;{{{ Redo/Undo
  (require 'redo)
;;}}}

;;{{{ Pager
  ;; Excellent package for better scrolling in emacs
  (require 'pager)
  (global-set-key [(next)]       'pager-page-down)
  (global-set-key [(prior)]      'pager-page-up)
  (global-set-key [(meta down)]  'pager-row-down)
  (global-set-key [(meta up)]    'pager-row-up)
;;}}}

;;{{{ LaTeX settings
   ;;start AucTeX
   (require 'tex-site)

   ;; If you want to make AUCTeX aware of style files and multi-file
   ;; documents right away, insert the following in your `.emacs' file.
   (setq TeX-auto-save t)
   (setq TeX-parse-self t)
   (setq TeX-electric-escape t)
   (setq-default TeX-master nil)

   ;;To integrate RefTeX with AUCTeX, use
   ;(setq reftex-plug-into-AUCTeX t)

   ;; configure CDLaTeX
   (autoload 'cdlatex-mode "cdlatex" "CDLaTeX Mode" t)
   (autoload 'turn-on-cdlatex "cdlatex" "CDLaTeX Mode" nil) 

   ;; I just want to open outline-minor-mode 
   (defun my-LaTeX-mode ()
     (outline-minor-mode)
     (turn-on-reftex)
     (setq reftex-plug-into-AUCTeX t)     ;;To integrate RefTeX with AUCTeX
     (turn-on-cdlatex))

   ;; Yeah, start my-LaTeX-mode function
   (add-hook 'LaTeX-mode-hook 'my-LaTeX-mode)   ; with AUCTeX LaTeX mode
   (add-hook 'latex-mode-hook 'my-LaTeX-mode)   ; with Emacs latex mode

;;}}}

;;{{{ Color Themes
  (require 'color-theme)
  (color-theme-initialize)
  (color-theme-gnome2)
;;}}}

;;{{{ Mark Rectangle
  (require 'rect-mark)
  (define-key ctl-x-map "r\C-@" 'rm-set-mark)
  (define-key ctl-x-map "r\C-x" 'rm-exchange-point-and-mark)
  (define-key ctl-x-map "r\C-w" 'rm-kill-region)
  (define-key ctl-x-map "r\M-w" 'rm-kill-ring-save)
  (define-key global-map [S-down-mouse-1] 'rm-mouse-drag-region)
  (autoload 'rm-set-mark "rect-mark"
    "Set mark for rectangle." t)
  (autoload 'rm-exchange-point-and-mark "rect-mark"
    "Exchange point and mark for rectangle." t)
  (autoload 'rm-kill-region "rect-mark"
    "Kill a rectangular region and save it in the kill ring." t)
  (autoload 'rm-kill-ring-save "rect-mark"
    "Copy a rectangular region to the kill ring." t)
  (autoload 'rm-mouse-drag-region "rect-mark"
    "Drag out a rectangular region with the mouse." t)
  
  ;; Use this section in your "~/.emacs" to modify picture mode so that
  ;; it automatically uses the rect-mark equivalents of many commands.
  
  ;; One vision of a better picture mode.
  (add-hook 'picture-mode-hook 'rm-example-picture-mode-bindings)
  (autoload 'rm-example-picture-mode-bindings "rect-mark"
    "Example rect-mark key and mouse bindings for picture mode.")
;;}}}

;;{{{ Show Line Number
  (require 'linum)
  (global-linum-mode t)
  (global-set-key [(f9)] 'linum-mode)    
;;}}}

;;{{{ Buffer Manager
  ;; ido.el--- convenient to switch between buffers
     (require 'ido)
     (setq ido-enable-flex-matching t) ; fuzzy matching
     (setq ido-auto-merge-delay-time 2)
     ; use C-j to create file
     (ido-mode t)
;;}}}

;;{{{ ibuffer.
     (require 'ibuffer)
     (global-set-key (kbd "C-x C-b") 'ibuffer)
     (setq ibuffer-formats '((mark modified read-only " " (name 16 16) " "
                                   (size 6 -1 :right) " " (mode 16 16 :center)
                                   " " (process 8 -1) " " filename)
                             (mark " " (name 16 -1) " " filename))
           ibuffer-elide-long-columns t)

;;}}}

;;{{{ align 
  (require 'align)
;;}}}
