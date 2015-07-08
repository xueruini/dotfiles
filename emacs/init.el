(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flycheck-phpcs-standard "WordPress")
 '(php-mode-coding-style (quote wordpress))
 '(phpcbf-standard "WordPress")
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values (quote ((TeX-modes . latex) (TeX-engine . pdflatex))))
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil))

(when (display-graphic-p)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  ; (menu-bar-mode -1)
  (scroll-bar-mode -1))

(setq make-backup-files nil)

(setq-default line-spacing 4)

(show-paren-mode t)
(setq show-paren-style 'expression)

;; http://www.emacswiki.org/emacs/NavigatingParentheses
(defun goto-match-paren (arg)
  "Go to the matching parenthesis if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key "%" 'goto-match-paren)

;; fill
(setq-default auto-fill-function 'do-auto-fill)
(setq-default fill-column 80)
;; https://github.com/xahlee/xah_emacs_init/blob/master/xah_emacs_font.el
(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0)))

;; line & column number
(global-linum-mode t)
(when (not (display-graphic-p))
  (setq linum-format "%d "))
; (global-hl-line-mode t)
(line-number-mode t)
(column-number-mode t)

;; tab & indentation
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; make tab key do indent first then completion.
;(set-default tab-always-indent 'complete)
;; make return key also do indent, globally
;(electric-indent-mode 1)

;; smooth scrolling
(setq scroll-step           1
      scroll-conservatively 10000)

;; fonts
(when (display-graphic-p)
  (setq fonts
        (cond ((eq system-type 'darwin)
               ; '("Manaco" "STHeiti"))
               '("Droid Sans Mono" "STHeiti"))
              ((eq system-type 'gnu/linux)
               '("Menlo" "WenQuanYi Zen Hei"))
              ((eq system-type 'windows-nt)
               '("Consolas"  "Microsoft Yahei"))))
  (setq face-font-rescale-alist
        '(("STHeiti" . 1.2)
		  ("Microsoft Yahei" . 1.2)
		  ("WenQuanYi Zen Hei" . 1.2)))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 13))
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font) charset
                      (font-spec :family (car (cdr fonts))))))

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)

;; package system >= emacs-version-24
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

;; PATH fix for macos
;; (package-install 'exec-path-from-shell)
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (package-install 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; auctex & reftex
;; (package-install 'auctex)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

;; helm
;; (package-install 'helm)
(require 'helm-config)
(require 'helm-grep)

;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebihnd tab to do persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-grep-mode-map (kbd "<return>")  'helm-grep-mode-jump-other-window)
(define-key helm-grep-mode-map (kbd "n")  'helm-grep-mode-jump-other-window-forward)
(define-key helm-grep-mode-map (kbd "p")
  'helm-grep-mode-jump-other-window-backward)

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-quick-update                     t ; do not display invisible candidates
      helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; markdown
;; (package-install 'markdown-mode)
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; color-theme
;; (package-install 'color-theme)
(when window-system
  (require 'color-theme)
  (color-theme-initialize)
  ; BUG: load diff-mode (or any necessary on error) before issuing color-theme-select
  ; (require 'diff-mode)
  ; (color-theme-snowish)
  ; (package-install 'color-theme-solarized)
  (load-theme 'solarized-dark t)
  ; (load-theme 'solarized-light t)
  )

;; sr-speedbar
;; (package-install 'sr-speedbar)

;; slime
;; (package-install 'slime)
(setq inferior-lisp-program "/usr/local/bin/sbcl")

;; clojure
;; (package-install 'cider)

;; php
;; (package-install 'php-mode)
(add-hook 'php-mode-hook 'php-enable-wordpress-coding-style)

;; javascript
;; (package-install 'js2-mode)
(add-hook 'js-mode-hook 'js2-minor-mode)
(setq js2-highlight-level 3)
(setq js2-basic-offset 4)
(setq js-indent-level 4)
(setq js-switch-indent-offset 4)

;; flycheck
;; (package-install 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; phpcbf (install phpcs first)
;; Auto format on save.
; (add-hook 'php-mode-hook 'phpcbf-enable-on-save)
