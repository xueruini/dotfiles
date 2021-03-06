(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yaml-mode sphinx-doc go-add-tags magit markdown-mode rainbow-mode latex-preview-pane foggy-night-theme sublime-themes web-mode undo-tree sql-indent slime python-docstring php-mode nginx-mode lua-mode jsx-mode json-mode graphviz-dot-mode go-errcheck go-eldoc go-dlv go-direx go-complete gitignore-mode gitconfig-mode gitattributes-mode git-gutter flycheck-tip flycheck-status-emoji flycheck-color-mode-line exec-path-from-shell dockerfile-mode django-manage color-theme-sanityinc-solarized color-theme cider auctex anaconda-mode aggressive-indent)))
 '(reb-re-syntax (quote string))
 '(safe-local-variable-values (quote ((TeX-modes . latex) (TeX-engine . pdflatex))))
 '(speedbar-show-unknown-files t)
 '(speedbar-use-images nil))

(show-paren-mode t)
(setq show-paren-style 'expression)

(when (display-graphic-p)
  (tooltip-mode -1)
  (tool-bar-mode -1)
  ;; (menu-bar-mode -1)
  (scroll-bar-mode -1))

(setq make-backup-files nil)

(setq-default line-spacing 4)

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
;; (setq-default auto-fill-function 'do-auto-fill)
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
               '("Fira Mono" "Cousine" "Droid Sans Mono" "Manaco" "STHeiti"))
              ((eq system-type 'gnu/linux)
               '("Menlo" "WenQuanYi Zen Hei"))
              ((eq system-type 'windows-nt)
               '("Consolas"  "Microsoft Yahei"))))
  (setq face-font-rescale-alist
        '(("STHeiti" . 1.2)
          ("Microsoft Yahei" . 1.2)
          ("WenQuanYi Zen Hei" . 1.2)))
  (set-face-attribute 'default nil :font
                      (format "%s:pixelsize=%d" (car fonts) 14))
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
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

;; (package-install 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
(setq undo-tree-visualizer-diff t)

;; auctex & reftex
;; (package-install 'auctex)
(add-hook 'LaTeX-mode-hook 'tex/enablers)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'tex/enablers)   ; with Emacs latex mode
(setq reftex-plug-into-AUCTeX t)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(setq TeX-source-correlate-method 'synctex)
(defun tex/enablers ()
  (turn-on-reftex)
  (add-to-list 'TeX-command-list '("Make" "make" TeX-run-command nil t)))
;; (package-install 'cdlatex)
(add-hook 'LaTeX-mode-hook 'turn-on-cdlatex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-cdlatex)   ; with Emacs latex mode
;; (package-install 'latex-preview-pane)
(latex-preview-pane-enable)

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
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; color-theme
;; (package-install 'color-theme)
(when (display-graphic-p)
  (color-theme-initialize)
  ; BUG: load diff-mode (or any necessary on error) before issuing color-theme-select
  ; (require 'diff-mode)
  (color-theme-xemacs)
  ; (package-install 'color-theme-sanityinc-solarized)
  ; (load-theme 'sanityinc-solarized-dark t)
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
; as major mode
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;; just for linting in minor mode
;; (add-hook 'js-mode-hook 'js2-minor-mode)
(setq-default js2-highlight-level 3
              js2-basic-offset 2
              js2-bounce-indent-p nil
              js2-auto-indent-p t
              js2-include-node-externs t
              js2-include-browser-externs t
              js2-skip-preprocessor-directives t)
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)

;; python
;; (package-install 'anaconda-mode)
;; (package-install 'eldoc-mode)
;; (package-install 'sphinx-doc)
(add-hook 'python-mode-hook (lambda ()
                              (anaconda-mode t)
                              (eldoc-mode t)
                              (sphinx-doc-mode t)))
;; flycheck
;; (package-install 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              '(javascript-jshint))

;; phpcbf (install phpcs first)
;; Auto format on save.
; (add-hook 'php-mode-hook 'phpcbf-enable-on-save)

;; (package-install 'git-gutter)
(global-git-gutter-mode t)
(git-gutter:linum-setup)

;; aggressive-indent
(global-aggressive-indent-mode 1)

;; (package-install 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; (when (fboundp 'electric-indent-mode) (electric-indent-mode -1))
