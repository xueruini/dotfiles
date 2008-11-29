;;{{{ define generic mode
  (define-generic-mode
      'my-rc-mode         ;; mode的名字
    '("#" ("/-" . "-/"))  ;; 注释符，可以是行注释符也可以是块注释符
    '(                    ;; 关键字
      "idx" "val" "src" "SHELL")
    '( ;; 还是关键字
      ("\\[\\(.+\\)\\]" 1 'font-lock-string-face t)
      ("\\(\\<[0-9a-fA-F]+\\>\\)" 1 'font-lock-builtin-face)
      ("\\(\\w+\\)=\\(\\w+\\)" 1 'font-lock-keyword-face))
    '(".*rc\\'")          ;; 本mode关联的文件名后缀
    nil                   ;; function-list, 做一些初始化的工作
    "this is a test mode" ;; 本mode的说明文档
    )
;;}}}

;;{{{ % as in Vi
  (global-set-key "%" 'match-paren)
  (defun match-paren (arg)
     "Go to the matching paren if on a paren; otherwise insert %."
     (interactive "p")
     (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
  ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
  (t (self-insert-command (or arg 1)))))
;;}}}

;;{{{ f * as in vi
  ;; go to char
  (defun my-forward-char (n char)
    "Move forward to Nth occurence of CHAR.  Typing
     `my-forward-char-key' again will move forwad to the next Nth
     occurence of CHAR."
    (interactive "p\ncForward to char: ")
    (search-forward (string char) nil nil n)
    (while (char-equal (read-char)  char)
      (search-forward (string char) nil nil n))
    (setq unread-command-events (list last-input-event)))
 
  (defun my-backward-char (n char)
    "Move backward to Nth occurence of CHAR.  Typing `my-backward-char-key'
     again will move forwad to the next Nth occurence of CHAR."
    (interactive "p\ncBackward to char: ")
    (search-backward (string char) nil nil n)
    (while (char-equal (read-char)
                       char)
      (search-backward (string char) nil nil n))
    (setq unread-command-events (list last-input-event)))

  (global-set-key (kbd "M-f") 'my-forward-char)
  (global-set-key (kbd "M-b") 'my-backward-char)
;;}}}

;;{{{ c-mode for indent
  (defun my-indent-or-complete ()
    (interactive)
    (if (looking-at "\\>")
        (hippie-expand nil)
        (indent-for-tab-command)))
  
  (add-hook 'c-mode-common-hook
            (function (lambda ()
                        (define-key c-mode-base-map [(tab)] 'my-indent-or-complete)
                        (define-key c-mode-base-map [(control m)] 'align-newline-and-indent)
                        (outline-minor-mode))))
;;}}}

;;{{{ nice line copy/kill
  (defun my-kill-ring-save (&optional line)
    "This function is a enhancement of `kill-ring-save', which is normal used
    to copy a region.  This function will do exactly as `kill-ring-save' if
    there is a region selected when it is called. If there is no region, then do
    copy lines as `yy' in vim."
    (interactive "P")
    (unless (or line (and mark-active (not (equal (mark) (point)))))
      (setq line 1))
    (if line
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (when (>= line 2)
            (setq end (line-end-position line)))
          (when (<= line -2)
            (setq beg (line-beginning-position (+ line 2))))
          (kill-ring-save beg end))
      (call-interactively 'kill-ring-save)))

  ;; bind it
  (global-set-key [?\M-w] 'my-kill-ring-save)


  (defun my-kill-region (&optional line)
    "This function is a enhancement of `kill-region', which is normal used to
     kill a region to kill-ring.  This function will do exactly as `kill-region'
     if there is a region selected when it is called. If there is no region, then
     do kill lines as `dd' in vim."
    (interactive "P")
    (unless (or line (and mark-active (not (equal (mark) (point)))))
      (setq line 1))
    (if line
        (let ((beg (line-beginning-position))
              (end (line-end-position)))
          (when (>= line 2)
            (setq end (line-end-position line)))
          (when (<= line -2)
            (setq beg (line-beginning-position (+ line 2))))
          (kill-region beg end)
          (if (and (= (line-beginning-position) (line-end-position))
                 (not (= (point) (point-max))))
              (delete-char 1)))
      (call-interactively 'kill-region)))
  ;; bind it
  (global-set-key [?\C-w] 'my-kill-region)

  (defun my-select-this-word ()
    "select the word I am looking at"
    (interactive)
    (forward-char 1)
    (condition-case nil
        (backward-sexp 1) (error ""))
    (when (looking-at "[^a-zA-Z0-9_-]")
      (skip-chars-forward "'\"`*"))
    (condition-case nil
        (mark-sexp) (error ""))
    (kill-new (buffer-substring (mark) (point))))
  ;;(global-set-key [?\C-f2] 'my-select-this-word)
;;}}}
