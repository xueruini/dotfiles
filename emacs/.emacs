;; -*- Mode: lisp -*-

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(line-spacing 2 t))

;;{{{ Setup Load Path
(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
    (let* ((my-lisp-dir "~/emacs/local-lisp/")
           (default-directory my-lisp-dir))
      (setq load-path (cons my-lisp-dir load-path))
      (normal-top-level-add-subdirs-to-load-path)))
;;}}} 

;;;;;;;;;;;;;;;;;;;;EMACS MAKES A CRAZY SOUL;;;;;;;
(load "~/emacs/myGlobals")
(load "~/emacs/myTools")
(load "~/emacs/myFunctions")
