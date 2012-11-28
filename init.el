;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://raw.github.com/dimitri/el-get/master/el-get-install.el"
   (lambda (s)
     (goto-char (point-max))
     (eval-print-last-sexp))))

(el-get 'sync)

;;no splash screen
(setq inhibit-splash-screen t)

;;org
(require 'org)

(org-babel-do-load-languages
 'org-babel-load-languages
 '(
   (C . t)
   ;(c++ . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (java .t)
   (js .t)
   (latex . t)
   (matlab . t)
   (ocaml . t)
   (org . t)
   (plantuml . t)
   (python . t)
   (sh . t)
   )
 )

(setq org-log-done 'time)

;; clang-completion

;(require 'clang-completion-mode)


;;Speedbar
(require 'sr-speedbar)
(setq sr-speedbar-right-side nil)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)
