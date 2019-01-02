;;; package --- Emacs initialization file
;;; Commentary: 
;; Copyright: (C) 2013-2016 Christoph Gerum
;; Based on
;; emacs kicker --- kick start emacs setup
;; Copyright (C) 2010 Dimitri Fontaine
;;
;; Author: Dimitri Fontaine <dim@tapoueh.org>
;; URL: https://github.com/dimitri/emacs-kicker
;; Created: 2011-04-15
;; Keywords: emacs setup el-get kick-start starter-kit
;; Licence: WTFPL, grab your copy here: http://sam.zoy.org/wtfpl/
;;
;; This file is NOT part of GNU Emacs.
;;; Code:
(require 'cl)	; common lisp goodies, loop
(require 'cl-lib)
(require 'package)

(setq package-archives '(("ELPA" . "http://tromey.com/elpa/") 
                         ("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))


(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))


;; now set our own packages
(setq
 my/packages
 '(anaconda-mode             ; python command completion
   switch-window	     ; takes over C-x o
   company                   ; autocompletion support
   company-irony             ; 
   company-anaconda          ;
   company-cmake 
   company-math              ;
   company-emoji             ;
   company-lsp               ;
   company-web               ;
   flycheck                  ;
   color-theme	             ; nice looking emacs
   color-theme-tango         ; check out color-theme-solarized
   org		      
   multiple-cursors          ;multiple cursors mode
   helm                      ;Better completion browsing
   helm-company		  ;
   helm-dash		  ;Search documentation with helm
   helm-pydoc		  ;Search pydocs with helm
   helm-c-yasnippet	  ;
   ;auctex                    ;Latex Mode
   multi-term                ;terminal-emulator
   cpputils-cmake
   fill-column-indicator
   cmake-mode
   yaml-mode                 ;syntax highlighting for yaml
   projectile                ;project management for emacs
   avy                       ;navigate to words starting with letters
   ace-window                ;navigate windows with short letters
   google-translate
   gitignore-mode
   define-word
   markdown-toc
   gitconfig-mode
   gitattributes-mode
   ;;textile-mode
   yasnippet
   yasnippet-snippets
   lsp-mode
   lsp-ui
   ))	

(defun my/install-packages ()
  "Ensure the packages I use are installed. See `my/packages'."
  (interactive)
  (let ((missing-packages (cl-remove-if #'package-installed-p my/packages)))
    (when missing-packages
      (message "Installing %d missing package(s)" (length missing-packages))
      (package-refresh-contents)
      (mapc #'package-install missing-packages))))

(my/install-packages)


;; on to the visual settings
(set-default-font "Liberation Mono 14") ;Select default font
(setq inhibit-splash-screen t)	; no splash screen, thanks
(line-number-mode 1)	; have line numbers and
(column-number-mode 1)	; column numbers in the mode line

(tool-bar-mode -1)	; no tool bar with icons
(scroll-bar-mode -1)	; no scroll bars

(global-hl-line-mode)	; highlight current line
(global-linum-mode 1)	; add line numbers on the left

;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))

;; copy/paste with C-c and C-v and C-x, check out C-RET too
(cua-mode)

;; Use the clipboard, pretty please, so that copy/paste "works"
(setq x-select-enable-clipboard t)

;; Navigate windows with M-<arrows>
(windmove-default-keybindings 'meta)
(setq windmove-wrap-around t)

;; Navigate windows with ace-window
(require 'ace-window)
(global-set-key (kbd "M-p") 'ace-window)

;; winner-mode provides C-<left> to get back to previous window layout
(winner-mode 1)


;; M-x shell is a nice shell interface to use, let's make it colorful. If
;; you need a terminal emulator rather than just a shell, consider M-x term
;; instead.
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; If you do use M-x term, you will notice there's line mode that acts like
;; emacs buffers, and there's the default char mode that will send your
;; input char-by-char, so that curses application see each of your key
;; strokes.
;;
;; The default way to toggle between them is C-c C-j and C-c C-k, let's
;; better use just one key to do the same.
(require 'term)
(define-key term-raw-map (kbd "C-'") 'term-line-mode)
(define-key term-mode-map (kbd "C-'") 'term-char-mode)

;; Have C-y act as usual in term-mode, to avoid C-' C-y C-'
;; Well the real default would be C-c C-j C-y C-c C-k.
(define-key term-raw-map (kbd "C-y") 'term-paste)

;;Enable Helm Mode for completion
;(global-set-key (kbd "M-x")	   'undefined)
;(global-set-key (kbd "M-x")	   'helm-M-x)
;(global-set-key (kbd "C-x r b")   'helm-filtered-bookmarks)
;(global-set-key (kbd "C-x C-f")   'helm-find-files)

;(helm-mode 1)

;; default key to switch buffer is C-x b, but that's not easy enough
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs. Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x B") 'ibuffer)


;;git
(require 'gitignore-mode)
(require 'gitconfig-mode)
(require 'gitattributes-mode)




;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

;;google-translate
(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key "\C-ct" 'google-translate-smooth-translate)

;;define-word
(require 'define-word)
(global-set-key "\C-cd" 'define-word-at-point)


;; full screen
(defun fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen
		       (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
(global-set-key [f11] 'fullscreen)

;;Settings for org mode
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
   )
 )

(setq org-log-done 'time)

;;EPresent
(global-set-key [f12] 'epresent-run)


;;Yasnippet
(require 'yasnippet)
(setq yas-snippet-dirs (append yas-snippet-dirs
                               '("~/.emacs.d/snippets")))
(yas-global-mode 1)

;;Auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)



;;markdown
(require 'markdown-mode)
(require 'markdown-toc)


;;textile
;;(require 'textile-mode)
;;(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

;;Anaconda mode for python completions 
(add-hook 'python-mode-hook
	  #'(lambda ()
              (add-to-list 'python-shell-extra-pythonpaths "/local/gerum/projects/timing/timing-annotation")
	      (when (projectile-project-p)
		(add-to-list 'python-shell-extra-pythonpaths projectile-project-root)
		
		(message "project root is %s."
			 (projectile-project-root))
		(anaconda-mode))))

;(require 'helm-pydoc)
;(define-key python-mode-map (kbd "C-c C-d") 'helm-pydoc)


;;Company-Mode
(add-hook 'after-init-hook 'global-company-mode)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
	(backward-char 1)
	(if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
	    (null (do-yas-expand)))
	(if (check-expansion)
	    (company-complete-common)
	  (indent-for-tab-command)))))

(global-set-key [tab] 'tab-indent-or-complete)


;;Enable projectile for to set globals
(require 'projectile)
(projectile-global-mode)



;;Flycheck syntax checking for emacs



;;Flyspell
(add-hook 'c-mode-hook 
	  (lambda ()
	    (flyspell-prog-mode)))
(add-hook 'c++-mode-hook 
	  (lambda ()
	    (flyspell-prog-mode)))
(add-hook 'python-mode-hook 
	  (lambda ()
	    (flyspell-prog-mode)))

(add-hook 'LaTeX-mode-hook 'turn-on-flyspell)


;; cppcm-utils cmake-mode
(require 'cpputils-cmake)

(add-hook 'c-mode-common-hook
          (lambda ()
            (if (derived-mode-p 'c-mode 'c++-mode)
                (cppcm-reload-all)
              )))

(setq cppcm-write-flymake-makefile nil)

;; avoid typing full path when starting gdb
(global-set-key (kbd "C-c C-g")
		'(lambda ()(interactive) (gud-gdb (concat "gdb --fullname " (cppcm-get-exe-path-current-buffer)))))


;; Doxymacs mode for doxygen Editing
;(require 'doxymacs)

;; Automatically load it for c and c++ and python
;(add-hook 'c-mode-common-hook 'doxymacs-mode)

;(defun custom-doxymacs-font-lock-hook ()
;  (if (or (eq major-mode 'c-mode) (eq major-mode 'c++-mode))
;      (doxymacs-font-lock)))
;(add-hook 'font-lock-mode-hook 'custom-doxymacs-font-lock-hook)


;;Macros for org-mode
(defun org-hex-strip-lead (str)
  (if (and (> (length str) 2) (string= (substring str 0 2) "0x"))
      (substring str 2) str))

(defun org-hex-to-hex (int)
  (format "0x%x" int))

(defun org-hex-to-dec (str)
  (cond
   ((and (stringp str)
         (string-match "\\([0-9a-f]+\\)" (setf str (org-hex-strip-lead str))))
    (let ((out 0))
      (mapc
       (lambda (ch)
         (setf out (+ (* out 16)
                      (if (and (>= ch 48) (<= ch 57)) (- ch 48) (- ch 87)))))
       (coerce (match-string 1 str) 'list))
      out))
   ((stringp str) (string-to-number str))
   (t str)))

(defmacro with-hex (hex-output-p &rest exprs)
  "Evaluate an org-table formula, converting all fields that look
    like hexadecimal to decimal integers.  If HEX-OUTPUT-P then
    return the result as a hex value."
  (list
   (if hex-output-p 'org-hex-to-hex 'identity)
   (cons 'progn
         (mapcar
          (lambda (expr)
            `,(cons (car expr)
                    (mapcar (lambda (el)
                              (if (listp el)
                                  (list 'with-hex nil el)
                                (org-hex-to-dec el)))
                            (cdr expr))))
          `,@exprs))))




;; TRAMP
(setq tramp-default-method "ssh")

;; uniquify.el is a helper routine to help give buffer names a better unique name.
(when (load "uniquify" 'NOERROR)
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'forward)
  ;(setq uniquify-buffer-name-style 'post-forward)
  )

(put 'scroll-left 'disabled nil)

;; Semantic refactoring and additional code completion

;;(require 'srefactor)
;;(semantic-mode 1) 
;; 
;;(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
;;(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)


;;GUD Configuration
;;(setq gdb-many-windows t)
(setq gdb-show-main t)
(setq gud-tooltip-mode t)

;; F5 now allows compiling the project0123
(global-set-key (kbd "<f5>") (lambda ()
                               (interactive)
                               (setq-local compilation-read-command nil)
                               (call-interactively 'compile)))


;; C-Styles
;; Add a cc-mode style for editing LLVM C++ code
(c-add-style "llvm.org"
             '((fill-column . 80)
	       (c++-indent-level . 2)
	       (c-basic-offset . 2)
	       (indent-tabs-mode . nil)
	       (c-offsets-alist . ((arglist-intro . ++)
				   (innamespace . 0)
				   (member-init-intro . ++)
				   ))
               ))

;; Files with "llvm" in their names will automatically be set to the
;; llvm.org coding style.
;;(add-hook 'c-mode-hook
;; 	  (function
;; 	   (lambda nil 
;; 	     (if (string-match "llvm" buffer-file-name)
;; 		 (progn
;; 		   (c-set-style "llvm.org")
;; 		   )
;; 	       ))))
;; 
;;(add-hook 'c++-mode-hook
;; 	  (function
;; 	   (lambda nil 
;; 	     (if (string-match "llvm" buffer-file-name)
;; 		 (progn
;; 		   (c-set-style "llvm.org")
;; 		   )
;; 	       ))))


;; Use llvm.org as default coding style
(add-hook 'c-mode-hook
	  (function
	   (lambda nil 
	     (progn
	       (c-set-style "llvm.org")
	       )
	     )))

(add-hook 'c++-mode-hook
	  (function
	   (lambda nil 
	     (progn
	       (c-set-style "llvm.org")
	       )
	     )))




(provide 'init)


(require 'lsp-mode)
(require 'lsp-clients)
(add-hook 'prog-mode-hook #'lsp)

;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (lsp-ui company-lsp yaml-mode switch-window srefactor projectile multiple-cursors multi-term markdown-toc magit lsp-mode helm-pydoc helm-dash helm-company helm-c-yasnippet google-translate gitignore-mode gitconfig-mode gitattributes-mode flycheck-irony fill-column-indicator define-word cpputils-cmake company-web company-math company-irony company-emoji company-cmake company-anaconda color-theme-tango cmake-mode auctex ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
