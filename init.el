;; Copyright: (C) 2013 Christoph Gerum
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


(require 'cl)	; common lisp goodies, loop

(require 'package)
(add-to-list 'package-archives
  ;; The 't' means to append, so that MELPA comes after the more
  ;; stable ELPA archive.
  '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Add ELPA if necessary. Looking at the El-Get package.rcp recipe in
;; ~/local/opt/el-get/recipes it seems this is probably unnecessary.
(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))

(package-initialize)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")


(unless (require 'el-get nil t)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://github.com/dimitri/el-get/raw/master/el-get-install.el")
    (end-of-buffer)
    (eval-print-last-sexp)))



(require 'el-get-elpa)
(unless (file-directory-p el-get-recipe-path-elpa)
  (el-get-elpa-build-local-recipes))

;; set local recipes
(setq
 el-get-sources
 '((:name buffer-move	; have to add your own keys
	  :after (progn
		   (global-set-key (kbd "<C-S-up>") 'buf-move-up)
		   (global-set-key (kbd "<C-S-down>") 'buf-move-down)
		   (global-set-key (kbd "<C-S-left>") 'buf-move-left)
		   (global-set-key (kbd "<C-S-right>") 'buf-move-right)))

   (:name smex	; a better (ido like) M-x
	  :after (progn
		   (setq smex-save-file "~/.emacs.d/.smex-items")
		   (global-set-key (kbd "M-x") 'smex)
		   (global-set-key (kbd "M-X") 'smex-major-mode-commands)))

   (:name magit	; git meet emacs, and a binding
	  :after (progn()
		       (global-set-key (kbd "C-x C-z") 'magit-status)))
   
   (:name goto-last-change	; move pointer back to last change
	  :after (progn
		   ;; when using AZERTY keyboard, consider C-x C-_
		   (global-set-key (kbd "C-x C-_") 'goto-last-change)))))

;; now set our own packages
(setq
 my:el-get-packages
 '(escreen              ; screen for emacs, C-\ C-h
   switch-window	; takes over C-x o
   company-mode         ; autocompletion support
   company-irony        ; 
   company-c-headers    ;
   company-anaconda     ;
   flycheck             ;
   color-theme	        ; nice looking emacs
   color-theme-tango    ; check out color-theme-solarized
   org-mode
   epresent             ;Emacs Org-Mode Presentations
   multiple-cursors     ;multiple cursors mode
   helm                 ;Better completion browsing
   irony-mode           ;Clang based completion
   auctex               ;Latex Mode
   ;jedi                ;python mode
   multi-term           ;terminal-emulator
   cpputils-cmake
   ))	

;;
;; Some recipes require extra tools to be installed
;;
;; Note: el-get-install requires git, so we know we have at least that.
;;
(when (el-get-executable-find "cvs")
  (add-to-list 'my:el-get-packages 'emacs-goodies-el)) ; the debian addons for emacs

(when (el-get-executable-find "svn")
  (loop for p in '(psvn ; M-x svn-status
yasnippet	; powerful snippet mode
)
do (add-to-list 'my:el-get-packages p)))

(setq my:el-get-packages
      (append
       my:el-get-packages
       (loop for src in el-get-sources collect (el-get-source-name src))))

;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

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

; winner-mode provides C-<left> to get back to previous window layout
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
(helm-mode 1)

;; default key to switch buffer is C-x b, but that's not easy enough
;;
;; when you do that, to kill emacs either close its frame from the window
;; manager or do M-x kill-emacs. Don't need a nice shortcut for a once a
;; week (or day) action.
(global-set-key (kbd "C-x C-b") 'ido-switch-buffer)
(global-set-key (kbd "C-x B") 'ibuffer)

;; C-x C-j opens dired with the cursor right on the file you're editing
(require 'dired-x)

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
   ;(C++ . t)
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

;;EPresent
(global-set-key [f12] 'epresent-run)


;;Yasnippet
(require 'yasnippet)
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

;; Company Irony
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))

(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;;Company Anaconda
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-anaconda))


;;Company C-Headers
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-c-headers))

;;Flycheck syntax checking for emacs
(add-hook 'after-init-hook 'global-flycheck-mode)


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



;;Python
;;(add-hook 'python-mode-hook 'jedi:setup)
;;(setq jedi:complete-on-dot t)  
