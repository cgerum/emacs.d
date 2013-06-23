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

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil t)
  (url-retrieve
   "https://github.com/dimitri/el-get/raw/master/el-get-install.el"))

;; now set our own packages
(setq
 my:el-get-packages
 '(el-get	; el-get is self-hosting
   org-mode
   epresent             ;Emacs Org-Mode Presentations
   ))	


;; install new packages and init already installed packages
(el-get 'sync my:el-get-packages)

;; on to the visual settings
(setq inhibit-splash-screen t)	; no splash screen, thanks
(column-number-mode 1)	; column numbers in the mode line

(tool-bar-mode -1)	; no tool bar with icons


;; avoid compiz manager rendering bugs
(add-to-list 'default-frame-alist '(alpha . 100))



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
   (sh . t)
   )
 )

(setq org-log-done 'time)

;;EPresent
(global-set-key [f12] 'epresent-run)

