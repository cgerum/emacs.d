((adoc-mode status "removed" recipe nil)
 (clang-completion-mode status "installed" recipe
			(:name clang-completion-mode :description "Clang Code-Completion minor mode, for use with C/Objective-C/C++" :type http :url "https://llvm.org/svn/llvm-project/cfe/trunk/utils/clang-completion-mode.el"))
 (el-get status "installed" recipe
	 (:name el-get :website "https://github.com/dimitri/el-get#readme" :description "Manage the external elisp bits and pieces you depend upon." :type github :branch "4.stable" :pkgname "dimitri/el-get" :features el-get :info "." :load "el-get.el"))
 (htmlize status "installed" recipe
	  (:name htmlize :website "http://www.emacswiki.org/emacs/Htmlize" :description "Convert buffer text and decorations to HTML." :type http :url "http://fly.srk.fer.hr/~hniksic/emacs/htmlize.el.cgi" :localname "htmlize.el" :feature htmlize))
 (o-blog status "installed" recipe
	 (:name o-blog :type github :description "Stand alone org-mode blog exporter." :pkgname "renard/o-blog"))
 (sr-speedbar status "installed" recipe
	      (:name sr-speedbar :type emacswiki :description "Same frame speedbar" :post-init
		     (require 'sr-speedbar)))
 (yasnippet status "installed" recipe
	    (:name yasnippet :website "https://github.com/capitaomorte/yasnippet.git" :description "YASnippet is a template system for Emacs." :type github :pkgname "capitaomorte/yasnippet" :features "yasnippet" :pre-init
		   (unless
		       (or
			(boundp 'yas/snippet-dirs)
			(get 'yas/snippet-dirs 'customized-value))
		     (setq yas/snippet-dirs
			   (list
			    (concat el-get-dir
				    (file-name-as-directory "yasnippet")
				    "snippets"))))
		   :post-init
		   (put 'yas/snippet-dirs 'standard-value
			(list
			 (list 'quote
			       (list
				(concat el-get-dir
					(file-name-as-directory "yasnippet")
					"snippets")))))
		   :compile nil :submodule nil)))
