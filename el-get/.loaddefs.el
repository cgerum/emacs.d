;;; .loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (el-get-checksum el-get-make-recipes el-get-cd
;;;;;;  el-get-self-update el-get-update-all el-get-version) "el-get"
;;;;;;  "el-get/el-get.el" (20599 19552))
;;; Generated autoloads from el-get/el-get.el

(autoload 'el-get-version "el-get" "\
Message the current el-get version

\(fn)" t nil)

(autoload 'el-get-update-all "el-get" "\
Performs update of all installed packages.

\(fn &optional NO-PROMPT)" t nil)

(autoload 'el-get-self-update "el-get" "\
Update el-get itself.  The standard recipe takes care of reloading the code.

\(fn)" t nil)

(autoload 'el-get-cd "el-get" "\
Open dired in the package directory.

\(fn PACKAGE)" t nil)

(autoload 'el-get-make-recipes "el-get" "\
Loop over `el-get-sources' and write a recipe file for each
entry which is not a symbol and is not already a known recipe.

\(fn &optional DIR)" t nil)

(autoload 'el-get-checksum "el-get" "\
Compute the checksum of the given package, and put it in the kill-ring

\(fn PACKAGE &optional PACKAGE-STATUS-ALIST)" t nil)

;;;***

;;;### (autoloads (el-get-list-packages) "el-get-list-packages" "el-get/el-get-list-packages.el"
;;;;;;  (20599 19550))
;;; Generated autoloads from el-get/el-get-list-packages.el

(autoload 'el-get-list-packages "el-get-list-packages" "\
Display a list of packages.

\(fn)" t nil)

;;;***

;;;### (autoloads (epresent-run) "epresent/epresent" "epresent/epresent.el"
;;;;;;  (20662 20553))
;;; Generated autoloads from epresent/epresent.el

(autoload 'epresent-run "epresent/epresent" "\
Present an Org-mode buffer.

\(fn)" t nil)
(global-set-key [f12] 'epresent-run)

;;;***

;;;### (autoloads (htmlize-many-files-dired htmlize-many-files htmlize-file
;;;;;;  htmlize-region htmlize-buffer) "htmlize/htmlize" "htmlize/htmlize.el"
;;;;;;  (20599 21589))
;;; Generated autoloads from htmlize/htmlize.el

(autoload 'htmlize-buffer "htmlize/htmlize" "\
Convert BUFFER to HTML, preserving colors and decorations.

The generated HTML is available in a new buffer, which is returned.
When invoked interactively, the new buffer is selected in the current
window.  The title of the generated document will be set to the buffer's
file name or, if that's not available, to the buffer's name.

Note that htmlize doesn't fontify your buffers, it only uses the
decorations that are already present.  If you don't set up font-lock or
something else to fontify your buffers, the resulting HTML will be
plain.  Likewise, if you don't like the choice of colors, fix the mode
that created them, or simply alter the faces it uses.

\(fn &optional BUFFER)" t nil)

(autoload 'htmlize-region "htmlize/htmlize" "\
Convert the region to HTML, preserving colors and decorations.
See `htmlize-buffer' for details.

\(fn BEG END)" t nil)

(autoload 'htmlize-file "htmlize/htmlize" "\
Load FILE, fontify it, convert it to HTML, and save the result.

Contents of FILE are inserted into a temporary buffer, whose major mode
is set with `normal-mode' as appropriate for the file type.  The buffer
is subsequently fontified with `font-lock' and converted to HTML.  Note
that, unlike `htmlize-buffer', this function explicitly turns on
font-lock.  If a form of highlighting other than font-lock is desired,
please use `htmlize-buffer' directly on buffers so highlighted.

Buffers currently visiting FILE are unaffected by this function.  The
function does not change current buffer or move the point.

If TARGET is specified and names a directory, the resulting file will be
saved there instead of to FILE's directory.  If TARGET is specified and
does not name a directory, it will be used as output file name.

\(fn FILE &optional TARGET)" t nil)

(autoload 'htmlize-many-files "htmlize/htmlize" "\
Convert FILES to HTML and save the corresponding HTML versions.

FILES should be a list of file names to convert.  This function calls
`htmlize-file' on each file; see that function for details.  When
invoked interactively, you are prompted for a list of files to convert,
terminated with RET.

If TARGET-DIRECTORY is specified, the HTML files will be saved to that
directory.  Normally, each HTML file is saved to the directory of the
corresponding source file.

\(fn FILES &optional TARGET-DIRECTORY)" t nil)

(autoload 'htmlize-many-files-dired "htmlize/htmlize" "\
HTMLize dired-marked files.

\(fn ARG &optional TARGET-DIRECTORY)" t nil)

;;;***

;;;### (autoloads (org-publish-blog-sync org-publish-blog o-blog-version)
;;;;;;  "o-blog/o-blog" "o-blog/o-blog.el" (20599 20731))
;;; Generated autoloads from o-blog/o-blog.el

(autoload 'o-blog-version "o-blog/o-blog" "\
Message the current o-blog version. If call using
`universal-argument', insert value in current position.

\(fn &optional HERE)" t nil)

(autoload 'org-publish-blog "o-blog/o-blog" "\
Publish FILE as a blog synchronously execpt ib ASYNC is
defined, or interactivelly called with `prefix-arg'.

\(fn &optional FILE ASYNC)" t nil)

(autoload 'org-publish-blog-sync "o-blog/o-blog" "\
Publish FILE synchronously.

\(fn FILE)" nil nil)

;;;***

;;;### (autoloads (o-blog-publish-alert) "o-blog/o-blog-alert" "o-blog/o-blog-alert.el"
;;;;;;  (20599 20731))
;;; Generated autoloads from o-blog/o-blog-alert.el

(autoload 'o-blog-publish-alert "o-blog/o-blog-alert" "\
Publish an alert in HTML mode.

Admonitions are specially marked topics that can appear
anywhere an ordinary body element can. They contain arbitrary
body elements. Typically, an alert is rendered as an offset
block in a document, sometimes outlined or shaded, with a title
matching the alert type. For example:

#+BEGIN_O_BLOG_ALERT type title
Some text inside the alert
#+END_O_BLOG_ALERT

Where type can be on of:

  - info
  - success
  - warning
  - error

This directive might be rendered something like this:

+----------------------------+
| Title                      |
|                            |
| Some text inside the alert |
+----------------------------+

In an HTML context, previous directive would be expanded as:

#+BEGIN_HTML
<div class=\"alert alert-type\">
<p class=\"alert-heading\">Title</p>
#+END_HTML
Some text inside the alert
#+BEGIN_HTML
<div>
#+END_HTML

The default replacement text could be changed using variables
`o-blog-alert-header', `o-blog-alert-footer' and
`o-blog-alert-title'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (o-blog-publish-grid) "o-blog/o-blog-grid" "o-blog/o-blog-grid.el"
;;;;;;  (20599 20731))
;;; Generated autoloads from o-blog/o-blog-grid.el

(autoload 'o-blog-publish-grid "o-blog/o-blog-grid" "\
Publish a grid in HTML mode.

The default replacement text could be changed using variables
`o-blog-grid-header', `o-blog-grid-footer' and
`o-blog-grid-title'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (o-blog-publish-source) "o-blog/o-blog-source"
;;;;;;  "o-blog/o-blog-source.el" (20599 20731))
;;; Generated autoloads from o-blog/o-blog-source.el

(autoload 'o-blog-publish-source "o-blog/o-blog-source" "\
Publish an sourced file in HTML mode.

A source file is defined using:

    #+O_BLOG_SOURCE: path/to/file [mode]

and is converted to 

    #+BEGIN_HTML
    <div class=\"o-blog-source\">
    <div class=\"title\">file</div>
    <div style=\"display:none;\" class=\"content\">
    #+END_HTML
    #+BEGIN_SRC mode
    (file content)
    #+END_SRC
    #+BEGIN_HTML
    </div></div>
    #+END_HTML

The default replacement text could be changed using variables
`o-blog-source-header' and `o-blog-source-footer'.

\(fn)" nil nil)

;;;***

;;;### (autoloads (yas-global-mode yas-minor-mode) "yasnippet/yasnippet"
;;;;;;  "yasnippet/yasnippet.el" (20599 21846))
;;; Generated autoloads from yasnippet/yasnippet.el

(autoload 'yas-minor-mode "yasnippet/yasnippet" "\
Toggle YASnippet mode.

When YASnippet mode is enabled, the `yas-trigger-key' key expands
snippets of code depending on the major mode.

With no argument, this command toggles the mode.
positive prefix argument turns on the mode.
Negative prefix argument turns off the mode.

You can customize the key through `yas-trigger-key'.

Key bindings:
\\{yas-minor-mode-map}

\(fn &optional ARG)" t nil)

(defvar yas-global-mode nil "\
Non-nil if Yas-Global mode is enabled.
See the command `yas-global-mode' for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `yas-global-mode'.")

(custom-autoload 'yas-global-mode "yasnippet/yasnippet" nil)

(autoload 'yas-global-mode "yasnippet/yasnippet" "\
Toggle Yas minor mode in all buffers.
With prefix ARG, enable Yas-Global mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Yas minor mode is enabled in all buffers where
`yas-minor-mode-on' would do it.
See `yas-minor-mode' for more information on Yas minor mode.

\(fn &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("clang-completion-mode/clang-completion-mode.el"
;;;;;;  "el-get/el-get-autoloads.el" "el-get/el-get-build.el" "el-get/el-get-byte-compile.el"
;;;;;;  "el-get/el-get-core.el" "el-get/el-get-custom.el" "el-get/el-get-dependencies.el"
;;;;;;  "el-get/el-get-install.el" "el-get/el-get-methods.el" "el-get/el-get-notify.el"
;;;;;;  "el-get/el-get-recipes.el" "el-get/el-get-status.el" "o-blog/o-blog-bootstrap.el"
;;;;;;  "o-blog/o-blog-i18n.el" "o-blog/o-blog-pkg.el" "o-blog/sample-init.el"
;;;;;;  "sr-speedbar/sr-speedbar.el" "yasnippet/dropdown-list.el"
;;;;;;  "yasnippet/yasnippet-debug.el" "yasnippet/yasnippet-tests.el")
;;;;;;  (20662 20554 967658))

;;;***

(provide '.loaddefs)
;; Local Variables:
;; version-control: never
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; .loaddefs.el ends here
