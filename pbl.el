;;; pbl.el --- package-build lite

;; Copyright (C) 2017 Radon Rosborough <radon.neon@gmail.com>
;; Copyright (C) 2011-2013 Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Copyright (C) 2012-2014 Steve Purcell <steve@sanityinc.com>
;; Copyright (C) 2009 Phil Hagelberg <technomancy@gmail.com>

;; Author: Donald Ephraim Curtis <dcurtis@milkbox.net>
;; Created: 2011-09-30
;; Version: 0.1
;; Keywords: tools
;; Package-Requires: ((cl-lib "0.5"))

;; This file is not (yet) part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Please refer to https://github.com/raxod502/pbl for more
;; information.

;;; Code:

(require 'cl-lib)

(defgroup pbl nil
  "Facilities for building package.el-compliant packages from upstream source code."
  :group 'development)

(defcustom pbl-timeout-executable
  (let ((prog (or (executable-find "timeout")
                  (executable-find "gtimeout"))))
    (when (and prog
               (string-match-p "^ *-k" (shell-command-to-string (concat prog " --help"))))
      prog))
  "Path to a GNU coreutils \"timeout\" command if available.
This must be a version which supports the \"-k\" option."
  :group 'pbl
  :type '(file :must-match t))

(defcustom pbl-timeout-secs 600
  "Wait this many seconds for external processes to complete.

If an external process takes longer than
`pbl-timeout-secs' seconds to complete, the process is
terminated.  The `pbl-timeout-secs' variable will only
have an effect if `pbl-timeout-executable' is not nil."
  :group 'pbl
  :type 'number)

;;; Internal Variables

(defconst pbl-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.")

(defun pbl--slurp-file (file-name)
  "Return the contents of FILE-NAME as a string, or nil if no such file exists."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents file-name)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pbl--string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n\r]+$" "" str))

(defun pbl--run-process (dir command &rest args)
  "In DIR (or `default-directory' if unset) run COMMAND with ARGS.
Output is written to the current buffer."
  (let* ((default-directory (file-name-as-directory (or dir default-directory)))
         (timeout (number-to-string pbl-timeout-secs))
         (argv (append
                (unless (eq system-type 'windows-nt)
                  '("env" "LC_ALL=C"))
                (if pbl-timeout-executable
                    (append (list pbl-timeout-executable "-k" "60" timeout command) args)
                  (cons command args)))))
    (unless (file-directory-p default-directory)
      (error "Can't run process in non-existent directory: %s" default-directory))
    (let ((exit-code (apply 'process-file (car argv) nil (current-buffer) t (cdr argv))))
      (or (zerop exit-code)
          (error "Command '%s' exited with non-zero status %d: %s"
                 argv exit-code (buffer-string))))))

(defun pbl--run-process-match (regex dir prog &rest args)
  "Find match for REGEX when - in DIR, or `default-directory' if unset - we run PROG with ARGS."
  (with-temp-buffer
    (apply 'pbl--run-process dir prog args)
    (goto-char (point-min))
    (re-search-forward regex)
    (match-string-no-properties 1)))

(defun pbl-checkout (package-name config working-dir)
  "Check out source for PACKAGE-NAME with CONFIG under WORKING-DIR.
In turn, this function uses the :fetcher option in the CONFIG to
choose a source-specific fetcher function, which it calls with
the same arguments."
  (let ((repo-type (plist-get config :fetcher)))
    (funcall (intern (format "pbl--checkout-%s" (symbol-name repo-type)))
             package-name config (file-name-as-directory working-dir))))

(defvar pbl--last-wiki-fetch-time 0
  "The time at which an emacswiki URL was last requested.
This is used to avoid exceeding the rate limit of 1 request per 2
seconds; the server cuts off after 10 requests in 20 seconds.")

(defvar pbl--wiki-min-request-interval 3
  "The shortest permissible interval between successive requests for Emacswiki URLs.")

(defmacro pbl--with-wiki-rate-limit (&rest body)
  "Rate-limit BODY code passed to this macro to match EmacsWiki's rate limiting."
  (let ((now (cl-gensym))
        (elapsed (cl-gensym)))
    `(let* ((,now (float-time))
            (,elapsed (- ,now pbl--last-wiki-fetch-time)))
       (when (< ,elapsed pbl--wiki-min-request-interval)
         (let ((wait (- pbl--wiki-min-request-interval ,elapsed)))
           (sleep-for wait)))
       (unwind-protect
           (progn ,@body)
         (setq pbl--last-wiki-fetch-time (float-time))))))

(require 'mm-decode)
(defvar url-http-response-status)
(defvar url-http-end-of-headers)

(defun pbl--url-copy-file (url newname &optional ok-if-already-exists)
  "Copy URL to NEWNAME.  Both args must be strings.
Returns the http request's header as a string.
Like `url-copy-file', but it produces an error if the http response is not 200.
Signals a `file-already-exists' error if file NEWNAME already exists,
unless a third argument OK-IF-ALREADY-EXISTS is supplied and non-nil.
A number as third arg means request confirmation if NEWNAME already exists."
  (if (and (file-exists-p newname)
           (not ok-if-already-exists))
      (error "Opening output file: File already exists, %s" newname))
  (let ((buffer (url-retrieve-synchronously url 'silent))
        (headers nil)
        (handle nil))
    (if (not buffer)
        (error "Opening input file: No such file or directory, %s" url))
    (with-current-buffer buffer
      (unless (= 200 url-http-response-status)
        (error "HTTP error %s fetching %s" url-http-response-status url))
      (setq handle (mm-dissect-buffer t))
      (mail-narrow-to-head)
      (setq headers (buffer-string)))
    (let ((message-log-max nil)
          (inhibit-message t))
      (mm-save-part-to-file handle newname))
    (kill-buffer buffer)
    (mm-destroy-parts handle)
    headers))

(defun pbl--grab-wiki-file (filename)
  "Download FILENAME from emacswiki, returning its last-modified time."
  (let* ((download-url
          (format "https://www.emacswiki.org/emacs/download/%s" filename))
         headers)
    (pbl--with-wiki-rate-limit
     (setq headers (pbl--url-copy-file download-url filename t)))
    (when (zerop (nth 7 (file-attributes filename)))
      (error "Wiki file %s was empty - has it been removed?" filename))))

(defun pbl--checkout-wiki (name config dir)
  "Checkout package NAME with config CONFIG from the EmacsWiki into DIR."
  (with-current-buffer (get-buffer-create "*pbl-checkout*")
    (unless (file-exists-p dir)
      (make-directory dir))
    (let ((files (or (plist-get config :files)
                     (list (format "%s.el" name))))
          (default-directory dir))
      (mapcar 'pbl--grab-wiki-file files))))

(defun pbl--darcs-repo (dir)
  "Get the current darcs repo for DIR."
  (pbl--run-process-match "Default Remote: \\(.*\\)" dir "darcs" "show" "repo"))

(defun pbl--checkout-darcs (name config dir)
  "Check package NAME with config CONFIG out of darcs into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (cond
       ((and (file-exists-p (expand-file-name "_darcs" dir))
             (string-equal (pbl--darcs-repo dir) repo))
        (pbl--run-process dir "darcs" "pull" "--all"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--run-process nil "darcs" "get" repo dir))))))

(defun pbl--fossil-repo (dir)
  "Get the current fossil repo for DIR."
  (pbl--run-process-match "\\(.*\\)" dir "fossil" "remote-url"))

(defun pbl--checkout-fossil (name config dir)
  "Check package NAME with config CONFIG out of fossil into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (cond
       ((and (or (file-exists-p (expand-file-name ".fslckout" dir))
                 (file-exists-p (expand-file-name "_FOSSIL_" dir)))
             (string-equal (pbl--fossil-repo dir) repo))
        (pbl--run-process dir "fossil" "update"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (make-directory dir)
        (pbl--run-process dir "fossil" "clone" repo "repo.fossil")
        (pbl--run-process dir "fossil" "open" "repo.fossil"))))))

(defun pbl--svn-repo (dir)
  "Get the current svn repo for DIR."
  (pbl--run-process-match "URL: \\(.*\\)" dir "svn" "info"))

(defun pbl--trim (str &optional chr)
  "Return a copy of STR without any trailing CHR (or space if unspecified)."
  (if (equal (elt str (1- (length str))) (or chr ? ))
      (substring str 0 (1- (length str)))
    str))

(defun pbl--checkout-svn (name config dir)
  "Check package NAME with config CONFIG out of svn into DIR."
  (with-current-buffer (get-buffer-create "*pbl-checkout*")
    (let ((repo (pbl--trim (plist-get config :url) ?/))
          (bound (goto-char (point-max))))
      (cond
       ((and (file-exists-p (expand-file-name ".svn" dir))
             (string-equal (pbl--svn-repo dir) repo))
        (pbl--run-process dir "svn" "up"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--run-process nil "svn" "checkout" repo dir))))))


(defun pbl--cvs-repo (dir)
  "Get the current CVS root and repository for DIR.

Return a cons cell whose `car' is the root and whose `cdr' is the repository."
  (apply 'cons
         (mapcar (lambda (file)
                   (pbl--string-rtrim (pbl--slurp-file (expand-file-name file dir))))
                 '("CVS/Root" "CVS/Repository"))))

(defun pbl--checkout-cvs (name config dir)
  "Check package NAME with config CONFIG out of cvs into DIR."
  (with-current-buffer (get-buffer-create "*pbl-checkout*")
    (let ((root (pbl--trim (plist-get config :url) ?/))
          (repo (or (plist-get config :module) (symbol-name name)))
          (bound (goto-char (point-max)))
          latest)
      (cond
       ((and (file-exists-p (expand-file-name "CVS" dir))
             (equal (pbl--cvs-repo dir) (cons root repo)))
        (pbl--run-process dir "cvs" "update" "-dP"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        ;; CVS insists on relative paths as target directory for checkout (for
        ;; whatever reason), and puts "CVS" directories into every subdirectory
        ;; of the current working directory given in the target path. To get CVS
        ;; to just write to DIR, we need to execute CVS from the parent
        ;; directory of DIR, and specific DIR as relative path.  Hence all the
        ;; following mucking around with paths.  CVS is really horrid.
        (let* ((dir (directory-file-name dir))
               (working-dir (file-name-directory dir))
               (target-dir (file-name-nondirectory dir)))
          (pbl--run-process working-dir "env" "TZ=UTC" "cvs" "-z3" "-d" root "checkout"
                            "-d" target-dir repo)))))))

(defun pbl--git-repo (dir)
  "Get the current git repo for DIR."
  (pbl--run-process-match
   "Fetch URL: \\(.*\\)" dir "git" "remote" "show" "-n" "origin"))

(defun pbl--git-head-branch (dir)
  "Get the current git repo for DIR."
  (or (ignore-errors
        (pbl--run-process-match
         "HEAD branch: \\(.*\\)" dir "git" "remote" "show" "origin"))
      "master"))

(defun pbl--checkout-git (name config dir)
  "Check package NAME with config CONFIG out of git into DIR."
  (let ((repo (plist-get config :url))
        (commit (or (plist-get config :commit)
                    (let ((branch (plist-get config :branch)))
                      (when branch
                        (concat "origin/" branch))))))
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".git" dir))
             (string-equal (pbl--git-repo dir) repo))
        (pbl--run-process dir "git" "fetch" "--all" "--tags"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--run-process nil "git" "clone" repo dir)))
      (pbl--update-git-to-ref dir (or commit (concat "origin/" (pbl--git-head-branch dir)))))))

(defun pbl--update-git-to-ref (dir ref)
  "Update the git repo in DIR so that HEAD is REF."
  (pbl--run-process dir "git" "reset" "--hard" ref)
  (pbl--run-process dir "git" "submodule" "sync" "--recursive")
  (pbl--run-process dir "git" "submodule" "update" "--init" "--recursive"))

(defun pbl--checkout-github (name config dir)
  "Check package NAME with config CONFIG out of github into DIR."
  (let* ((url (format "https://github.com/%s.git" (plist-get config :repo))))
    (pbl--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

(defun pbl--checkout-gitlab (name config dir)
  "Check package NAME with config CONFIG out of gitlab into DIR."
  (let* ((url (format "https://gitlab.com/%s.git" (plist-get config :repo))))
    (pbl--checkout-git name (plist-put (copy-sequence config) :url url) dir)))

(defun pbl--checkout-bitbucket (name config dir)
  "Check package NAME with config CONFIG out of bitbucket into DIR."
  (let* ((url (format "https://bitbucket.com/%s" (plist-get config :repo))))
    (pbl--checkout-hg name (plist-put (copy-sequence config) :url url) dir)))

(defun pbl--bzr-expand-repo (repo)
  "Get REPO expanded name."
  (pbl--run-process-match "\\(?:branch root\\|repository branch\\): \\(.*\\)" nil "bzr" "info" repo))

(defun pbl--bzr-repo (dir)
  "Get the current bzr repo for DIR."
  (pbl--run-process-match "parent branch: \\(.*\\)" dir "bzr" "info"))

(defun pbl--checkout-bzr (name config dir)
  "Check package NAME with config CONFIG out of bzr into DIR."
  (let ((repo (pbl--bzr-expand-repo (plist-get config :url))))
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".bzr" dir))
             (string-equal (pbl--bzr-repo dir) repo))
        (pbl--run-process dir "bzr" "merge" "--force"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--run-process nil "bzr" "branch" repo dir))))))

(defun pbl--hg-repo (dir)
  "Get the current hg repo for DIR."
  (pbl--run-process-match "default = \\(.*\\)" dir "hg" "paths"))

(defun pbl--checkout-hg (name config dir)
  "Check package NAME with config CONFIG out of hg into DIR."
  (let ((repo (plist-get config :url)))
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (goto-char (point-max))
      (cond
       ((and (file-exists-p (expand-file-name ".hg" dir))
             (string-equal (pbl--hg-repo dir) repo))
        (pbl--run-process dir "hg" "pull")
        (pbl--run-process dir "hg" "update"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--run-process nil "hg" "clone" repo dir))))))

(defun pbl-expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs lst)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (pbl-expand-file-specs dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (pbl-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             (let ((destname)))
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.in\\'"
                                            ""
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))


(defun pbl--config-file-list (config)
  "Get the :files spec from CONFIG, or return `pbl-default-files-spec'."
  (let ((file-list (plist-get config :files)))
    (cond
     ((null file-list)
      pbl-default-files-spec)
     ((eq :defaults (car file-list))
      (append pbl-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun pbl--expand-source-file-list (dir config)
  "Shorthand way to expand paths in DIR for source files listed in CONFIG."
  (mapcar 'car (pbl-expand-file-specs dir (pbl--config-file-list config))))

(defun pbl--generate-info-files (files source-dir target-dir)
  "Create .info files from any .texi files listed in FILES.

The source and destination file paths are expanded in SOURCE-DIR
and TARGET-DIR respectively.

Any of the original .texi(nfo) files found in TARGET-DIR are
deleted."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (source-path (expand-file-name source-file source-dir))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (string-match ".texi\\(nfo\\)?$" source-file)
        (when (not (file-exists-p info-path))
          (with-current-buffer (get-buffer-create "*pbl-info*")
            (ignore-errors
              (pbl--run-process
               (file-name-directory source-path)
               "makeinfo"
               source-path
               "-o"
               info-path))))
        (delete-file (expand-file-name dest-file target-dir))))))

(defun pbl--generate-dir-file (files target-dir)
  "Create dir file from any .info files listed in FILES in TARGET-DIR."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (and (or (string-match ".info$" source-file)
                     (string-match ".texi\\(nfo\\)?$" source-file))
                 (file-exists-p info-path))
        (with-current-buffer (get-buffer-create "*pbl-info*")
          (ignore-errors
            (pbl--run-process
             nil
             "install-info"
             (concat "--dir=" (expand-file-name "dir" target-dir))
             info-path)))))))

(provide 'pbl)

;; Local Variables:
;; coding: utf-8
;; End:

;;; pbl.el ends here
