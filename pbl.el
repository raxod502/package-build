;;; pbl.el --- Tools for assembling a package archive

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

;; This file allows a curator to publish an archive of Emacs packages.

;; The archive is generated from a set of recipes which describe elisp
;; projects and repositories from which to get them.  The term
;; "package" here is used to mean a specific version of a project that
;; is prepared for download and installation.

;;; Code:

(require 'cl-lib)

(require 'package)
(require 'lisp-mnt)
(require 'json)

(defconst pbl--melpa-base
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name (buffer-file-name))))))

(defgroup pbl nil
  "Facilities for building package.el-compliant packages from upstream source code."
  :group 'development)

(defcustom pbl-working-dir (expand-file-name "working/" pbl--melpa-base)
  "Directory in which to keep checkouts."
  :group 'pbl
  :type 'string)

(defcustom pbl-archive-dir (expand-file-name "packages/" pbl--melpa-base)
  "Directory in which to keep compiled archives."
  :group 'pbl
  :type 'string)

(defcustom pbl-recipes-dir (expand-file-name "recipes/" pbl--melpa-base)
  "Directory containing recipe files."
  :group 'pbl
  :type 'string)

(defcustom pbl-verbose t
  "When non-nil, `pbl' feels free to print information about its progress."
  :group 'pbl
  :type 'boolean)

(defcustom pbl-stable nil
  "When non-nil, `pbl' tries to build packages from versions-tagged code."
  :group 'pbl
  :type 'boolean)

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

(defcustom pbl-tar-executable
  (or (executable-find "gtar")
      (executable-find "tar"))
  "Path to a (preferably GNU) tar command.
Certain package names (e.g. \"@\") may not work properly with a BSD tar."
  :group 'pbl
  :type '(file :must-match t))

(defcustom pbl-write-melpa-badge-images nil
  "When non-nil, write MELPA badge images alongside packages, for use on GitHub pages etc."
  :group 'pbl
  :type 'boolean)

(defcustom pbl-version-regexp "^[rRvV]?\\(.*\\)$"
  "Default pattern for matching valid version-strings within repository tags.
The string in the capture group should be parsed as valid by `version-to-list'."
  :group 'pbl
  :type 'string)

;;; Internal Variables

(defvar pbl--recipe-alist nil
  "Internal list of package build specs.

Do not use this directly.  Use `pbl-recipe-alist'
function.")

(defvar pbl--recipe-alist-initialized nil
  "Determines if `pbl--recipe-alist` has been initialized.")

(defvar pbl--archive-alist nil
  "Internal list of already-built packages, in the standard package.el format.

Do not use this directly.  Use `pbl-archive-alist'
function for access to this function")

(defvar pbl--archive-alist-initialized nil
  "Determines if pbl--archive-alist has been initialized.")

(defconst pbl-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.")

(defun pbl--message (format-string &rest args)
  "Log a message using FORMAT-STRING and ARGS as per `message'."
  (when pbl-verbose
    (apply 'message format-string args)))

(defun pbl--slurp-file (file-name)
  "Return the contents of FILE-NAME as a string, or nil if no such file exists."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents file-name)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defun pbl--string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n\r]+$" "" str))


(defun pbl--valid-version (str &optional regexp)
  "Apply to STR the REGEXP if defined, \
then pass the string to `version-to-list' and return the result, \
or nil if the version cannot be parsed."
  (when (and regexp (string-match regexp str))
    (setq str (match-string 1 str)))
  (ignore-errors (version-to-list str)))

(defun pbl--parse-time (str)
  "Parse STR as a time, and format as a YYYYMMDD.HHMM string."
  ;; We remove zero-padding the HH portion, as it is lost
  ;; when stored in the archive-contents
  (let* ((s (substring-no-properties str))
         (time (date-to-time
                (if (string-match "^\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\) \\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)$" s)
                    (concat (match-string 1 s) "-" (match-string 2 s) "-"
                            (match-string 3 s) " " (match-string 4 s))
                  s))))
    (concat (format-time-string "%Y%m%d." time)
            (format "%d" (string-to-number (format-time-string "%H%M" time))))))

(defun pbl--find-parse-time (regex &optional bound)
  "Find REGEX in current buffer and format as a time-based version string, \
optionally looking only as far back as BOUND."
  (pbl--parse-time (and (re-search-backward regex bound t)
                                  (match-string-no-properties 1))))

(defun pbl--find-parse-time-newest (regex &optional bound)
  "Find REGEX in current buffer and format as a time-based version string, \
optionally looking only as far back as BOUND."
  (save-match-data
    (let (cur matches)
      (while (setq cur (ignore-errors (pbl--find-parse-time regex bound)))
        (push cur matches))
      (car (nreverse (sort matches 'string<))))))

(defun pbl--find-version-newest (regex &optional bound)
  "Find the newest version matching REGEX before point, optionally stopping at BOUND."
  (let ((tags (split-string
               (buffer-substring-no-properties
                (or bound (point-min)) (point))
               "\n")))
    (setq tags (append
                (mapcar
                 ;; Because the default `version-separator' is ".",
                 ;; version-strings like "1_4_5" will be parsed
                 ;; wrongly as (1 -4 4 -4 5), so we set
                 ;; `version-separator' to "_" below and run again.
                 (lambda (tag)
                   (when (pbl--valid-version tag regex)
                     (list (pbl--valid-version tag regex) tag)))
                 tags)
                (mapcar
                 ;; Check for valid versions again, this time using
                 ;; "_" as a separator instead of "." to catch
                 ;; version-strings like "1_4_5".  Since "_" is
                 ;; otherwise treated as a snapshot separator by
                 ;; `version-regexp-alist', we don't have to worry
                 ;; about the incorrect version list above—(1 -4 4 -4
                 ;; 5)—since it will always be treated as older by
                 ;; `version-list-<'.
                 (lambda (tag)
                   (let ((version-separator "_"))
                     (when (pbl--valid-version tag regex)
                       (list (pbl--valid-version tag regex) tag))))
                 tags)))
    (setq tags (cl-remove-if nil tags))
    ;; Returns a list like ((0 1) ("v0.1")); the first element is used
    ;; for comparison and for `package-version-join', and the second
    ;; (the original tag) is used by git/hg/etc.
    (car (nreverse (sort tags (lambda (v1 v2) (version-list-< (car v1) (car v2))))))))

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
the same arguments.

Returns the package version as a string."
  (let ((repo-type (plist-get config :fetcher)))
    (pbl--message "Fetcher: %s" (symbol-name repo-type))
    (unless (eq 'wiki repo-type)
      (pbl--message "Source: %s\n" (or (plist-get config :repo) (plist-get config :url))))
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
           (pbl--message "Waiting %.2f secs before hitting Emacswiki again" wait)
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
  (let ((buffer (url-retrieve-synchronously url))
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
    (mm-save-part-to-file handle newname)
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
      (error "Wiki file %s was empty - has it been removed?" filename))
    (pbl--parse-time
     (with-temp-buffer
       (insert headers)
       (mail-fetch-field "last-modified")))))

(defun pbl--checkout-wiki (name config dir)
  "Checkout package NAME with config CONFIG from the EmacsWiki into DIR."
  (unless pbl-stable
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (unless (file-exists-p dir)
        (make-directory dir))
      (let ((files (or (plist-get config :files)
                       (list (format "%s.el" name))))
            (default-directory dir))
        (car (nreverse (sort (mapcar 'pbl--grab-wiki-file files) 'string-lessp)))))))

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
        (pbl--princ-exists dir)
        (pbl--run-process dir "darcs" "pull" "--all"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--princ-checkout repo dir)
        (pbl--run-process nil "darcs" "get" repo dir)))
      (if pbl-stable
          (let* ( (bound (goto-char (point-max)))
                  (regexp (or (plist-get config :version-regexp)
                              pbl-version-regexp))
                  (tag-version (and (pbl--run-process dir "darcs" "show" "tags")
                                    (or (pbl--find-version-newest regexp bound)
                                        (error "No valid stable versions found for %s" name)))) )
            (pbl--run-process dir "darcs" "obliterate" "--all" "--from-tag" (cadr tag-version))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (apply 'pbl--run-process dir "darcs" "changes" "--max-count" "1"
               (pbl--expand-source-file-list dir config))
        (pbl--find-parse-time
         "\\([a-zA-Z]\\{3\\} [a-zA-Z]\\{3\\} \\( \\|[0-9]\\)[0-9] [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\} [A-Za-z]\\{3\\} [0-9]\\{4\\}\\)")))))

(defun pbl--fossil-repo (dir)
  "Get the current fossil repo for DIR."
  (pbl--run-process-match "\\(.*\\)" dir "fossil" "remote-url"))

(defun pbl--checkout-fossil (name config dir)
  "Check package NAME with config CONFIG out of fossil into DIR."
  (unless pbl-stable
    (let ((repo (plist-get config :url)))
      (with-current-buffer (get-buffer-create "*pbl-checkout*")
        (cond
         ((and (or (file-exists-p (expand-file-name ".fslckout" dir))
                   (file-exists-p (expand-file-name "_FOSSIL_" dir)))
               (string-equal (pbl--fossil-repo dir) repo))
          (pbl--princ-exists dir)
          (pbl--run-process dir "fossil" "update"))
         (t
          (when (file-exists-p dir)
            (delete-directory dir t))
          (pbl--princ-checkout repo dir)
          (make-directory dir)
          (pbl--run-process dir "fossil" "clone" repo "repo.fossil")
          (pbl--run-process dir "fossil" "open" "repo.fossil")))
        (pbl--run-process dir "fossil" "timeline" "-n" "1" "-t" "ci")
        (or (pbl--find-parse-time
             "=== \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} ===\n[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\) ")
            (error "No valid timestamps found!"))))))

(defun pbl--svn-repo (dir)
  "Get the current svn repo for DIR."
  (pbl--run-process-match "URL: \\(.*\\)" dir "svn" "info"))

(defun pbl--trim (str &optional chr)
  "Return a copy of STR without any trailing CHR (or space if unspecified)."
  (if (equal (elt str (1- (length str))) (or chr ? ))
      (substring str 0 (1- (length str)))
    str))

(defun pbl--princ-exists (dir)
  "Print a message that the contents of DIR will be updated."
  (pbl--message "Updating %s" dir))

(defun pbl--princ-checkout (repo dir)
  "Print a message that REPO will be checked out into DIR."
  (pbl--message "Cloning %s to %s" repo dir))

(defun pbl--checkout-svn (name config dir)
  "Check package NAME with config CONFIG out of svn into DIR."
  (unless pbl-stable
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (let ((repo (pbl--trim (plist-get config :url) ?/))
            (bound (goto-char (point-max))))
        (cond
         ((and (file-exists-p (expand-file-name ".svn" dir))
               (string-equal (pbl--svn-repo dir) repo))
          (pbl--princ-exists dir)
          (pbl--run-process dir "svn" "up"))
         (t
          (when (file-exists-p dir)
            (delete-directory dir t))
          (pbl--princ-checkout repo dir)
          (pbl--run-process nil "svn" "checkout" repo dir)))
        (apply 'pbl--run-process dir "svn" "info"
               (pbl--expand-source-file-list dir config))
        (or (pbl--find-parse-time-newest "Last Changed Date: \\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)" bound)
            (error "No valid timestamps found!"))))))


(defun pbl--cvs-repo (dir)
  "Get the current CVS root and repository for DIR.

Return a cons cell whose `car' is the root and whose `cdr' is the repository."
  (apply 'cons
         (mapcar (lambda (file)
                   (pbl--string-rtrim (pbl--slurp-file (expand-file-name file dir))))
                 '("CVS/Root" "CVS/Repository"))))

(defun pbl--checkout-cvs (name config dir)
  "Check package NAME with config CONFIG out of cvs into DIR."
  (unless pbl-stable
    (with-current-buffer (get-buffer-create "*pbl-checkout*")
      (let ((root (pbl--trim (plist-get config :url) ?/))
            (repo (or (plist-get config :module) (symbol-name name)))
            (bound (goto-char (point-max)))
            latest)
        (cond
         ((and (file-exists-p (expand-file-name "CVS" dir))
               (equal (pbl--cvs-repo dir) (cons root repo)))
          (pbl--princ-exists dir)
          (pbl--run-process dir "cvs" "update" "-dP"))
         (t
          (when (file-exists-p dir)
            (delete-directory dir t))
          (pbl--princ-checkout (format "%s from %s" repo root) dir)
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
                                        "-d" target-dir repo))))
        (apply 'pbl--run-process dir "cvs" "log"
               (pbl--expand-source-file-list dir config))

        ;; `cvs log` does not provide a way to view the previous N
        ;; revisions, so instead of parsing the entire log we examine
        ;; the Entries file, which looks like this:
        ;;
        ;; /.cvsignore/1.2/Thu Sep  1 12:42:02 2005//
        ;; /CHANGES/1.1/Tue Oct  4 11:47:54 2005//
        ;; /GNUmakefile/1.8/Tue Oct  4 11:47:54 2005//
        ;; /Makefile/1.14/Tue Oct  4 11:47:54 2005//
        ;;
        (insert-file-contents (concat dir "/CVS/Entries"))
        (setq latest (car (sort
                           (split-string (buffer-substring-no-properties (point) (point-max)) "\n")
                           (lambda (x y)
                             (when (string-match "^\\/[^\\/]*\\/[^\\/]*\\/\\([^\\/]*\\)\\/\\/$" x)
                               (setq x (pbl--parse-time (match-string 1 x))))
                             (when (string-match "^\\/[^\\/]*\\/[^\\/]*\\/\\([^\\/]*\\)\\/\\/$" y)
                               (setq y (pbl--parse-time (match-string 1 y))))
                             (version-list-<= (pbl--valid-version y)
                                              (pbl--valid-version x))))))
        (when (string-match "^\\/[^\\/]*\\/[^\\/]*\\/\\([^\\/]*\\)\\/\\/$" latest)
          (setq latest (match-string 1 latest)))
        (or (pbl--parse-time latest)
            (error "No valid timestamps found!"))))))

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
        (pbl--princ-exists dir)
        (pbl--run-process dir "git" "fetch" "--all" "--tags"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--princ-checkout repo dir)
        (pbl--run-process nil "git" "clone" repo dir)))
      (if pbl-stable
          (let* ((bound (goto-char (point-max)))
                 (regexp (or (plist-get config :version-regexp)
                             pbl-version-regexp))
                 (tag-version (and (pbl--run-process dir "git" "tag")
                                   (or (pbl--find-version-newest regexp bound)
                                       (error "No valid stable versions found for %s" name)))) )

            ;; Using reset --hard here to comply with what's used for
            ;; unstable, but maybe this should be a checkout?
            (pbl--update-git-to-ref dir (concat "tags/" (cadr tag-version)))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (pbl--update-git-to-ref dir (or commit (concat "origin/" (pbl--git-head-branch dir))))
        (apply 'pbl--run-process dir "git" "log" "--first-parent" "-n1" "--pretty=format:'\%ci'"
               (pbl--expand-source-file-list dir config))
        (pbl--find-parse-time
         "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

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
        (pbl--princ-exists dir)
        (pbl--run-process dir "bzr" "merge" "--force"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--princ-checkout repo dir)
        (pbl--run-process nil "bzr" "branch" repo dir)))
      (if pbl-stable
          (let ( (bound (goto-char (point-max)))
                 (regexp (or (plist-get config :version-regexp)
                             pbl-version-regexp))
                 tag-version )
            (pbl--run-process dir "bzr" "tags")
            (goto-char bound)
            (ignore-errors (while (re-search-forward "\\ +.*")
                             (replace-match "")))
            (setq tag-version (or (pbl--find-version-newest regexp bound)
                                  (error "No valid stable versions found for %s" name)))

            (pbl--run-process dir "bzr" "revert" "-r" (concat "tag:" (cadr tag-version)))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (apply 'pbl--run-process dir "bzr" "log" "-l1"
               (pbl--expand-source-file-list dir config))
        (pbl--find-parse-time
         "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

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
        (pbl--princ-exists dir)
        (pbl--run-process dir "hg" "pull")
        (pbl--run-process dir "hg" "update"))
       (t
        (when (file-exists-p dir)
          (delete-directory dir t))
        (pbl--princ-checkout repo dir)
        (pbl--run-process nil "hg" "clone" repo dir)))
      (if pbl-stable
          (let ( (bound (goto-char (point-max)))
                 (regexp (or (plist-get config :version-regexp)
                             pbl-version-regexp))
                 tag-version )
            (pbl--run-process dir "hg" "tags")
            ;; The output of `hg tags` shows the ref of the tag as well
            ;; as the tag itself, e.g.:
            ;;
            ;; tip                             1696:73ad80e8fea1
            ;; 1.2.8                           1691:464af57fd2b7
            ;;
            ;; So here we remove that second column before passing the
            ;; buffer contents to `pbl--find-version-newest'.
            ;; This isn't strictly necessary for Mercurial since the
            ;; colon in "1691:464af57fd2b7" means that won't be parsed
            ;; as a valid version-string, but it's an example of how to
            ;; do it in case it's necessary elsewhere.
            (goto-char bound)
            (ignore-errors (while (re-search-forward "\\ +.*")
                             (replace-match "")))
            (setq tag-version (or (pbl--find-version-newest regexp bound)
                                  (error "No valid stable versions found for %s" name)))

            (pbl--run-process dir "hg" "update" (cadr tag-version))
            ;; Return the parsed version as a string
            (package-version-join (car tag-version)))
        (apply 'pbl--run-process dir "hg" "log" "--style" "compact" "-l1"
               (pbl--expand-source-file-list dir config))
        (pbl--find-parse-time
         "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} [0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)")))))

(defun pbl--dump (data file &optional pretty-print)
  "Write DATA to FILE as a Lisp sexp.
Optionally PRETTY-PRINT the data."
  (with-temp-file file
    (pbl--message "File: %s" file)
    (if pretty-print
        (pp data (current-buffer))
      (print data (current-buffer)))))

(defun pbl--write-pkg-file (pkg-file pkg-info)
  "Write PKG-FILE containing PKG-INFO."
  (with-temp-file pkg-file
    (pp
     `(define-package
        ,(aref pkg-info 0)
        ,(aref pkg-info 3)
        ,(aref pkg-info 2)
        ',(mapcar
           (lambda (elt)
             (list (car elt)
                   (package-version-join (cadr elt))))
           (aref pkg-info 1))
        ;; Append our extra information
        ,@(cl-mapcan (lambda (entry)
                       (let ((value (cdr entry)))
                         (when (or (symbolp value) (listp value))
                           ;; We must quote lists and symbols,
                           ;; because Emacs 24.3 and earlier evaluate
                           ;; the package information, which would
                           ;; break for unquoted symbols or lists
                           (setq value (list 'quote value)))
                         (list (car entry) value)))
                     (when (> (length pkg-info) 4)
                       (aref pkg-info 4))))
     (current-buffer))
    (princ ";; Local Variables:\n;; no-byte-compile: t\n;; End:\n" (current-buffer))))

(defun pbl--read-from-file (file-name)
  "Read and return the Lisp data stored in FILE-NAME, or nil if no such file exists."
  (when (file-exists-p file-name)
    (car (read-from-string (pbl--slurp-file file-name)))))

(defun pbl--create-tar (file dir &optional files)
  "Create a tar FILE containing the contents of DIR, or just FILES if non-nil."
  (when (eq system-type 'windows-nt)
    (setq file (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" file)))
  (apply 'process-file
         pbl-tar-executable nil
         (get-buffer-create "*pbl-checkout*")
         nil "-cvf"
         file
         "--exclude=.svn"
         "--exclude=CVS"
         "--exclude=.git"
         "--exclude=_darcs"
         "--exclude=.fslckout"
         "--exclude=_FOSSIL_"
         "--exclude=.bzr"
         "--exclude=.hg"
         (or (mapcar (lambda (fn) (concat dir "/" fn)) files) (list dir))))


(defun pbl--find-package-commentary (file-path)
  "Get commentary section from FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (lm-commentary))))

(defun pbl--write-pkg-readme (target-dir commentary file-name)
  "In TARGET-DIR, write COMMENTARY to a -readme.txt file prefixed with FILE-NAME."
  (when commentary
    (with-temp-buffer
      (insert commentary)
      ;; Adapted from `describe-package-1'.
      (goto-char (point-min))
      (save-excursion
        (when (re-search-forward "^;;; Commentary:\n" nil t)
          (replace-match ""))
        (while (re-search-forward "^\\(;+ ?\\)" nil t)
          (replace-match ""))
        (goto-char (point-min))
        (when (re-search-forward "\\`\\( *\n\\)+" nil t)
          (replace-match "")))
      (delete-trailing-whitespace)
      (let ((coding-system-for-write buffer-file-coding-system))
        (write-region nil nil
                      (pbl--readme-file-name target-dir file-name))))))

(defun pbl--readme-file-name (target-dir file-name)
  "Name of the readme file in TARGET-DIR for the package FILE-NAME."
  (expand-file-name (concat file-name "-readme.txt")
                    target-dir))

(defun pbl--update-or-insert-version (version)
  "Ensure current buffer has a \"Package-Version: VERSION\" header."
  (goto-char (point-min))
  (if (let ((case-fold-search t))
        (re-search-forward "^;+* *Package-Version *: *" nil t))
      (progn
        (move-beginning-of-line nil)
        (search-forward "V" nil t)
        (backward-char)
        (insert "X-Original-")
        (move-beginning-of-line nil))
    ;; Put the new header in a sensible place if we can
    (re-search-forward "^;+* *\\(Version\\|Package-Requires\\|Keywords\\|URL\\) *:" nil t)
    (forward-line))
  (insert (format ";; Package-Version: %s" version))
  (newline))

(defun pbl--ensure-ends-here-line (file-path)
  "Add a 'FILE-PATH ends here' trailing line if missing."
  (save-excursion
    (goto-char (point-min))
    (let* ((fname (file-name-nondirectory file-path))
           (trailer (concat ";;; " fname " ends here")))
      (unless (search-forward trailer nil t)
        (goto-char (point-max))
        (newline)
        (insert trailer)
        (newline)))))

(defun pbl--get-package-info (file-path)
  "Get a vector of package info from the docstrings in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (with-temp-buffer
        (insert-file-contents file-path)
        ;; next few lines are a hack for some packages that aren't
        ;; commented properly.
        (pbl--update-or-insert-version "0")
        (pbl--ensure-ends-here-line file-path)
        (cl-flet ((package-strip-rcs-id (str) "0"))
          (pbl--package-buffer-info-vec))))))

(defun pbl--get-pkg-file-info (file-path)
  "Get a vector of package info from \"-pkg.el\" file FILE-PATH."
  (when (file-exists-p file-path)
    (let ((package-def (pbl--read-from-file file-path)))
      (if (eq 'define-package (car package-def))
          (let* ((pkgfile-info (cdr package-def))
                 (descr (nth 2 pkgfile-info))
                 (rest-plist (cl-subseq pkgfile-info (min 4 (length pkgfile-info))))
                 (extras (let (alist)
                           (while rest-plist
                             (unless (memq (car rest-plist) '(:kind :archive))
                               (let ((value (cadr rest-plist)))
                                 (when value
                                   (push (cons (car rest-plist)
                                               (if (eq (car-safe value) 'quote)
                                                   (cadr value)
                                                 value))
                                         alist))))
                             (setq rest-plist (cddr rest-plist)))
                           alist)))
            (when (string-match "[\r\n]" descr)
              (error "Illegal multi-line package description in %s" file-path))
            (vector
             (nth 0 pkgfile-info)
             (mapcar
              (lambda (elt)
                (unless (symbolp (car elt))
                  (error "Invalid package name in dependency: %S" (car elt)))
                (list (car elt) (version-to-list (cadr elt))))
              (eval (nth 3 pkgfile-info)))
             descr
             (nth 1 pkgfile-info)
             extras))
        (error "No define-package found in %s" file-path)))))

(defun pbl--merge-package-info (pkg-info name version)
  "Return a version of PKG-INFO updated with NAME, VERSION and info from CONFIG.
If PKG-INFO is nil, an empty one is created."
  (let* ((merged (or (copy-sequence pkg-info)
                     (vector name nil "No description available." version))))
    (aset merged 0 name)
    (aset merged 3 version)
    merged))

(defun pbl--archive-entry (pkg-info type)
  "Return the archive-contents cons cell for PKG-INFO and TYPE."
  (let* ((name (intern (aref pkg-info 0)))
         (requires (aref pkg-info 1))
         (desc (or (aref pkg-info 2) "No description available."))
         (version (aref pkg-info 3))
         (extras (when (> (length pkg-info) 4)
                   (aref pkg-info 4))))
    (cons name
          (vector (version-to-list version)
                  requires
                  desc
                  type
                  extras))))

(defun pbl--archive-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     pbl-archive-dir)))

(defun pbl--entry-file-name (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0))))
    (expand-file-name
     (format "%s-%s.entry" name version)
     pbl-archive-dir)))

(defun pbl--delete-file-if-exists (file)
  "Delete FILE if it exists."
  (when (file-exists-p file)
    (delete-file file)))

(defun pbl--remove-archive-files (archive-entry)
  "Remove ARCHIVE-ENTRY from archive-contents, and delete associated file.
Note that the working directory (if present) is not deleted by
this function, since the archive list may contain another version
of the same-named package which is to be kept."
  (pbl--message "Removing archive: %s" archive-entry)
  (mapcar 'pbl--delete-file-if-exists
          (list  (pbl--archive-file-name archive-entry)
                 (pbl--entry-file-name archive-entry))))

(defun pbl--read-recipe (file-name)
  "Return the plist of recipe info for the package called FILE-NAME.
It performs some basic checks on the recipe to ensure that known
keys have values of the right types, and raises an error if that
is the not the case.  If invalid combinations of keys are
supplied then errors will only be caught when an attempt is made
to build the recipe."
  (let* ((pkg-info (pbl--read-from-file file-name))
         (pkg-name (car pkg-info))
         (rest (cdr pkg-info)))
    (cl-assert pkg-name)
    (cl-assert (symbolp pkg-name))
    (cl-assert (string= (symbol-name pkg-name) (file-name-nondirectory file-name))
               nil
               "Recipe '%s' contains mismatched package name '%s'"
               (file-name-nondirectory file-name)
               (car pkg-info))
    (cl-assert rest)
    (let* ((symbol-keys '(:fetcher))
           (string-keys '(:url :repo :module :commit :branch :version-regexp))
           (list-keys '(:files :old-names))
           (all-keys (append symbol-keys string-keys list-keys)))
      (dolist (thing rest)
        (when (keywordp thing)
          (cl-assert (memq thing all-keys) nil "Unknown keyword %S" thing)))
      (let ((fetcher (plist-get rest :fetcher)))
        (cl-assert fetcher nil ":fetcher is missing")
        (when (memq fetcher '(github gitlab bitbucket))
          (cl-assert (plist-get rest :repo) ":repo is missing")))
      (dolist (key symbol-keys)
        (let ((val (plist-get rest key)))
          (when val
            (cl-assert (symbolp val) nil "%s must be a list but is %S" key val))))
      (dolist (key list-keys)
        (let ((val (plist-get rest key)))
          (when val
            (cl-assert (listp val) nil "%s must be a list but is %S" key val ))))
      (dolist (key string-keys)
        (let ((val (plist-get rest key)))
          (when val
            (cl-assert (stringp val) nil "%s must be a string but is %S" key val )))))
    pkg-info))

(defun pbl--read-recipes ()
  "Return a list of data structures for all recipes in `pbl-recipes-dir'."
  (cl-loop for file-name in (directory-files  pbl-recipes-dir t "^[^.]")
           collect (pbl--read-recipe file-name)))

(defun pbl--read-recipes-ignore-errors ()
  "Return a list of data structures for all recipes in `pbl-recipes-dir'."
  (cl-loop for file-name in (directory-files  pbl-recipes-dir t "^[^.]")
           for pkg-info = (condition-case err (pbl--read-recipe file-name)
                            (error (pbl--message "Error reading recipe %s: %s"
                                                           file-name
                                                           (error-message-string err))
                                   nil))
           when pkg-info
           collect pkg-info))


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
               info-path)
              (pbl--message "Created %s" info-path))))
        (pbl--message "Removing %s" (expand-file-name dest-file target-dir))
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

(defun pbl--copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (cl-loop for (source-file . dest-file) in files
           do (pbl--copy-file
               (expand-file-name source-file source-dir)
               (expand-file-name dest-file target-dir))))

(defun pbl--copy-file (file newname)
  "Copy FILE to NEWNAME and create parent directories for NEWNAME if they don't exist."
  (let ((newdir (file-name-directory newname)))
    (unless (file-exists-p newdir)
      (make-directory newdir t)))
  (cond
   ((file-regular-p file)
    (pbl--message "%s -> %s" file newname)
    (copy-file file newname))
   ((file-directory-p file)
    (pbl--message "%s => %s" file newname)
    (copy-directory file newname))))


(defun pbl--package-name-completing-read ()
  "Prompt for a package name, returning a symbol."
  (intern (completing-read "Package: " (pbl-recipe-alist))))

(defun pbl--find-source-file (target files)
  "Search for source of TARGET in FILES."
  (let* ((entry (rassoc target files)))
    (when entry (car entry))))

(defun pbl--find-package-file (name)
  "Return the filename of the most recently built package of NAME."
  (pbl--archive-file-name (assoc name (pbl-archive-alist))))

(defun pbl--package-buffer-info-vec ()
  "Return a vector of package info.
`package-buffer-info' returns a vector in older Emacs versions,
and a cl struct in Emacs HEAD.  This wrapper normalises the results."
  (let ((desc (package-buffer-info))
        (keywords (lm-keywords-list)))
    (if (fboundp 'package-desc-create)
        (let ((extras (package-desc-extras desc)))
          (when (and keywords (not (assq :keywords extras)))
            ;; Add keywords to package properties, if not already present
            (push (cons :keywords keywords) extras))
          (vector (package-desc-name desc)
                  (package-desc-reqs desc)
                  (package-desc-summary desc)
                  (package-desc-version desc)
                  extras))
      ;; The regexp and the processing is taken from `lm-homepage' in Emacs 24.4
      (let* ((page (lm-header "\\(?:x-\\)?\\(?:homepage\\|url\\)"))
             (homepage (if (and page (string-match "^<.+>$" page))
                           (substring page 1 -1)
                         page))
             extras)
        (when keywords (push (cons :keywords keywords) extras))
        (when homepage (push (cons :url homepage) extras))
        (vector  (aref desc 0)
                 (aref desc 1)
                 (aref desc 2)
                 (aref desc 3)
                 extras)))))

(defconst pbl--this-file load-file-name)

;; TODO: This function should be fairly sound, but it has a few
;; possible failure modes. Primarily, if a file matching the recipe's
;; file spec appears in a new upstream revision, but that file has an
;; older date than the version timestamp provided here, the function
;; will return t.
(defun pbl--up-to-date-p (name version)
  "Return non-nil if there is an up-to-date package for NAME with the given VERSION."
  (let* ((package-file-base (expand-file-name (format "%s-%s." name version) pbl-archive-dir))
         (recipe-file (expand-file-name name pbl-recipes-dir)))
    (cl-dolist (ext '("tar" "el"))
      (let ((package-file (concat package-file-base ext)))
        (when (and (file-newer-than-file-p package-file recipe-file)
                   (or (null pbl--this-file)
                       (file-newer-than-file-p package-file pbl--this-file)))
          (cl-return t))))))


;;; Public interface
;;;###autoload
(defun pbl-archive (name)
  "Build a package archive for package NAME."
  (interactive (list (pbl--package-name-completing-read)))
  (let* ((file-name (symbol-name name))
         (rcp (or (cdr (assoc name (pbl-recipe-alist)))
                  (error "Cannot find package %s" (symbol-name name))))
         (pkg-working-dir
          (file-name-as-directory
           (expand-file-name file-name pbl-working-dir))))

    (unless (file-exists-p pbl-archive-dir)
      (pbl--message "Creating directory %s" pbl-archive-dir)
      (make-directory pbl-archive-dir))

    (pbl--message "\n;;; %s\n" file-name)
    (let* ((version (pbl-checkout name rcp pkg-working-dir))
           (default-directory pbl-working-dir)
           (start-time (current-time)))
      (if (pbl--up-to-date-p (symbol-name name) version)
          (pbl--message "Package %s is up to date - skipping." name)
        (progn
          (let ((archive-entry (pbl-package (symbol-name name)
                                                      version
                                                      (pbl--config-file-list rcp)
                                                      pkg-working-dir
                                                      pbl-archive-dir)))
            (pbl--dump archive-entry (pbl--entry-file-name archive-entry)))
          (when pbl-write-melpa-badge-images
            (pbl--write-melpa-badge-image (symbol-name name) version pbl-archive-dir))
          (pbl--message "Built %s in %.3fs, finished at %s"
                                  name
                                  (time-to-seconds (time-since start-time))
                                  (current-time-string))))
      (list file-name version))))

;;;###autoload
(defun pbl-package (package-name version file-specs source-dir target-dir)
  "Create PACKAGE-NAME with VERSION.

The information in FILE-SPECS is used to gather files from
SOURCE-DIR.

The resulting package will be stored as a .el or .tar file in
TARGET-DIR, depending on whether there are multiple files.

Argument FILE-SPECS is a list of specs for source files, which
should be relative to SOURCE-DIR.  The specs can be wildcards,
and optionally specify different target paths.  They extended
syntax is currently only documented in the MELPA README.  You can
simply pass `pbl-default-files-spec' in most cases.

Returns the archive entry for the package."
  (when (symbolp package-name)
    (setq package-name (symbol-name package-name)))
  (let ((files (pbl-expand-file-specs source-dir file-specs)))
    (unless (equal file-specs pbl-default-files-spec)
      (when (equal files (pbl-expand-file-specs
                          source-dir pbl-default-files-spec nil t))
        (pbl--message "Note: %s :files spec is equivalent to the default."
                                package-name)))
    (cond
     ((not version)
      (error "Unable to check out repository for %s" package-name))
     ((= 1 (length files))
      (pbl--build-single-file-package package-name version (caar files) source-dir target-dir))
     ((< 1 (length  files))
      (pbl--build-multi-file-package package-name version files source-dir target-dir))
     (t (error "Unable to find files matching recipe patterns")))))

(defun pbl--build-single-file-package (package-name version file source-dir target-dir)
  (let* ((pkg-source (expand-file-name file source-dir))
         (pkg-target (expand-file-name
                      (concat package-name "-" version ".el")
                      target-dir))
         (pkg-info (pbl--merge-package-info
                    (pbl--get-package-info pkg-source)
                    package-name
                    version)))
    (unless (string-equal (downcase (concat package-name ".el"))
                          (downcase (file-name-nondirectory pkg-source)))
      (error "Single file %s does not match package name %s"
             (file-name-nondirectory pkg-source) package-name))
    (if (file-exists-p pkg-target)
        (pbl--message "Skipping rebuild of %s" pkg-target)
      (copy-file pkg-source pkg-target)
      (let ((enable-local-variables nil)
            (make-backup-files nil))
        (with-current-buffer (find-file pkg-target)
          (pbl--update-or-insert-version version)
          (pbl--ensure-ends-here-line pkg-source)
          (write-file pkg-target nil)
          (condition-case err
              (pbl--package-buffer-info-vec)
            (error
             (pbl--message "Warning: %S" err)))
          (kill-buffer)))

      (pbl--write-pkg-readme target-dir
                                       (pbl--find-package-commentary pkg-source)
                                       package-name))
    (pbl--archive-entry pkg-info 'single)))

(defun pbl--build-multi-file-package (package-name version files source-dir target-dir)
  (let ((tmp-dir (file-name-as-directory (make-temp-file package-name t))))
    (unwind-protect
        (let* ((pkg-dir-name (concat package-name "-" version))
               (pkg-tmp-dir (expand-file-name pkg-dir-name tmp-dir))
               (pkg-file (concat package-name "-pkg.el"))
               (pkg-file-source (or (pbl--find-source-file pkg-file files)
                                    pkg-file))
               (file-source (concat package-name ".el"))
               (pkg-source (or (pbl--find-source-file file-source files)
                               file-source))
               (pkg-info (pbl--merge-package-info
                          (let ((default-directory source-dir))
                            (or (pbl--get-pkg-file-info pkg-file-source)
                                ;; some packages (like magit) provide name-pkg.el.in
                                (pbl--get-pkg-file-info
                                 (expand-file-name (concat pkg-file ".in")
                                                   (file-name-directory pkg-source)))
                                (pbl--get-package-info pkg-source)))
                          package-name
                          version)))
          (pbl--copy-package-files files source-dir pkg-tmp-dir)
          (pbl--write-pkg-file (expand-file-name pkg-file
                                                           (file-name-as-directory pkg-tmp-dir))
                                         pkg-info)

          (pbl--generate-info-files files source-dir pkg-tmp-dir)
          (pbl--generate-dir-file files pkg-tmp-dir)

          (let ((default-directory tmp-dir))
            (pbl--create-tar (expand-file-name (concat package-name "-" version ".tar")
                                                         target-dir)
                                       pkg-dir-name))

          (let ((default-directory source-dir))
            (pbl--write-pkg-readme target-dir
                                             (pbl--find-package-commentary pkg-source)
                                             package-name))
          (pbl--archive-entry pkg-info 'tar))
      (delete-directory tmp-dir t nil))))


;; In future we should provide a hook, and perform this step in a separate package.
;; Note also that it would be straightforward to generate the SVG ourselves, which would
;; save the network overhead.
(defun pbl--write-melpa-badge-image (package-name version target-dir)
  (let ((badge-url (concat "https://img.shields.io/badge/"
                           (if pbl-stable "melpa stable" "melpa")
                           "-"
                           (url-hexify-string version)
                           "-"
                           (if pbl-stable "3e999f" "922793")
                           ".svg"))
        (badge-filename (expand-file-name (concat package-name "-badge.svg") target-dir)))
    (if (executable-find "curl")
        ;; Not strictly needed, but less likely to break due to gnutls issues
        (shell-command (mapconcat #'identity
                                  (list "curl" "-f" "-o"
                                        (shell-quote-argument badge-filename)
                                        (shell-quote-argument badge-url))
                                  " "))
      (pbl--url-copy-file badge-url badge-filename t))))


;;; Helpers for recipe authors

(defvar pbl-minor-mode-map
  (let ((m (make-sparse-keymap)))
    (define-key m (kbd "C-c C-c") 'pbl-current-recipe)
    m)
  "Keymap for `pbl-minor-mode'.")

(define-minor-mode pbl-minor-mode
  "Helpful functionality for building packages."
  nil
  " PBuild"
  pbl-minor-mode-map
  (when pbl-minor-mode
    (message "Use C-c C-c to build this recipe.")))

;;;###autoload
(defun pbl-create-recipe (name fetcher)
  "Create a new recipe for package NAME using FETCHER."
  (interactive
   (list (intern (read-string "Package name: "))
         (intern
          (let ((fetcher-types (mapcar #'symbol-name '(github gitlab bitbucket git wiki bzr hg cvs svn))))
            (completing-read
             "Fetcher: "
             fetcher-types
             nil t nil nil (car fetcher-types))))))
  (let ((recipe-file (expand-file-name (symbol-name name) pbl-recipes-dir)))
    (when (file-exists-p recipe-file)
      (error "Recipe already exists"))
    (find-file recipe-file)
    (let* ((extra-params
            (cond
             ((eq 'github fetcher) '(:repo "USER/REPO"))
             ((eq 'wiki fetcher) '())
             (t '(:url "SCM_URL_HERE"))))
           (template `(,name :fetcher ,fetcher ,@extra-params)))
      (insert (pp-to-string template))
      (emacs-lisp-mode)
      (pbl-minor-mode)
      (goto-char (point-min)))))

;;;###autoload
(defun pbl-current-recipe ()
  "Build archive for the recipe defined in the current buffer."
  (interactive)
  (unless (and (buffer-file-name)
               (file-equal-p (file-name-directory (buffer-file-name))
                             pbl-recipes-dir))
    (error "Buffer is not visiting a recipe"))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save file %s? " buffer-file-name))
        (save-buffer)
      (error "Aborting")))
  (check-parens)
  (pbl-reinitialize)
  (let ((pkg-name (intern (file-name-nondirectory (buffer-file-name)))))
    (pbl-archive pkg-name)
    (pbl-dump-archive-contents)
    (let ((output-buffer-name "*pbl-result*"))
      (with-output-to-temp-buffer output-buffer-name
        (princ ";; Please check the following package descriptor.\n")
        (princ ";; If the correct package description or dependencies are missing,\n")
        (princ ";; then the source .el file is likely malformed, and should be fixed.\n")
        (pp (assoc pkg-name (pbl-archive-alist))))
      (with-current-buffer output-buffer-name
        (emacs-lisp-mode)
        (view-mode)))
    (when (yes-or-no-p "Install new package? ")
      (package-install-file (pbl--find-package-file pkg-name)))))

(defun pbl-archive-ignore-errors (pkg)
  "Build archive for package PKG, ignoring any errors."
  (interactive (list (pbl--package-name-completing-read)))
  (let* ((debug-on-error t)
         (debug-on-signal t)
         (pbl--debugger-return nil)
         (debugger (lambda (&rest args)
                     (setq pbl--debugger-return (with-output-to-string
                                                            (backtrace))))))
    (condition-case err
        (pbl-archive pkg)
      (error
       (pbl--message "%s" (error-message-string err))
       nil))))



;;;###autoload
(defun pbl-all ()
  "Build all packages in the `pbl-recipe-alist'."
  (interactive)
  (let ((failed (cl-loop for pkg in (mapcar 'car (pbl-recipe-alist))
                         when (not (pbl-archive-ignore-errors pkg))
                         collect pkg)))
    (if (not failed)
        (princ "\nSuccessfully Compiled All Packages\n")
      (princ "\nFailed to Build the Following Packages\n")
      (princ (mapconcat 'symbol-name failed "\n"))
      (princ "\n")))
  (pbl-cleanup))

(defun pbl-cleanup ()
  "Remove previously-built packages that no longer have recipes."
  (interactive)
  (let* ((known-package-names (mapcar 'car (pbl-recipe-alist)))
         (stale-archives (cl-loop for built in (pbl--archive-entries)
                                  when (not (memq (car built) known-package-names))
                                  collect built)))
    (mapc 'pbl--remove-archive-files stale-archives)
    (pbl-dump-archive-contents)))

(defun pbl-recipe-alist ()
  "Retun the list of avalailable packages."
  (unless pbl--recipe-alist-initialized
    (setq pbl--recipe-alist (pbl--read-recipes-ignore-errors)
          pbl--recipe-alist-initialized t))
  pbl--recipe-alist)

(defun pbl-archive-alist ()
  "Return the archive list."
  (cdr (pbl--read-from-file
        (expand-file-name "archive-contents"
                          pbl-archive-dir))))

(defun pbl-reinitialize ()
  "Forget any information about packages which have already been built."
  (interactive)
  (setq pbl--recipe-alist-initialized nil))

(defun pbl-dump-archive-contents (&optional file-name pretty-print)
  "Dump the list of built packages to FILE-NAME.

If FILE-NAME is not specified, the default archive-contents file is used."
  (pbl--dump (cons 1 (pbl--archive-entries))
                       (or file-name
                           (expand-file-name "archive-contents" pbl-archive-dir))
                       pretty-print))

(defun pbl--archive-entries ()
  "Read all .entry files from the archive directory and return a list of all entries."
  (let ((entries '()))
    (dolist (new (mapcar 'pbl--read-from-file
                         (directory-files pbl-archive-dir t
                                          ".*\.entry$"))
                 entries)
      (let ((old (assq (car new) entries)))
        (when old
          (when (version-list-< (elt (cdr new) 0)
                                (elt (cdr old) 0))
            ;; swap old and new
            (cl-rotatef old new))
          (pbl--remove-archive-files old)
          (setq entries (remove old entries)))
        (add-to-list 'entries new)))))



;;; Exporting data as json

(defun pbl-recipe-alist-as-json (file-name)
  "Dump the recipe list to FILE-NAME as json."
  (interactive)
  (with-temp-file file-name
    (insert (json-encode (pbl-recipe-alist)))))

(defun pbl--sym-to-keyword (s)
  "Return a version of symbol S as a :keyword."
  (intern (concat ":" (symbol-name s))))

(defun pbl--pkg-info-for-json (info)
  "Convert INFO into a data structure which will serialize to JSON in the desired shape."
  (let* ((ver (elt info 0))
         (deps (elt info 1))
         (desc (elt info 2))
         (type (elt info 3))
         (props (when (> (length info) 4) (elt info 4))))
    (list :ver ver
          :deps (cl-mapcan (lambda (dep)
                             (list (pbl--sym-to-keyword (car dep))
                                   (cadr dep)))
                           deps)
          :desc desc
          :type type
          :props props)))

(defun pbl--archive-alist-for-json ()
  "Return the archive alist in a form suitable for JSON encoding."
  (cl-mapcan (lambda (entry)
               (list (pbl--sym-to-keyword (car entry))
                     (pbl--pkg-info-for-json (cdr entry))))
             (pbl-archive-alist)))

(defun pbl-archive-alist-as-json (file-name)
  "Dump the build packages list to FILE-NAME as json."
  (interactive)
  (with-temp-file file-name
    (insert (json-encode (pbl--archive-alist-for-json)))))


(provide 'pbl)

;; Local Variables:
;; coding: utf-8
;; checkdoc-minor-mode: 1
;; End:

;;; pbl.el ends here
