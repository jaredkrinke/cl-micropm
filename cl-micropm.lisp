(defpackage micropm
  (:use :cl)
  (:export #:setup))

(in-package :micropm)

;; Use CL_ROOT for CL projects root (or parent directory, if not specified)
(defvar *root-dir* (let ((cl-root (uiop:getenv "CL_ROOT")))
		     (if cl-root
			 (make-pathname :directory cl-root)
			 (make-pathname :directory '(:relative :up)))))

;; Store dependencies in "deps/"
(defvar *deps-dir* (uiop:merge-pathnames* (make-pathname :directory '(:relative "deps")) *root-dir*))

;; Try to find cl-micropm (TODO: actual algorithm)
(defvar *micropm-dir*
  (uiop:merge-pathnames* (make-pathname :directory '(:relative "cl-micropm"))
			 *root-dir*))

(defvar *quicklisp-projects-dir*
  (uiop:merge-pathnames*
   (make-pathname :directory '(:relative "quicklisp-projects" "projects"))
   *micropm-dir*))

(defun split-sequence (sequence delimiter)
  "Splits a sequence by delimiter (which is then omitted)"
  (loop for i = 0 then (1+ j)
	for j = (position delimiter sequence :start i)
	collect (subseq sequence i j)
	while j))

(defun split-string-on-spaces (sequence)
  "Splits a string into words delimited by a single space character"
  (split-sequence sequence #\Space))

(defun generate-quicklisp-index ()
  "Generates the quicklisp index"
  ;; https://github.com/quicklisp/quicklisp-controller/blob/master/indexes.lisp#L162
  (let* ((systems-path (uiop:merge-pathnames* "systems.txt" *micropm-dir*))
	 (systems-lines (cdr (uiop:read-file-lines systems-path)))
         (systems (loop for line in systems-lines
			collect (split-string-on-spaces line))))
    (loop for x in systems
          ;; Just get the main system for a project, and its dependencies
          when (and (equal (first x) (second x)) (equal (first x) (third x)))
            collect (cddr x))))

;; Quicklisp systems index (obtained from systems.txt)
(defvar *systems-alist* (generate-quicklisp-index))

(defun setup (system-name &key clone dry-run)
  "Sets up micropm and the project's dependencies"
  (ensure-directories-exist *deps-dir*)

  ;; Clone the dependencies listed in the system
  (clone-dependencies system-name :clone clone :dry-run dry-run))

(defun fetch-system-quicklisp-source (system-name)
  "Fetches the quicklisp source for the given system"
  (let ((system-source
          (uiop:merge-pathnames* (make-pathname :directory (list :relative (string-downcase system-name))
						:name "source.txt")
				 *quicklisp-projects-dir*)))
    (map 'list (lambda (source) (uiop:split-string source :separator " "))
         (uiop:read-file-lines system-source))))

(defun get-local-system (system-name)
  "Returns an ASDF system if one exists; otherwise returns NIL"
  (asdf:find-system system-name nil))

(defun is-local-system (system-name)
  "Returns non-NIL if the system is known locally to ASDF"
  (get-local-system system-name))

(defun get-direct-dependencies (system-name)
  "Returns the direct dependencies of a system (favoring dependencies ASDF already knows about over Quicklisp's dependency index)"
  (let ((system (get-local-system system-name)))
    (if system
	(asdf:system-depends-on system)
	(rest (assoc system-name *systems-alist* :test 'equal)))))

(defun get-dependencies-recursive (system-name)
  "Recursively finds all of the dependencies for the system (favoring dependencies ASDF already knows about over Quicklisp's dependency index)"
  (let ((dependencies (get-direct-dependencies system-name)))
    (if dependencies
        (let ((list (mapcan (lambda (x) (cons system-name (get-dependencies-recursive x))) dependencies)))
          (remove-duplicates list :test 'equal))
        (list system-name))))

(defun get-dependencies (system-name)
  "Finds all (transitive) dependencies of a system (favoring dependencies ASDF already knows about over Quicklisp's dependency index), excluding ASDF and UIOP"
  ;; Filter out ASDF and UIOP since they come bundled with the Common Lisp implementation
  (set-difference (get-dependencies-recursive system-name) '("asdf" "uiop") :test 'equal))

(defun get-source-type (source)
  (first source))

;; TODO: Always use HTTPS!
(defun http-get-source-p (source)
  (member-if (lambda (x) (equal (get-source-type source) x))
             '("http" "https" "single-file")))

;; TODO: Is this encrypted? If not, use an encrypted version!
(defun git-clone-source-p (source)
  (member-if (lambda (x) (equal (get-source-type source) x))
             '("git" "latest-github-release" "latest-github-tag" "latest-gitlab-release")))

(defun git-clone-tagged-source-p (source)
  (member-if (lambda (x) (equal (get-source-type source) x))
             '("branched-git" "tagged-git")))

;; TODO: Consolidate git "source.txt"
(defun clone-dependency (system-name source &key clone dry-run)
  (flet ((run-command (command) (if dry-run
				    (format t "~a~%" command)
				    (uiop:run-program command :output t))))
    (let ((url (second source))
          (dir (uiop:merge-pathnames* (make-pathname :directory (list :relative system-name)) *deps-dir*))
          (git-cmd (if clone "clone" "submodule add -f")))
      (unless (uiop:directory-exists-p dir)
	(format t "Cloning ~a...~%" system-name)
	(cond
	  ((http-get-source-p source)
	   (run-command (format nil "wget ~a ~a" url dir)))
	  ((git-clone-source-p source)
	   (run-command (format nil "git ~a --depth 1 ~a ~a" git-cmd url dir)))
	  ((git-clone-tagged-source-p source)
	   (let ((tag (third source)))
             (run-command (format nil "git ~a --depth 1 ~a#~a ~a" git-cmd url tag dir))))
	  (t (error (format nil "Unimplemented for source: ~a" source))))))))

(defun clone-dependencies (system-name &key clone dry-run)
  "Clones the dependencies of a Quicklisp system"
  (let ((dependencies (get-dependencies system-name)))
    (loop for system-name in dependencies
	  when (not (is-local-system system-name))
	    do (clone-dependency system-name
				 (first (fetch-system-quicklisp-source system-name))
				 :clone clone
				 :dry-run dry-run))))
