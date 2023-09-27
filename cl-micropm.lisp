(defpackage micropm
  (:use :cl)
  (:export #:setup))

(in-package :micropm)

;; Use CL_ROOT for CL projects root (or parent directory, if not specified)
(defvar *root-dir* (let ((cl-root (uiop:getenv "CL_ROOT")))
		     (if cl-root
			 (make-pathname :directory cl-root)
			 (uiop:merge-pathnames* "../" (uiop:getcwd)))))

;; Store dependencies in "deps/"
(defvar *deps-dir* (uiop:merge-pathnames* "deps/" *root-dir*))

;; Try to find cl-micropm (TODO: actual algorithm)
(defvar *micropm-dir* (uiop:merge-pathnames* "cl-micropm/" *root-dir*))
(defvar *quicklisp-projects-dir* (uiop:merge-pathnames* "quicklisp-projects/projects/" *micropm-dir*))

(defun setup (system-name &key dry-run)
  "Sets up micropm and the project's dependencies"
  (ensure-directories-exist *deps-dir*)

  ;; Quicklisp systems index (obtained from systems.txt)
  (unless (boundp '*systems-alist*)
    (defvar *systems-alist* (generate-quicklisp-index)))

  ;; Clone the dependencies listed in the system
  (loop for dependency-name in (locate-dependencies system-name) do
    (clone-dependencies dependency-name *systems-alist* :dry-run dry-run)))

(defun locate-dependencies (system-name)
  "Locates the dependencies of system-name"
  (asdf:system-depends-on (asdf:find-system system-name)))

(defun fetch-system-quicklisp-source (system-name)
  "Fetches the quicklisp source for the given system"
  (let ((system-source
          (uiop:merge-pathnames* (format nil "~a/source.txt" (string-downcase system-name))
                                 *quicklisp-projects-dir*)))
    (map 'list (lambda (source) (uiop:split-string source :separator " "))
         (uiop:read-file-lines system-source))))

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

(defun get-deps (system alist)
  "Recursively finds all of the dependencies for the system"
  (let* ((system-name (intern (string-upcase system)))
         (dependencies
          (rest (assoc-if (lambda (x) (equal system-name x)) alist))))
    (if dependencies
        (let ((list (mapcan (lambda (x) (cons system-name (micropm::get-deps x alist)))
                            dependencies)))
          (remove-duplicates list))
        (list system-name))))

(defun get-dependencies (system systems-alist)
  (let ((system-name (intern (string-upcase system))))
    ;; Filter out ASDF and UIOP since they come bundled with the Common Lisp implementation
    (loop for x in (get-deps system-name systems-alist)
          when (not (member-if
                     (lambda (e) (equal (symbol-name x) e))
                     `(,(string-upcase system) "UIOP" "ASDF")))
            collect x)))

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

(defun clone-dependency (system-name source &key dry-run)
  (flet ((run-command (command) (if dry-run
				    (format nil "~a~%" command)
				    (uiop:run-program command :output t))))
    (let ((url (second source))
          (dir (uiop:merge-pathnames* *deps-dir* system-name)))
      (unless (uiop:directory-exists-p dir)
	(format t "Cloning ~a...~%" system-name)
	(cond
	  ((http-get-source-p source)
	   (run-command (format nil "wget ~a ~a" url dir)))
	  ((git-clone-source-p source)
	   (run-command (format nil "git clone ~a ~a" url dir)))
	  ((git-clone-tagged-source-p source)
	   (let ((tag (third source)))
             (run-command (format nil "git clone ~a#~a ~a" url tag dir))))
	  (t (error (format nil "Unimplemented for source: ~a" source))))))))

(defun clone-dependencies (system systems-alist &key dry-run)
  "Clones the dependencies of a Quicklisp system"
  (let ((dependencies (get-dependencies system systems-alist)))
    (loop for system-name in dependencies do
      (setf system-name (string-downcase system-name))
      (clone-dependency system-name
                        (first (fetch-system-quicklisp-source system-name))
			:dry-run dry-run))))
