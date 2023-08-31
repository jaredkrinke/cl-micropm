(defpackage micropm
  (:use :cl))

(in-package :micropm)

(require 'asdf)

(defvar *lisp-systems-dir* #P"./lisp-systems/")

(defvar *quicklisp-projects-dir*
  (uiop:merge-pathnames* #P"quicklisp-projects/projects/" (uiop:getcwd)))

(defun setup (system-name &key (ignore-error t))
  "Sets up micropm and the project's dependencies"
  ;; Quicklisp sources (obtained from the quicklisp-projects repo)
  (unless (uiop:directory-exists-p *quicklisp-projects-dir*)
    (add-quicklisp-projects-submodule))

  ;; Quicklisp systems index (obtained from a generated file from Dockerfile)
  (unless (boundp '*systems-alist*)
    (generate-quicklisp-index))

  ;; Clone the dependencies listed in the system
  (add-local-project-to-asdf)
  (loop for i in (locate-dependencies system-name) do
    (if ignore-error
        (ignore-errors (clone-dependencies system-name *systems-alist* :include-system t))
        (clone-dependencies system-name *systems-alist* :include-system t))))

#+nil(init "micropm")

(defun setup-asdf-registry ()
  "Initializes the ASDF registry with the existing dependencies in *lisp-systems-dir*"
  (setf asdf:*central-registry* (cons (uiop:getcwd) (list-lisp-systems-paths))))

(defun add-quicklisp-projects-submodule ()
  (uiop:run-program "git submodule add https://github.com/quicklisp/quicklisp-projects.git"))

(defun add-local-project-to-asdf ()
  "Configures ASDF to find the project in the current working directory"
  (when (not (find-if (lambda (e) (equal e (uiop:getcwd))) asdf:*central-registry*))
    (push (uiop:getcwd) asdf:*central-registry*)))

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

#+nil(fetch-system-quicklisp-source "xmls")

(defvar *quicklisp-container-name* "quicklisp")

#+nil(define-condition progress (condition)
  ((topic :initarg :topic)
   (msg :initarg :msg)))

#+nil(defmacro with-progress (&body body)
  `(handler-bind
    ((progress #'(lambda (condition)
      (with-slots (topic msg) condition
        (format t "~&* ~a: ~a" topic msg))
      (continue))))
     (progn ,@body)))

#+nil(with-progress (build-quicklisp-image))

(defconstant *dockerfile*
  "FROM debian:bullseye-slim
RUN apt-get update && apt-get install -y sbcl curl gnupg
RUN useradd -ms /bin/bash lisp
USER lisp
WORKDIR /home/lisp

# Setup quicklisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp
RUN curl -O https://beta.quicklisp.org/quicklisp.lisp.asc
RUN curl -O https://beta.quicklisp.org/release-key.txt
RUN gpg --import release-key.txt
RUN gpg --verify quicklisp.lisp.asc quicklisp.lisp
RUN sbcl --non-interactive \\
       --load quicklisp.lisp \\
       --eval '(quicklisp-quickstart:install)' \\
       --eval '(ql::without-prompting (ql:add-to-init-file))'")

(defun micropm::build-quicklisp-image ()
  "Builds an OCI container with quicklisp installed inside"
  ;; https://github.com/quicklisp/quicklisp-projects
  #+nil(signal 'progress :topic :build-quicklisp :msg "Building quicklisp image...")
  (multiple-value-bind (output err-output status-code)
      (uiop:run-program
       (format nil "podman build -t ~a -" *quicklisp-container-name*)
       :input
       (make-string-input-stream *dockerfile*)
       :output t
       :err-output t
       :ignore-error-status t)
    (declare (ignore output err-output))
    #+nil(signal 'progress :topic :build-quicklisp :msg (format nil "Command exited (~d)" status-code))
    status-code))

(defun micropm::quicklisp-image-exists-p ()
  (multiple-value-bind (output err-output status-code)
      (uiop:run-program (format nil "podman inspect --type=image ~a" *quicklisp-container-name*)
                        :ignore-error-status t)
    (declare (ignore output err-output))
    (if (= status-code 0) t nil)))

(defun generate-quicklisp-index ()
  "Generates the quicklisp index"
  ;; https://github.com/quicklisp/quicklisp-controller/blob/master/indexes.lisp#L162
  (when (not (quicklisp-image-exists-p))
    (build-quicklisp-image))

  (let* ((systems-path "/home/lisp/quicklisp/dists/quicklisp/systems.txt")
         (systems (uiop:run-program
                   (format nil
                           "podman run --rm --entrypoint cat ~a ~a | tail -n +2 | sed -e '1i(' -e '$a)' -e 's/^/(/g' -e 's/$/)/g'"
                           *quicklisp-container-name*
                           systems-path)
                   :output '(:string :stripped t)
                   :ignore-error-status t)))
    (loop for x in (read-from-string systems)
          ;; Just get the main system for a project, and it's dependencies
          when (and (eql (first x) (second x)) (eql (first x) (third x)))
            collect (cddr x))))

#+nil(defvar *systems-alist* (generate-quicklisp-index))

(defun micropm::get-deps (system alist)
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
    (loop for x in (get-deps system-name systems-alist)
          when (not (member x `(,system-name uiop asdf))) collect x)))

#+nil(get-dependencies 'cffi *systems-alist*)

#+nil(defconstant +source-types+
  '(branched-git
    cvs
    darcs
    ediware-http
    git
    http
    https
    kmr-git
    latest-github-release
    latest-github-tag
    latest-gitlab-release
    mercurial
    single-file
    svn
    tagged-git))

(defun get-source-type (source)
  (first source))

(defun ediware-p (source)
  "Git source: https://github.com/edicl/"
  (equal (get-source-type source) "ediware-http"))

(defun kmr-p (source)
  "Git source: http://git.kpe.io/"
  (equal (get-source-type source) "kmr-git"))

(defun http-get-source-p (source)
  (member-if (lambda (x) (equal (get-source-type source) x))
             '("http" "https" "single-file")))

(defun git-clone-source-p (source)
  (member-if (lambda (x) (equal (get-source-type source) x))
             '("git" "latest-github-release" "latest-github-tag" "latest-gitlab-release")))

(defun git-clone-tagged-source-p (source)
  (member-if (lambda (x) (equal (get-source-type source) x))
             '("branched-git" "tagged-git")))

(defun clone-dependency (system-name source &key (clone nil))
  (let ((url (second source))
        (dir (uiop:merge-pathnames* *lisp-systems-dir* system-name))
        (git-cmd (if clone "clone" "submodule add")))
    (cond
      ((http-get-source-p source)
       (uiop:run-program (format nil "wget ~a ~a" url dir) :output t))
      ((git-clone-source-p source)
       (uiop:run-program (format nil "git ~a ~a ~a" git-cmd url dir) :output t))
      ((git-clone-tagged-source-p source)
       (let ((tag (third source)))
         (uiop:run-program (format nil "git ~a ~a#~a ~a" git-cmd url tag dir) :output t)))
      (t (error (format nil "Unimplemented for source: ~a" source))))))

(defun clone-dependencies (system systems-alist &key (include-system t) (clone nil))
  (let ((dependencies (get-dependencies system systems-alist)))
    (loop for system-name in dependencies do
      (setf system-name (string-downcase system-name))
      (clone-dependency system-name
                        (first (fetch-system-quicklisp-source system-name))
                        :clone clone))
    (when include-system
      (clone-dependency system
                        (first (fetch-system-quicklisp-source system))
                        :clone clone))))

(defun add-dependency (system-name)
  "Configures ASDF to include the dependency"
  (declaim (ignore system-name)))

(defun setup-asdf-central-registry (lisp-systems-paths)
  "Setup ASDF to read the systems already setup in lisp-systems dir"
  (setf asdf:*central-registry* lisp-systems-paths))

(defun list-lisp-systems-paths ()
  "Lists the paths of the dependencies in lisp-systems"
  (let ((dir (uiop:merge-pathnames* *lisp-systems-dir* (uiop:getcwd))))
    (uiop:subdirectories dir)))

#+nil(push
      (uiop:strcat (uiop:native-namestring (uiop:getcwd)) "lisp-systems/babel/")
      asdf:*central-registry*)
