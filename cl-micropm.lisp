(defpackage micropm
  (:use :cl))

(in-package :micropm)

(defvar *lisp-systems-dir* #P"./lisp-systems")

(defvar *quicklisp-projects-dir*
  (uiop:merge-pathnames* #P"quicklisp-projects/projects/" (uiop:getcwd)))

(defun init (system-name)
  (add-local-project-to-asdf)
  (loop for i in (locate-dependencies system-name) do
    (print i)))

#+nil(init "micropm")

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
          (uiop:merge-pathnames* (uiop:strcat system-name "/source.txt")
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
  (let ((dependencies (rest (assoc system alist))))
    (if dependencies
        (let ((list (mapcan (lambda (x) (cons system (micropm::get-deps x alist))) dependencies)))
          (remove-duplicates list))
        (list system))))

(defun get-dependencies (system systems-alist)
  (loop for x in (get-deps system systems-alist)
        when (not (member x `(,system uiop asdf))) collect x))

#+nil(get-dependencies 'cffi *systems-alist*)

#|
branched-git
cvs
darcs
ediware-http
git
http
https
kmr-git
latest-github-release
LATEST-GITHUB-RELEASE
latest-github-tag
LATEST-GITHUB-TAG
latest-gitlab-release
mercurial
single-file
svn
tagged-git
|#

(defun clone-dependencies (system systems-alist)
  (let ((dependencies (get-dependencies system systems-alist)))
    ()
    ))

(defun add-dependency (system-name)
  "Configures ASDF to include the dependency"
  (declaime (ignore system-name)))

(defun setup-asdf-central-registry (lisp-systems-paths)
  "Setup ASDF to read the systems already setup in lisp-systems dir"
  (setf asdf:*central-registry* lisp-systems-paths))

(defun list-lisp-systems-paths ()
  "Lists the paths of the dependencies in lisp-systems"
  (let ((dir (uiop:merge-pathnames* (uiop:getcwd) *lisp-systems-dir*)))
    (uiop:subdirectories dir)))

#+nil(push
      (uiop:strcat (uiop:native-namestring (uiop:getcwd)) "lisp-systems/babel/")
      asdf:*central-registry*)
