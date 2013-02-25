(in-package :cl-user)

(defvar *build-dir* (pathname-directory (pathname (concatenate 'string (getenv "BUILD_DIR") "/"))))
(defvar *cache-dir* (pathname-directory (pathname (concatenate 'string (getenv "CACHE_DIR") "/"))))
(defvar *buildpack-dir* (pathname-directory (pathname (concatenate 'string (getenv "BUILDPACK_DIR") "/"))))

(format t "~%*build-dir* = ~a" *build-dir*)
(format t "~%*cache-dir* = ~a" *cache-dir*)
(format t "~%*buildpack-dir* = ~a" *buildpack-dir*)

;;; Tell ASDF to store binaries in the cache dir
(ccl:setenv "XDG_CACHE_HOME" (concatenate 'string (getenv "CACHE_DIR") "/.asdf/"))

(require :asdf)

(let ((ql-setup (make-pathname :directory (append *cache-dir* '("quicklisp")) :defaults "setup.lisp")))
  (if (probe-file ql-setup)
      (load ql-setup)
      (progn
	(load (make-pathname :directory (append *buildpack-dir* '("lib")) :defaults "quicklisp.lisp"))
	(funcall (symbol-function (find-symbol "INSTALL" (find-package "QUICKLISP-QUICKSTART")))
		 :path (make-pathname :directory (pathname-directory ql-setup))))))

;;; Load all .asd files in the repos subdirectory.  The compile script puts
;;; several systems in there, because we are using versions that are 
;;; different from those in Quicklisp. (update: Can't just load the files apparently,
;;; have to add dirs to asdf:*central-registry*.  Blah.
(let* ((asds (directory (make-pathname :directory  (append *cache-dir* '( "repos" :wild-inferiors))
				       :name :wild
				       :type "asd")))
       (directories (remove-duplicates (mapcar #'pathname-directory asds) :test #'equal)))
  (dolist (d directories)
    (format t "~%~a added to asdf registry" d)
    (push (make-pathname :directory d) asdf:*central-registry*)))

;;; App can redefine this to do runtime initializations
(defun initialize-application (&key port)
  )

(defvar *root* "/app")			;this is always the app root on Heroku now?

;;; Default toplevel, app can redefine.
(defun heroku-toplevel ()
  (initialize-application :port (parse-integer (getenv "PORT")))
  (loop (sleep 60))			;sleep forever
  )

;;; Load the application from sources
(load (make-pathname :directory *build-dir* :defaults "heroku-compile.lisp"))

;;; Save the application as an image
(let ((app-file (format nil "~A/lispapp" (getenv "BUILD_DIR")))) ;must match path specified in bin/release
  (format t "Saving to ~A~%" app-file)
  (save-application app-file
		    :prepend-kernel t
		    :toplevel-function #'heroku-toplevel
		    ))
