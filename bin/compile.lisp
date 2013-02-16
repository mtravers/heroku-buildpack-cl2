(in-package :cl-user)

(require :asdf)

(defvar *build-dir* (pathname (concatenate 'string (asdf::getenv "BUILD_DIR") "/")))
(defvar *cache-dir* (pathname (concatenate 'string (asdf::getenv "CACHE_DIR") "/")))
(defvar *buildpack-dir* (pathname (concatenate 'string (asdf::getenv "BUILDPACK_DIR") "/")))

(defun require-quicklisp ()
  (let ((ql-setup (merge-pathnames "quicklisp/setup.lisp" *build-dir*)))
    (if (probe-file ql-setup)
        (load ql-setup)
        (progn
          (load (merge-pathnames "bin/quicklisp.lisp" *buildpack-dir*))
          (funcall (read-from-string "quicklisp-quickstart:install")
                   :path (make-pathname :directory (pathname-directory ql-setup)))))))

;;; stacktrace printing, copy/pasted from the ql-test by Fare:
;;; ssh://common-lisp.net/home/frideau/git/ql-test.git
(defun print-backtrace (out)
  "Print a backtrace (implementation-defined)"
  (declare (ignorable out))
  #+clozure (let ((*debug-io* out))
	      (ccl:print-call-history :count 100 :start-frame-number 1)
	      (finish-output out))
  #+sbcl
  (sb-debug:backtrace most-positive-fixnum out))

(defun call-with-ql-test-context (thunk)
  (block nil
    (handler-bind (((or error serious-condition)
                     (lambda (c)
                       (format *error-output* "~%~A~%" c)
                       (print-backtrace *error-output*)
                       (format *error-output* "~%~A~%" c)
                       (return nil))))
      (funcall thunk))))

(defmacro with-ql-test-context (() &body body)
  `(call-with-ql-test-context #'(lambda () ,@body)))

;;; Load the application compile script
(with-ql-test-context ()
  (load (merge-pathnames "heroku-compile.lisp" *build-dir*)))
