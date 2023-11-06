;; -*- lexical-binding: t; -*-
(defun efs--officialize-macro-names ()
  "Give syntax highlighting to the names of macros."
  ;; This code is adapted from the ERT source.
  (font-lock-add-keywords
   nil
   '(("(\\(\\<efs-use-fixture\\)\\>\\s *\\(\\(?:\\sw\\|\\s_\\)+\\)?"
      (1 font-lock-keyword-face nil t)
      (2 font-lock-function-name-face nil t)))))

(add-hook 'emacs-lisp-mode-hook #'efs--officialize-macro-names)

(defmacro efs-define-fixture (spec)
  "Define a test fixture.

The returned fixture is a function which dynamically binds a set
of variables according to SPEC. This function accepts a test-code
function as its sole argument. In this test-code function, the
bindings introduced in SPEC are visible because of having been
dynamically let-bound in the fixture-function.

For example, you might be testing something geometry related, and
would like to write tests for the origin point. So you'd like a
common fixture for those tests:

(defvar origin-fixture (ert-fixtures-define-fixture ((x 0) (y 0)))
  \"A fixture that uses the origin as its main point.\")

You'd then use the macro 'ert-fixtures-use-fixture' to apply this
fixture to the test code you'd otherwise write with
'ert-deftest':

(ert-fixtures-use-fixture origin-test-1 (origin-fixture)
  (should (zerop x))
  (should (zerop y)))

See 'elisp#Dynamic Binding'."
  ;; I'm not sure if this should be the debug specification. See
  ;; 'elisp#Specification Examples', in particular the
  ;; 'def-edebug-spec' for 'let'.
  (declare (indent 1) (debug ((&rest (gate symbolp def-form)))))
  (let ((vars (mapcar #'car spec)))
    `(lambda (body)
       ,@(mapcar (lambda (var) `(defvar ,var)) vars)
       (let* ,spec (funcall body)))))

(defmacro efs-use-fixture (name fixture &rest body)
  "Define the ERT test named NAME, except that the body of the test
is now run under the given fixture.

See documentation for 'ert-fixtures-define-fixture' for an
example."
  (declare (indent defun) (debug ((name listp) def-body)))
  `(ert-deftest ,name ()
     (funcall ,(car fixture) (lambda () ,@body))))

(provide 'ert-fixtures)

;; Local Variables:
;; read-symbol-shorthands: (("efs-" . "ert-fixtures-"))
;; End:
