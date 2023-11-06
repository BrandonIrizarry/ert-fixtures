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

ORIGIN-FIXTURE is a lambda that takes in test code as an
argument (in the form of a function), where x and y will be
defined via dynamic scope. That is, the test code already expects
to use these variables. Example use:

(ert-deftest origin-test-1 ()
  (funcall origin-fixture
           (lambda ()
              (should (zerop x))
              (should (zerop y)))))

Since this still looks a bit artifical, the convenience macro
'ert-fixtures-use-fixture' is provided to let you apply this
fixture directly to your test code, setting up the required ERT
test for you:

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
