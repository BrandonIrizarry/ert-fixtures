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

(oclosure-define efs-fixture
  "A closure representing test code.

The alist consisting of this fixture's dynamically-bound
variables (called here 'bindings'), is accessible via
'efs-fixture--bindings'."
  (bindings :mutable t))

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
  ;; Notes:
  ;;
  ;; 1. VARS is used for forming the defvar expressions.
  ;;
  ;; 2. For OCLOSURE-LAMBDA, SPEC needs to be evaluated, but quoted
  ;; again so that it isn't confused for a function call.
  (let ((vars (mapcar #'car spec)))
    `(oclosure-lambda (efs-fixture (bindings (quote ,spec)))
         (body)
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

(defun efs-merge-fixtures (f1 f2)
  "Merge two existing fixtures F1 and F2.

That is, return a new fixture whose bindings are the union of F1
and F2's bindings. For example, we already have simple fixtures
defined for our initial, more basic tests, but then we'd like to
be able to reuse these for more complex tests, rather than
repeat the same bindings across various fixtures.

If F2 overshadows one of F1's bindings, then that binding takes
precedence, per the behavior of 'let*'."
  (let ((bindings1 (efs-fixture--bindings f1))
        (bindings2 (efs-fixture--bindings f2)))
    (cl-symbol-macrolet ((exp `(macroexpand-1 (efs-define-fixture ,(append bindings1 bindings2)))))
      (eval (macroexpand-1 exp) t))))

(provide 'ert-fixtures)

;; Local Variables:
;; read-symbol-shorthands: (("efs-" . "ert-fixtures-"))
;; End:
