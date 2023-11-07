;; -*- lexical-binding: t; -*-

;; Instructions: run all of the tests at least once, to load any inner
;; tests. Then afterwards, run tests as normal.

(require 'ert)
(require 'ert-fixtures)

(ert-deftest test-define-fixture-basic ()
  "Run ERT-FIXTURE-DEFINE-FIXTURE through a simple example."
  (let ((fixture (efs-define-fixture ((x 1) (y 2))))
        (test-code (lambda ()
                     (should (= x 1))
                     (should (= y 2))
                     (should (not (boundp 'label))))))
    (should (funcall fixture test-code))))

(ert-deftest test-use-fixtures-basic ()
  "Do the previous test, but abbreviate it using our convenience
macro."
  (let ((fixture (efs-define-fixture ((x 1) (y 2)))))
    (efs-use-fixtures --test-use-fixture-basic (fixture)
      (should (= x 1))
      (should (= y 2))
      (should (not (boundp 'label))))))

(ert-deftest test-use-fixtures-inline-arg ()
  "Do the previous test, but evaluate the fixture directly in the
argument list."
  (efs-use-fixtures --test-use-fixtures-inline-arg ((efs-define-fixture ((x 1) (y 2))))
    (should (= x 1))
    (should (= y 2))
    (should (not (boundp 'label)))))

(ert-deftest test-merge-fixtures-basic ()
  "A merging of fixtures should be the union of the bindings of the
merged fixtures."
  (let* ((f1 (efs-define-fixture ((x 1) (y 2))))
         (f2 (efs-define-fixture ((label "FUN"))))
         (union (efs-merge-fixtures f1 f2))
         (test-code (lambda ()
                      (should (= x 1))
                      (should (= y 2))
                      (should (string= label "FUN")))))
    (should (funcall union test-code))))

(ert-deftest test-merge-fixtures-multiple ()
  "Explicitly test the merging of more than two fixtures at at time."
  (let* ((f1 (efs-define-fixture ((x 1) (y 2))))
         (f2 (efs-define-fixture ((label "FUN"))))
         (f3 (efs-define-fixture ((symbol 'strange))))
         (union (efs-merge-fixtures f1 f2 f3))
         (test-code (lambda ()
                      (should (= x 1))
                      (should (= y 2))
                      (should (string= label "FUN"))
                      (should (eq symbol 'strange)))))
    (should (funcall union test-code))))

(ert-deftest test-merge-fixtures-multiple-convenient ()
  "Perform the last test, but using our convenience macro."
  (let* ((f1 (efs-define-fixture ((x 1) (y 2))))
         (f2 (efs-define-fixture ((label "FUN"))))
         (f3 (efs-define-fixture ((symbol 'strange)))))
    (efs-use-fixtures --test-merge-fixtures-multiple-convenient (f1 f2 f3)
      (should (= x 1))
      (should (= y 2))
      (should (string= label "FUN"))
      (should (eq symbol 'strange)))))

(ert-deftest test-merge-fixtures-no-arguments ()
  "Merging no fixtures (nil input) should be legal."
  (efs-merge-fixtures))

(ert-deftest test-use-fixtures-no-arguments ()
  "Using no fixtures should be legal."
  :tags '(no-args)
  (efs-use-fixtures --test-merge-fixtures-multiple-convenient ())
  (message "I'm a normal test, and I ran."))

;; This is more of a demo than a test, since this has to be run twice:
;; once to load the inner test, and again to see the inner test
;; execute. (That is, using the '(tag no-args)' selector).
(ert-deftest test-use-fixtures-ert-goodies ()
  "Pass in ERT's auxiliary arguments to our macro."
  :tags '(no-args)
  (message "I'm the parent of a special test, and I ran.")
  (efs-use-fixtures --some-test ()
    "This is an enhanced test unit."
    :tags '(no-args)
    (message "I'm a special test, and I ran."))
  (efs-use-fixtures --another-test ()
    :expected-result :failed
    (time-forward -1)))

(ert-deftest test-define-fixture-evaluate-rhs ()
  "Make sure the rhs of each binding is evaluated."
  (let ((fixture (efs-define-fixture ((x (+ 1 1))))))
    (should (funcall fixture (lambda () (should (= x 2)))))))

;; Local Variables:
;; read-symbol-shorthands: (("efs-" . "ert-fixtures-"))
;; End:
