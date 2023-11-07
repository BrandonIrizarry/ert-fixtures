;; -*- lexical-binding: t; -*-

(require 'ert-fixtures)

(ert-deftest test-define-fixture-basic ()
  "Run ERT-FIXTURE-DEFINE-FIXTURE through a simple example."
  (let ((fixture (efs-define-fixture ((x 1) (y 2))))
        (test-code (lambda ()
                     (should (= x 1))
                     (should (= y 2))
                     (should (not (boundp 'label))))))
    (should (funcall fixture test-code))))

(ert-deftest test-use-fixture-basic ()
  "Do the previous test, but abbreviate it using our convenience
macro."
  (let ((fixture (efs-define-fixture ((x 1) (y 2)))))
    (efs-use-fixture --test-use-fixture-basic (fixture)
      (should (= x 1))
      (should (= y 2))
      (should (not (boundp 'label))))))

(ert-deftest test-use-fixture-inline-arg ()
  "Do the previous test, but evaluate the fixture directly in the
argument list."
  (efs-use-fixture --test-use-fixture-inline-arg ((efs-define-fixture ((x 1) (y 2))))
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
         (f3 (efs-define-fixture ((symbol 'strange))))
         (union (efs-merge-fixtures f1 f2 f3))
         (test-code (lambda ()
                      (should (= x 1))
                      (should (= y 2))
                      (should (string= label "FUN"))
                      (should (eq symbol 'strange)))))
    (efs-use-fixture --test-merge-fixtures-multiple-convenient ())
    (should (funcall union test-code))))

;; Local Variables:
;; read-symbol-shorthands: (("efs-" . "ert-fixtures-"))
;; End:
