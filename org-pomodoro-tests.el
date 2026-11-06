;;; org-pomodoro-tests.el --- Org-pomodoro tests

;;; Commentary:
;; All tests for org-pomodoro.el.

;;; Code:

(ert-deftest org-pomodoro-expires-when-last-clockin-too-old ()
  "Test the return value is true when org-pomodoro-last-clock-in is too old."
  (let ((org-pomodoro-last-clock-in '(0 120 0 0))
        (org-pomodoro-expiry-time 120))
    (should (equal t (org-pomodoro-expires-p)))))

(ert-deftest org-pomodoro-expires-when-last-clockin-is-new ()
  "Test the return value is false when org-pomodoro-last-clock-in is new."
  (let ((org-pomodoro-last-clock-in (current-time))
        (org-pomodoro-expiry-time 120))
    (should (equal nil (org-pomodoro-expires-p)))))

(defmacro org-pomodoro-test-with-task (&rest body)
  "Evaluate BODY at a temporary Org task."
  `(with-temp-buffer
     (org-mode)
     (insert "* TODO Example\n")
     (goto-char (point-min))
     ,@body))

(defun org-pomodoro-test-effort-minutes ()
  "Return the current task's recorded effort in minutes."
  (org-duration-to-minutes (org-entry-get nil "Effort")))

(ert-deftest org-pomodoro-effort-uses-configured-length ()
  (org-pomodoro-test-with-task
   (let ((org-pomodoro-length 30))
     (org-pomodoro-set-effort-in-pomodoros 2)
     (should (= 60 (org-pomodoro-test-effort-minutes))))))

(ert-deftest org-pomodoro-effort-allows-fractional-count ()
  (org-pomodoro-test-with-task
   (let ((org-pomodoro-length 30))
     (org-pomodoro-set-effort-in-pomodoros 0.5)
     (should (= 15 (org-pomodoro-test-effort-minutes))))))

(ert-deftest org-pomodoro-effort-accepts-prefix-arguments ()
  (dolist (prefix '(3 (4)))
    (org-pomodoro-test-with-task
     (let ((org-pomodoro-length 25)
           (current-prefix-arg prefix))
       (call-interactively #'org-pomodoro-set-effort-in-pomodoros)
       (should (= (* (prefix-numeric-value prefix) 25)
                  (org-pomodoro-test-effort-minutes)))))))

(ert-deftest org-pomodoro-effort-prompts-without-prefix ()
  (org-pomodoro-test-with-task
   (let ((org-pomodoro-length 25)
         (current-prefix-arg nil))
     (cl-letf (((symbol-function 'read-number)
                (lambda (&rest _) 2)))
       (call-interactively #'org-pomodoro-set-effort-in-pomodoros))
     (should (= 50 (org-pomodoro-test-effort-minutes))))))

(ert-deftest org-pomodoro-effort-rejects-invalid-counts ()
  (dolist (count '(0 -1 "2" nil))
    (org-pomodoro-test-with-task
     (org-set-effort nil "0:45")
     (should-error (org-pomodoro-set-effort-in-pomodoros count)
                   :type 'user-error)
     (should (= 45 (org-pomodoro-test-effort-minutes))))))

(ert-deftest org-pomodoro-effort-rejects-invalid-length ()
  (dolist (length '(0 -25 nil))
    (org-pomodoro-test-with-task
     (let ((org-pomodoro-length length))
       (should-error (org-pomodoro-set-effort-in-pomodoros 2)
                     :type 'user-error)))))

(ert-deftest org-pomodoro-effort-preserves-default-binding ()
  (should-not (eq (lookup-key org-mode-map (kbd "C-c C-x e"))
                  #'org-pomodoro-set-effort-in-pomodoros)))

;;; org-pomodoro-tests.el ends here
