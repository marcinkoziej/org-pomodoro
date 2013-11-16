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

;;; org-pomodoro-tests.el ends here
