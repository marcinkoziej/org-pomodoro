;; BSD license
;; Author: Marcin Koziej
;; marcin at lolownia dot org
;; website: https://github.com/lolownia/org-pomodoro

(require 'timer)
(require 'org)

;;(add-to-list 'org-modules 'org-timer)
;;(setq org-timer-default-timer 25)

(defvar org-pomodoro-timer nil)
(defvar org-pomodoro-timer-start 0)
(defvar org-pomodoro-phase :none)
(defvar org-pomodoro-mode-line "")
(put 'org-pomodoro-mode-line 'risky-local-variable t)

(defface org-pomodoro-mode-line
  '((t (:foreground "tomato1")))
  "Org Pomodoro mode line color"
  :group 'faces)

(defun org-pomodoro-set-mode-line (enable?)
  (or global-mode-string (setq global-mode-string '("")))
  (if enable?
      (when (not (memq 'org-pomodoro-mode-line global-mode-string))
	(setq global-mode-string 
	      (append global-mode-string '(org-pomodoro-mode-line))))
      (setq global-mode-string (delq 'org-pomodoro-mode-line global-mode-string))
      )
  (force-mode-line-update)
  )

(defun org-pomodoro-seconds-elapsed ()
  (round (- (org-float-time (current-time))
       (org-float-time org-pomodoro-timer-start)
       ))
  )

(defun org-pomodoro-minutes-elapsed-text ()
  (let ((hms (org-timer-secs-to-hms 
	      (org-pomodoro-seconds-elapsed))))
    (substring hms (- (length hms) 5))
    ))

(defun org-pomodoro-update-mode-line ()
  (setq org-pomodoro-mode-line
	(cond 
	 ((eq org-pomodoro-phase :none) "")
	 ((eq org-pomodoro-phase :pomodoro)  
	  (propertize 
	   (format "(%s)" (org-pomodoro-minutes-elapsed-text)) 
	   'face 'org-pomodoro-mode-line) )
	 ((eq org-pomodoro-phase :break) 
	  (propertize (format "[break %s]" (org-pomodoro-minutes-elapsed-text))
		      'face 'org-pomodoro-update-mode-line))
	 ))
  (force-mode-line-update))

(defun org-pomodoro-kill ()
  (cancel-timer org-pomodoro-timer)
  (org-pomodoro-set-mode-line nil)
  (setq org-pomodoro-phase :none)
  )

(defun org-pomodoro-heartbeat ()
  (cond
   ((and (eq org-pomodoro-phase :none) org-pomodoro-timer) 
    (cancel-timer org-pomodoro-timer))
   ((eq org-pomodoro-phase :pomodoro) 
    (progn
;      (message "%s %s" org-pomodoro-phase (org-pomodoro-seconds-elapsed))
      (when (> (org-pomodoro-seconds-elapsed) (* 60 25))
	(message "Pomodoro completed! Time for a break!")
	(org-pomodoro-start :break)
	(run-hooks 'org-pomodoro-done-hook))
      (org-pomodoro-update-mode-line)))
   ((eq org-pomodoro-phase :break)
    (progn 
      (when (> (org-pomodoro-seconds-elapsed) (* 60 5))
	(message "Break is over, ready for another one?")
	(progn 
	  (org-pomodoro-kill)
	  (message "You've smashed the pomodoro")
	  ))
      (org-pomodoro-update-mode-line)))))


(defun org-pomodoro-start (what)
  (when org-pomodoro-timer 
    (cancel-timer org-pomodoro-timer))
  (org-pomodoro-set-mode-line t)
  (setq org-pomodoro-phase what
	org-pomodoro-timer-start (current-time)
	org-pomodoro-timer (run-with-timer 1 1 'org-pomodoro-heartbeat))
  )

;;; These names are not so great.
(defvar org-pomodoro-done-hook nil)
;;(defvar org-pomodoro-break-hook nil)

(add-hook 'org-pomodoro-done-hook
	  '(lambda () 
	     (call-interactively 'org-clock-out)))

(defun org-pomodoro (&optional phase)
  (interactive "p")
  (if (equal org-pomodoro-phase :none)
      (progn
	(if (eq major-mode 'org-mode)
	    (call-interactively 'org-clock-in)
	  (let ((current-prefix-arg '(4)))
	    (call-interactively 'org-clock-in))
	  )
	(org-pomodoro-start :pomodoro)
	)
    (if (y-or-n-p "You are already doing a pomodoro. Would You like to stop it?")
	(org-pomodoro-kill)
      (message "Alright, keep up the good work!")
	)))


;; (add-hook 'org-clock-in-hook '(lambda () 
;;       (if (not org-timer-last-timer) 
;; 	  (org-timer-set-timer "25"))))

(provide 'pomodoro)
