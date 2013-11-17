;;; org-pomodoro.el --- Pomodoro implementation for org-mode.

;; Author: Arthur Leonard Andersen <leoc.git@gmail.com>, Marcin Koziej <marcin at lolownia dot org>
;; Created: May 10, 2013
;; Version: 1.3.1
;; Package-Requires: ((alert "0.5.10"))

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Org-pomodoro introduces an easy way to clock time in org-mode with
;; the pomodoro technique.  You can clock into tasks with starting a
;; pomodoro time automatically.  Each finished pomodoro is followed by
;; a break timer.  If you completed 4 pomodoros in a row the break is
;; longer that the shorter break between each pomodoro.
;;
;; For a full explanation of the pomodoro technique, have a look at:
;;   http://www.pomodorotechnique.com

;;; Code:

(require 'timer)
(require 'org)
(require 'org-clock)
(require 'org-timer)
(require 'alert)

;;; Custom Variables

(defgroup org-pomodoro nil
  "Org pomodoro customization"
  :tag "Org Pomodoro"
  :group 'org-progress)

(defcustom org-pomodoro-long-break-frequency 4
  "The maximum number of pomodoros until a long break is started."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-play-sounds t
  "Determines whether sounds are played or not."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-play-ticking-sounds nil
  "Determines whether ticking clock sounds are played or not."
  :group 'org-pomodoro
  :type 'boolean)

;; Pomodoro Values

(defcustom org-pomodoro-length 25
  "The length of a pomodoro in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-time-format "%.2m:%.2s"
  "Defines the format of the time representation in the modeline."
  :group 'org-pomodoro
  :type 'boolean)

(defcustom org-pomodoro-format "Pomodoro~%s"
  "The format of the mode line string during a long break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-ticking-sound (when load-file-name
                                        (concat (file-name-directory load-file-name) "resources/tick.wav"))
  "The path to a sound file that´s to be played while a pomodoro is running."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-sound (when load-file-name
                                (concat (file-name-directory load-file-name) "resources/bell.wav"))
  "The path to a sound file that´s to be played when a pomodoro was finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-killed-sound nil
  "The path to a sound file, that´s to be played when a pomodoro is killed."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-expiry-time 120
  "The time in minutes for which a pomodoro group is valid.
If you do not clock in for this period of time you will be prompted
whether to reset the pomodoro count next time you call `org-pomodoro'."
  :group 'org-pomodoro
  :type 'integer)

;; Break Values

(defcustom org-pomodoro-short-break-length 5
  "The length of a break in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-short-break-format "Short Break~%s"
  "The format of the mode line string during a long break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-short-break-sound (when load-file-name
                                            (concat (file-name-directory load-file-name)
                                                    "resources/bell.wav"))
  "The path to a sound file that´s to be played when a break was finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-long-break-length 20
  "The length of a long break in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-long-break-format "Long Break~%s"
  "The format of the mode line string during a long break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-long-break-sound (when load-file-name
                                           (concat (file-name-directory load-file-name)
                                                   "resources/bell_multiple.wav"))
  "The path to a sound file that´s to be played when a long break is finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-audio-player (or (executable-find "aplay")
                                         (executable-find "afplay"))
  "Music player used to play sounds."
  :group 'org-pomodoro
  :type 'string)

;; Hooks

(defvar org-pomodoro-started-hook nil
  "Hooks run when a pomodoro is started.")

(defvar org-pomodoro-finished-hook nil
  "Hooks run when a pomodoro is finished.")

(defvar org-pomodoro-killed-hook nil
  "Hooks run when a pomodoro is killed.")

(defvar org-pomodoro-break-finished-hook nil
  "Hook run after any break has finished.
Run before a break's specific hook.")

(defvar org-pomodoro-long-break-finished-hook nil
  "Hooks run when a long break is finished.")

(defvar org-pomodoro-short-break-finished-hook nil
  "Hooks run when short break is finished.")

;; Faces

(defface org-pomodoro-mode-line
  '((t (:foreground "tomato1")))
  "Face of a pomodoro in the modeline."
  :group 'faces)

(defface org-pomodoro-mode-line-break
  '((t (:foreground "#2aa198")))
  "Face of a pomodoro break in the modeline ."
  :group 'faces)

;; Temporary Variables

(defvar org-pomodoro-mode-line "")
(put 'org-pomodoro-mode-line 'risky-local-variable t)

(defvar org-pomodoro-timer nil
  "The timer while a pomodoro or a break.")

(defvar org-pomodoro-countdown 0
  "The actual countdown value for a phase in seconds.")

(defvar org-pomodoro-state :none
  "The current state of `org-pomodoro`.
It changes to :pomodoro when starting a pomodoro and to :longbreak
or :break when starting a break.")

(defvar org-pomodoro-count 0
  "The number of pomodoros since the last long break.")

(defvar org-pomodoro-last-clock-in nil
  "The last time the pomodoro was set.")

;;; Internal

;; Helper Functions

(defun org-pomodoro-active-p ()
  "Retrieve whether org-pomodoro is active or not."
  (not (eq org-pomodoro-state :none)))

(defun org-pomodoro-expires-p ()
  "Return true when the last clock-in was more than `org-pomodoro-expiry-time`."
  (let* ((current-time-secs (nth 1 (current-time)))
         (last-clock-in-secs (nth 1 org-pomodoro-last-clock-in))
         (delta-minutes (/ (- current-time-secs last-clock-in-secs) 60)))
    (< org-pomodoro-expiry-time delta-minutes)))

(defun org-pomodoro-play-sound (type)
  "Play an audio file specified by TYPE (:pomodoro, :short-break, :long-break)."
  (let ((sound (cl-case type
                 (:pomodoro org-pomodoro-sound)
                 (:short-break org-pomodoro-short-break-sound)
                 (:long-break org-pomodoro-long-break-sound)
                 (t (error "Unknown org-pomodoro sound: %S" type)))))
    (when (and org-pomodoro-play-sounds sound org-pomodoro-audio-player)
      (call-process org-pomodoro-audio-player nil 0 nil (expand-file-name sound)))))

(defun org-pomodoro-format-seconds ()
  "Format the countdown with the format specified in org-pomodoro-time-format."
  (format-seconds org-pomodoro-time-format org-pomodoro-countdown))

(defun org-pomodoro-update-mode-line ()
  "Set the modeline accordingly to the current state."
  (let ((s (cl-case org-pomodoro-state
             (:pomodoro
              (propertize org-pomodoro-format 'face 'org-pomodoro-mode-line))
             (:short-break
              (propertize org-pomodoro-short-break-format
                          'face 'org-pomodoro-mode-line-break))
             (:long-break
              (propertize org-pomodoro-long-break-format
                          'face 'org-pomodoro-mode-line-break)))))
    (setq org-pomodoro-mode-line
          (if (org-pomodoro-active-p)
              (list "[" (format s (org-pomodoro-format-seconds)) "] ")
            nil)))
  (force-mode-line-update))

(defun org-pomodoro-kill ()
  "Kill the current timer, reset the phase and update the modeline."
  (org-pomodoro-reset)
  (org-pomodoro-killed))

(defun org-pomodoro-tick ()
  "A callback that is invoked by the running timer each second.
It checks whether we reached the duration of the current phase, when 't it
invokes the handlers for finishing."
  (if (and (equal org-pomodoro-state :none) org-pomodoro-timer)
      (org-pomodoro-reset)
    (progn
      (setq org-pomodoro-countdown (- org-pomodoro-countdown 1))
      (when (< org-pomodoro-countdown 1)
        (cl-case org-pomodoro-state
          (:pomodoro (org-pomodoro-finished))
          (:short-break (org-pomodoro-short-break-finished))
          (:long-break (org-pomodoro-long-break-finished))))))
  (when (and org-pomodoro-play-sounds
             org-pomodoro-play-ticking-sounds
             org-pomodoro-audio-player
             org-pomodoro-ticking-sound
             (eq :pomodoro org-pomodoro-state))
        (call-process org-pomodoro-audio-player nil 0 nil
                      (expand-file-name org-pomodoro-ticking-sound)))
  (org-pomodoro-update-mode-line))

(defun org-pomodoro-start (&optional state)
  "Start the `org-pomodoro` timer.
The argument STATE is optional.  The default state is `:pomodoro`."
  (when org-pomodoro-timer (cancel-timer org-pomodoro-timer))

  ;; add the org-pomodoro-mode-line to the global-mode-string
  (unless global-mode-string (setq global-mode-string '("")))
  (unless (memq 'org-pomodoro-mode-line global-mode-string)
    (setq global-mode-string (append global-mode-string
                                     '(org-pomodoro-mode-line))))

  (unless state (setq state :pomodoro))
  (setq org-pomodoro-state state
        org-pomodoro-countdown (cl-case state
                                 (:pomodoro (* 60 org-pomodoro-length))
                                 (:short-break (* 60 org-pomodoro-short-break-length))
                                 (:long-break (* 60 org-pomodoro-long-break-length)))
        org-pomodoro-timer (run-with-timer t 1 'org-pomodoro-tick))
  (when (eq org-pomodoro-state :pomodoro)
    (run-hooks 'org-pomodoro-started-hook))
  (org-pomodoro-update-mode-line))

(defun org-pomodoro-reset ()
  "Reset the org-pomodoro state."
  (when org-pomodoro-timer
    (cancel-timer org-pomodoro-timer))
  (setq org-pomodoro-state :none
        org-pomodoro-countdown 0)
  (org-pomodoro-update-mode-line))

(defun org-pomodoro-notify (title message)
  "Send a notification with TITLE and MESSAGE using `alert'."
  (alert message :title title :category 'org-pomodoro))

;; Handlers for pomodoro events.

(defun org-pomodoro-finished ()
  "Is invoked when a pomodoro was finished successfully.
This may send a notification, play a sound and start a pomodoro break."
  (org-clock-out nil t)
  (org-pomodoro-play-sound :pomodoro)
  (setq org-pomodoro-count (+ org-pomodoro-count 1))
  (if (zerop (mod org-pomodoro-count org-pomodoro-long-break-frequency))
      (org-pomodoro-start :long-break)
    (org-pomodoro-start :short-break))
  (org-pomodoro-notify "Pomodoro completed!" "Time for a break.")
  (run-hooks 'org-pomodoro-finished-hook)
  (org-pomodoro-update-mode-line))

(defun org-pomodoro-killed ()
  "Is invoked when a pomodoro was killed.
This may send a notification, play a sound and adds log."
  (org-pomodoro-notify "Pomodoro killed." "One does not simply kill a pomodoro!")
  (when (org-clocking-p)
    (org-clock-cancel))
  (org-pomodoro-reset)
  (run-hooks 'org-pomodoro-killed-hook)
  (org-pomodoro-update-mode-line))

(defun org-pomodoro-short-break-finished ()
  "Is invoked when a break is finished.
This may send a notification and play a sound."
  (org-pomodoro-notify "Short break finished." "Ready for another pomodoro?")
  (org-pomodoro-play-sound :short-break)
  (run-hooks 'org-pomodoro-break-finished-hook 'org-pomodoro-short-break-finished-hook)
  (org-pomodoro-reset))

(defun org-pomodoro-long-break-finished ()
  "Is invoked when a long break is finished.
This may send a notification and play a sound."
  (org-pomodoro-notify "Long break finished." "Ready for another pomodoro?")
  (org-pomodoro-play-sound :long-break)
  (run-hooks 'org-pomodoro-break-finished-hook 'org-pomodoro-long-break-finished-hook)
  (org-pomodoro-reset))

;;;###autoload
(defun org-pomodoro ()
  "Start a new pomodoro or stop the current one.
When no timer is running for `org-pomodoro` a new pomodoro is started and
the current task is clocked in.  Otherwise EMACS will ask whether we´d like to
kill the current timer, this may be a break or a running pomodoro."
  (interactive)

  (when (and org-pomodoro-last-clock-in
             org-pomodoro-expiry-time
             (org-pomodoro-expires-p)
             (y-or-n-p "Reset pomodoro count? "))
    (setq org-pomodoro-count 0))
  (setq org-pomodoro-last-clock-in (current-time))

  (if (equal org-pomodoro-state :none)
      (progn
        (cond
         ((eq major-mode 'org-mode)
          (call-interactively 'org-clock-in))
         ((eq major-mode 'org-agenda-mode)
          (org-with-point-at (org-get-at-bol 'org-hd-marker)
            (call-interactively 'org-clock-in)))
         (t (let ((current-prefix-arg '(4)))
              (call-interactively 'org-clock-in))))
        (org-pomodoro-start :pomodoro))
    (if (y-or-n-p "There is already a running timer.  Would you like to stop it? ")
        (org-pomodoro-kill)
      (message "Alright, keep up the good work!"))))

(provide 'org-pomodoro)

;;; org-pomodoro.el ends here
