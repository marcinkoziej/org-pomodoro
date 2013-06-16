;;; org-pomodoro.el --- Pomodoro implementation for org-mode.

;; Author: Arthur Leonard Andersen <leoc.git@gmail.com>, Marcin Koziej <marcin at lolownia dot org>
;; Created: May 10, 2013
;; Version: 1.0
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
(require 'org-timer)
(require 'alert)

;; -----------------------------
;; Customizables
;; -----------------------------
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

;; POMODORO VALUES
(defcustom org-pomodoro-length 25
  "The length of a pomodoro in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-format "Pomodoro~%s"
  "The format of the mode line string during a long break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-sound
  (concat (file-name-directory load-file-name) "/resources/bell.wav")
  "The path to a sound file that´s to be played when a pomodoro was finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-killed-sound nil
  "The path to a sound file, that´s to be played when a pomodoro is killed."
  :group 'org-pomodoro
  :type 'file)

;; SHORT BREAK VALUES
(defcustom org-pomodoro-short-break-length 5
  "The length of a break in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-short-break-format "Short Break~%s"
  "The format of the mode line string during a long break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-short-break-sound
  (concat (file-name-directory load-file-name) "/resources/bell.wav")
  "The path to a sound file that´s to be played when a break was finished."
  :group 'org-pomodoro
  :type 'file)

;; LONG BREAK VALUES
(defcustom org-pomodoro-long-break-length 20
  "The length of a long break in minutes."
  :group 'org-pomodoro
  :type 'integer)

(defcustom org-pomodoro-long-break-format "Long Break~%s"
  "The format of the mode line string during a long break."
  :group 'org-pomodoro
  :type 'string)

(defcustom org-pomodoro-long-break-sound
  (concat (file-name-directory load-file-name) "/resources/bell_multiple.wav")
  "The path to a sound file that´s to be played when a long break is finished."
  :group 'org-pomodoro
  :type 'file)

(defcustom org-pomodoro-audio-player "/usr/bin/aplay"
  "Music player used to play sounds."
  :group 'org-pomodoro
  :type 'string)

;; -----------------------------
;; Hooks
;; -----------------------------
(defvar org-pomodoro-started-hook nil
  "Hooks run when a pomodoro is started.")

(defvar org-pomodoro-finished-hook nil
  "Hooks run when a pomodoro is finished.")

(defvar org-pomodoro-killed-hook nil
  "Hooks run when a pomodoro is killed.")

;; -----------------------------
;; Faces
;; -----------------------------
(defface org-pomodoro-mode-line
  '((t (:foreground "tomato1")))
  "Org Pomodoro mode line color"
  :group 'faces)

;; -----------------------------
;; Temporary Variables
;; -----------------------------
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

;; -----------------------------
;; Helper Functions
;; -----------------------------
(defun org-pomodoro-play-sound (sound)
  "Play an audio file specified by SOUND."
  (when (and org-pomodoro-play-sounds sound (executable-find org-pomodoro-audio-player))
    (call-process org-pomodoro-audio-player nil 0 nil (expand-file-name sound))))

(defun org-pomodoro-minutes ()
  "Return the current countdown value in minutes as string."
  (let ((hms (org-timer-secs-to-hms org-pomodoro-countdown)))
    (substring hms (- (length hms) 5))))


(defun org-pomodoro-update-mode-line ()
  "Set the modeline accordingly to the current state."
  (setq org-pomodoro-mode-line
        (if (not (eq org-pomodoro-state :none))
            (list
             "["
             (propertize (format (case org-pomodoro-state
                                   (:none "")
                                   (:pomodoro org-pomodoro-format)
                                   (:short-break org-pomodoro-short-break-format)
                                   (:long-break org-pomodoro-long-break-format))
                                 (org-pomodoro-minutes))
                         'face 'org-pomodoro-mode-line)
             "] ")
          nil))
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
        (case org-pomodoro-state
          (:pomodoro (org-pomodoro-finished))
          (:short-break (org-pomodoro-short-break-finished))
          (:long-break (org-pomodoro-long-break-finished))))))
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
        org-pomodoro-countdown (case state
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


;; -----------------------------
;; Handlers for pomodoro events.
;; -----------------------------
(defun org-pomodoro-finished ()
  "Is invoked when a pomodoro was finished successfully.
This may send a notification, play a sound and start a pomodoro break."
  (org-clock-out nil t)
  (org-pomodoro-play-sound org-pomodoro-sound)
  (setq org-pomodoro-count (+ org-pomodoro-count 1))
  (if (> org-pomodoro-count org-pomodoro-long-break-frequency)
      (progn
        (alert-message-notify "Pomodoro completed! Time for a long break.")
        (org-pomodoro-start :long-break))
    (progn
      (alert-message-notify "Pomodoro completed! Time for a short break.")
      (org-pomodoro-start :short-break)))
  (run-hooks 'org-pomodoro-finished-hook)
  (org-pomodoro-update-mode-line))


(defun org-pomodoro-killed ()
  "Is invoked when a pomodoro was killed.
This may send a notification, play a sound and adds log."
  (alert-message-notify "One does not simply kill a pomodoro!")
  (org-clock-cancel)
  (org-pomodoro-reset)
  (run-hooks 'org-pomodoro-killed-hook)
  (org-pomodoro-update-mode-line))


(defun org-pomodoro-short-break-finished ()
  "Is invoked when a break is finished.
This may send a notification and play a sound."
  (alert-message-notify "Short break finished. Ready for another pomodoro?")
  (org-pomodoro-play-sound org-pomodoro-short-break-sound)
  (org-pomodoro-reset))


(defun org-pomodoro-long-break-finished ()
  "Is invoked when a long break is finished.
This may send a notification and play a sound."
  (alert-message-notify "Long break finished. Ready for another pomodoro?")
  (org-pomodoro-play-sound org-pomodoro-long-break-sound)
  (setq org-pomodoro-count 0)
  (org-pomodoro-reset))

;; ---------------------------------------
;; The actual function to handle pomodoros
;; ---------------------------------------
(defun org-pomodoro ()
  "Start a new pomodoro or stop the current one.
When no timer is running for `org-pomodoro` a new pomodoro is started and
the current task is clocked in.  Otherwise EMACS will ask whether we´d like to
kill the current timer, this may be a break or a running pomodoro."
  (interactive)
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
