Org-Pomodoro
============

This adds very basic support for
[Pomodoro technique](http://www.pomodorotechnique.com/)
in Emacs' org-mode.

With default customs:

You can start a pomodoro for the task at point or select one of the
last tasks that you has clocked time for. Each clocked in pomodoro
starts a timer of 25 minutes and after each pomodoro a break timer of
5 mionutes is started automatically. Every 4 breaks a long break is
started with 20 minutes. All values are customizable.

Installation
===========

 Install from MEPLA:

 * Add MELPA to your EMACS installation:

         (add-to-list 'package-archives
                      '("melpa" . "http://melpa.milkbox.net/packages/") t)

 * Install the package via `M-x package-install RET org-pomodoro`

Usage
=====

 1. Move point to a task as you would do with `org-clock-in`.
    Call `org-pomodoro` The task will be clocked-in.
 2. When there's time for break, the task will be `org-clock-out`'ed
 3. If You `org-pomodoro` during a pomodoro, You'll be asked to reset
    a pomodoro.
 4. If You call `org-pomodoro` outside org-mode, You'll be presented
    with list of recent tasks, as `C-u org-clock-in` would.
