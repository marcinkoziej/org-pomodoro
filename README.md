
Org-Pomodoro
============

This adds very basic support for
[Pomodoro technique](http://www.pomodorotechnique.com/)
in Emacs' org-mode.

Instalation
===========
 1. Put `org-pomodoro.el` somewhere emacs can find it.
 2. `M-x customize-variable` `org-modules`
 3. add `org-pomodoro` to external modules list.
 4. Optionally bind `org-pomodoro` command to a key

Usage
=====

 1. Move point to a task as You would do with `org-clock-in`. Call
`org-pomodoro` The task will be clocked-in.
 2. When there's time for break, the task will be `org-clock-out`'ed
 3. If You `org-pomodoro` during a pomodoro, You'll be asked to reset
 a pomodoro.
 4. If You call `org-pomodoro` outside org-mode, You'll be presented
 with list of recent tasks, as `C-u org-clock-in` would.
 
