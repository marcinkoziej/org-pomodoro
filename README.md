Org-Pomodoro
============

This adds very basic support for
[Pomodoro technique](http://www.pomodorotechnique.com/)
in Emacs' org-mode.

With default customs:

You can start a pomodoro for the task at point or select one of the
last tasks that you clocked time for. Each clocked-in pomodoro
starts a timer of 25 minutes and after each pomodoro a break timer of
5 minutes is started automatically. Every 4 breaks a long break is
started with 20 minutes. All values are customizable.

Installation
============

 Install from MELPA:

 * Add MELPA to your EMACS installation:

         (add-to-list 'package-archives
                      '("melpa" . "http://melpa.milkbox.net/packages/") t)

 * Install the package via `M-x package-install RET org-pomodoro`

Usage
=====

 1. Move point to a task as you would do with `org-clock-in`.
    Call `org-pomodoro` the task will be clocked-in.
 2. When there's time for break, the task will be `org-clock-out`'ed
 3. If you call `org-pomodoro` during a pomodoro, you'll be asked to reset
    a pomodoro.
 4. If you call `org-pomodoro` outside org-mode, you'll be presented
    with list of recent tasks, as `C-u org-clock-in` would.

License
=======

This file is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

This file is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Emacs; see the file COPYING.  If not, write to
the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.
