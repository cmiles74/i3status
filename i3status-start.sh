#!/usr/local/bin/racket
#lang racket

;;
;; Provides a stream of continuous i3bar protocol compatible data that may be
;; used to populate the status bar area in the i3 window manager.
;;
(require (planet cmiles74/i3status:1:1))

(start-status (lambda () (list (mpd)             ;; music player daemon status
	      	      	       (mail)            ;; notmuch inbox status
			       (battery-charge)  ;; current battery charge
			       (cpu-time)        ;; CPU load over the last 250ms
			       (system-time))))  ;; current system time
