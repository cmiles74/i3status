#!/usr/local/bin/racket
#lang racket

(require (planet cmiles74/i3status:1:1))

(start-status (lambda () (list (mpd)
	      	      	       (mail)
			       (battery-charge)
			       (cpu-time)
			       (system-time))))
