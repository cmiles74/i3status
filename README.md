This project provides an i3 status bar and is written in Racket. It's
not super clever but it works. You may want to use this as the basis
for your own custom status bar.

The i3 status bar (i3bar) works by reading JSON data and using that
data to populate the status bar, the format of this JSON data
comprises the
[i3bar Input Protocol](http://i3wm.org/docs/i3bar-protocol.html). When
i3 starts, it can be configured to also start the script that will
provide this data by writing JSON data to standard out. In your i3
configuration file, configure `status_command` to start i3status.

```shell
bar {
    status_command ~/Projects/i3status/i3status-start.sh
 	tray_output primary
}
```

An example shell script is included, called `i3-status-start.sh`. It
requires this module and then starts writing JSON data to standard
out.

```racket
(require (planet cmiles74/i3status:1:4))

(start-status (lambda () (list (mpd)             ;; music player daemon status
                               (mail)            ;; notmuch inbox status
                               (battery-charge)  ;; current battery charge
                               (cpu-time)        ;; CPU load over the last 250ms
                               (system-time))))  ;; current system time
```

In all cases, these modules have really only been tested on my
workstation. I've added optional parameters to the functions that are
most likely to need customization in order to work somewhere else. For
instance, the path you your laptop's battery status might live
somewhere else.

```racket
(start-status (lambda () (list (battery-charge
                                #:battery-path "/sys/class/power_supply/BAT0"))))
```

You can Check out the documentation on the
[Racket Planet](http://planet.racket-lang.org/display.ss?package=i3status.plt&owner=cmiles74)
for more informatio
