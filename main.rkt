#lang racket

;;
;; Provides functions for specifying and creating an i3bar protocol compatible
;; status bar.
;;
(provide (except-out (all-defined-out) header write-data write-header write-footer
                     write-closing sample-status uptime cpu-time-sample))

(require srfi/19)
(require srfi/48)
(require json)
(require racket/file)

;; Colors for the status output, these are roughly compatible with the
;; Solarized theme.
(define solarized
  #hash((blue     . "#268bd2")
        (yellow   . "#b58900")
        (red      . "#dc322f")
        (magenta  . "#d33682")))

;;
;; Returns the current time formatted all pretty style.
;;
(define (system-time #:full-format  [full-format "~Y-~m-~d ~k:~M"]
                     #:short-format [short-format "~k:~M"])
  (let ([map-out (make-hash)])
    (hash-set! map-out 'full_text (date->string (current-date) full-format))
    (hash-set! map-out 'short_text (date->string (current-date) short-format))
    map-out))

;;
;; Returns a map with the current idle and uptime. The keys in this map
;; are 'uptime and 'idle.
;;
(define (uptime)
  (let* [(uptime-raw (file->string "/proc/uptime"))
         (uptime-data (regexp-split #px"[[:space:]]+" uptime-raw))
         (uptime-map (make-hash))]
    (hash-set! uptime-map 'uptime (string->number (first uptime-data)))
    (hash-set! uptime-map 'idle (string->number (second uptime-data)))
    uptime-map))

;;
;; Computes the current CPU load (in jiffies).
;;
(define (cpu-time-sample)
  (apply +
         (map (lambda (dir-in)
                (if (file-exists? dir-in)
                    (let [(stat-data (regexp-split #px"[[:space:]]+" (file->string dir-in)))]
                      (+ (string->number (list-ref stat-data 13))
                         (string->number (list-ref stat-data 14))))
                    0))
              (map (lambda (pid-in) (string-append "/proc/" (first pid-in) "/stat"))
                   (filter (lambda (pid-in) (not (false? pid-in)))
                           (map (lambda (file-name)
                                  (regexp-match #px"[[:digit:]]+" file-name))
                                (directory-list "/proc")))))))
;;
;; Returns a map with the current CPU load expressed as a percentage of the
;; overall amount of CPU time available, this map is formatted such that it is
;; compatible with the i3bar protocl. This value is computed by collecting
;; two samples of the current CPU load gathered 250ms apart.
;;
(define (cpu-time #:color-scheme [color-scheme solarized]
                  #:delay        [delay 0.25])
  (let [(map-out (make-hash))
        (uptime-1 (hash-ref (uptime) 'uptime))
        (sample-1 (cpu-time-sample))]
    (sleep delay)
    (let [(uptime-2 (hash-ref (uptime) 'uptime))
          (sample-2 (cpu-time-sample))]
      (let [(avg-time (/ (* 1
                            (/ (- sample-2 sample-1)
                               (- uptime-2 uptime-1)))
                         2))]

        (hash-set! map-out 'full_text (string-append "CPU: "
                                                     (format "~6,2F" avg-time)))

        (cond
         [(< 100 avg-time) (lambda () ((hash-set! map-out 'color "#d33682")
                                       (hash-set! map-out 'urgent true)))]
         [(< 75 avg-time) (hash-set! map-out 'color
                                     (hash-ref color-scheme 'red))]
         [(< 50 avg-time) (hash-set! map-out 'color
                                     (hash-ref color-scheme 'yellow))])
        map-out))))

;;
;; Returns a map with the status of the charge of battery 0. This map is
;; formatted such that it is compatible with the i3bar protocol.
;;
(define (battery-charge #:color-scheme [color-scheme solarized]
                        #:battery-path [battery-path "/sys/class/power_supply/BAT0"])
  (let [(map-out (make-hash))
        (description (string-trim (file->string (string-append battery-path
                                                               "/status"))))
        (charge-now (string->number (string-trim
                                  (file->string (string-append battery-path
                                                               "/charge_now")))))
        (charge-full (string->number (string-trim
                                   (file->string (string-append battery-path
                                                                "/charge_full_design")))))]
    (let [(charge-pct (* 100 (/ charge-now charge-full)))]
          (hash-set! map-out 'full_text
               (string-append "BATTERY: " description " " (format "~6,2F" charge-pct)))
          (hash-set! map-out 'short_text
               (string-append "BATTERY: " (cond [(string=? "Full" description) "F"]
                                                [(string=? "Charging" description) "C"]
                                                [(string=? "Discharging" description) "D"])
                              " " (format "~6,2F" charge-pct)))

          (cond
           [(string=? "Discharging" description) (hash-set! map-out 'color
                                                            (hash-ref color-scheme 'blue))]
           [(> 0.4 charge-pct) (hash-set! map-out 'color
                                          (hash-ref color-scheme 'magenta))]
           [(> 0.25 charge-pct) (lambda () ((hash-set! map-out 'color
                                                       (hash-ref color-scheme 'red))
                                            (hash-set! map-out 'urgent true)))]))
    map-out))

;;
;; Returns a map reporting the status of the Notmuch inbox. This is comprised of
;; the total number of messages in the inbox followed by the number of unread
;; messages. This is formatted such that it is compatible with the i3bar protocol.
;;
(define (mail #:color-scheme [color-scheme solarized]
              #:unread-query [unread-query "tag:inbox"]
              #:read-query   [read-query "tag:inbox and tag:unread"])
  (let [(map-out (make-hash))
        (inbox (string-trim (with-output-to-string
                              (lambda () (system (string-append "notmuch count "
                                                                unread-query))))))
        (unread (string-trim (with-output-to-string
                               (lambda () (system (string-append "notmuch count "
                                                                 read-query))))))]
    (hash-set! map-out 'full_text
               (string-append "INBOX: " inbox "/" unread))
    (when (< 0 (string->number unread))
      (hash-set! map-out 'color (hash-ref color-scheme 'blue))
      (hash-set! map-out 'urgent true))
    map-out))

;;
;; Returns a map reporting the current status of the Music Player Daemon. The
;; long version includes the artist, trackname, action (playing or paused) as
;; well as the elapsed and total track time. This map is formatted such that
;; it is compatible with the i3bar protocol.
;;
(define (mpd #:color-scheme [color-scheme solarized])
  (let [(map-out (make-hash))
        (status (string-split (with-output-to-string
                                (lambda () (system "mpc"))) "\n"))
        (current (string-trim (with-output-to-string
                                 (lambda () (system "mpc current")))))]
    (when (< 1 (length status))
      (let [(current (first status))
            (info (string-split (second status)))]
        (hash-set! map-out 'full_text (string-append "MPD: " current " "
                                                     (first info) " "
                                                     (third info)))
        (hash-set! map-out 'short_text current)
        (cond
         [(string=? "[playing]" (first info))
          (hash-set! map-out 'color (hash-ref color-scheme 'blue))])))
    map-out))

;;
;; Private functions
;;

;;
;; Returns a hash with the i3bar header.
;;
(define (header)
  #hash((version . 1)
        (stop_signal . 10)
        (cont_signal . 12)))

;;
;; Writes the provided map of data to the current output port as JSON data.
;;
(define (write-data data-in)
  (fprintf (current-output-port) (jsexpr->string data-in)))

;;
;; Writes the i3bar header data to the current output port as JSON data.
;;
(define (write-header)
  (write-data (header))
  (fprintf (current-output-port) "["))

;;
;; Writes the i3bar status footer to the current output port as JSON data. This
;; indicates the end of the current status bar refresh and prepares for the
;; next.
;;
(define (write-footer)
  (fprintf (current-output-port) ","))

;;
;; Writes he i3bar status closing text to the current output port, completing a
;; status bar session. In reality this function is never used, data is provided
;; to i3 continously as long as it is running. This function is in place solely
;; for testing (i.e., ensuring the JSON data is valid).
;;
(define (write-closing)
  (fprintf (current-output-port) "]\n"))

;;
;; Writes a sample batch of i3bar status data to the current output port as JSON
;; data.
;;
(define (sample-status)
  (write-header)
  (write-data (filter (lambda (item) (< 0 (length (hash-keys item))))
                      (list (mpd) (mail) (battery-charge) (cpu-time) (system-time))))
  (write-closing)
  (flush-output))

;;
;; Starts the process that provides status data to i3. The function provided is
;; expected to produce i3bar protocol compatible maps of data, this data is then
;; assembled into a list, converted to JSON and then provided to i3. This
;; function will be invoked once every 1000ms and this process will run
;; continously.
;;
(define (start-status status-fn #:delay [delay 1])
  (write-header)
  (let loop ()
    (when true
        (write-data (filter (lambda (item) (< 0 (length (hash-keys item))))
                            (status-fn)))
      (write-footer)
      (flush-output)
      (sleep delay)
      (loop))))
