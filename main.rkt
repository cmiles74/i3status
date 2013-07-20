#lang racket

(provide (except-out (all-defined-out) header write-data write-header write-footer
                     write-closing sample-status))

(require srfi/19)
(require srfi/48)
(require json)
(require racket/file)

(define (system-time)
  (let ([map-out (make-hash)])
    (hash-set! map-out 'full_text (date->string (current-date) "~Y-~m-~d ~k:~M"))
    (hash-set! map-out 'short_text (date->string (current-date) "~k:~M"))
    map-out))

(define (uptime)
  (let* [(uptime-raw (file->string "/proc/uptime"))
         (uptime-data (regexp-split #px"[[:space:]]+" uptime-raw))
         (uptime-map (make-hash))]
    (hash-set! uptime-map 'uptime (string->number (first uptime-data)))
    (hash-set! uptime-map 'idle (string->number (second uptime-data)))
    uptime-map))

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

(define (cpu-time)
  (let [(map-out (make-hash))
        (uptime-1 (hash-ref (uptime) 'uptime))
        (sample-1 (cpu-time-sample))]
    (sleep 0.25)
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
         [(< 75 avg-time) (hash-set! map-out 'color "#dc322f")]
         [(< 50 avg-time) (hash-set! map-out 'color "#b58900")])
        map-out))))

(define (battery-charge)
  (let [(map-out (make-hash))
        (description (string-trim (file->string "/sys/class/power_supply/BAT0/status")))
        (charge-now (string->number (string-trim
                                  (file->string "/sys/class/power_supply/BAT0/charge_now"))))
        (charge-full (string->number (string-trim
                                   (file->string "/sys/class/power_supply/BAT0/charge_full_design"))))]
    (let [(charge-pct (* 100 (/ charge-now charge-full)))]
          (hash-set! map-out 'full_text
               (string-append "BATTERY: " description " " (format "~6,2F" charge-pct)))
          (hash-set! map-out 'short_text
               (string-append "BATTERY: " (cond [(string=? "Full" description) "F"]
                                                [(string=? "Charging" description) "C"]
                                                [(string=? "Discharging" description) "D"])
                              " " (format "~6,2F" charge-pct)))
          (cond
           [(string=? "Discharging" description) (hash-set! map-out 'color "#268bd2")]
           [(> 0.4 charge-pct) (hash-set! map-out 'color "#d33682")]
           [(> 0.25 charge-pct) (lambda () ((hash-set! map-out 'color "#dc322f")
                                            (hash-set! map-out 'urgent true)))]))
    map-out))

(define (mail)
  (let [(map-out (make-hash))
        (inbox (string-trim (with-output-to-string
                              (lambda () (system "notmuch count tag:inbox")))))
        (unread (string-trim (with-output-to-string
                               (lambda () (system "notmuch count tag:inbox and tag:unread")))))]
    (hash-set! map-out 'full_text
               (string-append "INBOX: " inbox "/" unread))
    (when (< 0 (string->number unread))
      (hash-set! map-out 'color "#268bd2")
      (hash-set! map-out 'urgent true))
    map-out))

(define (mpd)
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
          (hash-set! map-out 'color "#268bd2")])))
    map-out))

;;
;; Private functions
;;

(define (header)
  #hash((version . 1)
        (stop_signal . 10)
        (cont_signal . 12)))

(define (write-data data-in)
  (fprintf (current-output-port) (jsexpr->string data-in)))

(define (write-header)
  (write-data (header))
  (fprintf (current-output-port) "["))

(define (write-footer)
  (fprintf (current-output-port) ","))

(define (write-closing)
  (fprintf (current-output-port) "]\n"))

(define (sample-status)
  (write-header)
  (write-data (filter (lambda (item) (< 0 (length (hash-keys item))))
                      (list (mpd) (mail) (battery-charge) (cpu-time) (system-time))))
  (write-closing)
  (flush-output))

(define (start-status status-fn)
  (write-header)
  (let loop ()
    (when true
        (write-data (filter (lambda (item) (< 0 (length (hash-keys item))))
                            (status-fn)))
      (write-footer)
      (flush-output)
      (sleep 1)
      (loop))))
