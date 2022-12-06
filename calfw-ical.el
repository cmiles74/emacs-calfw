;;; calfw-ical.el --- calendar view for ical format

;; Copyright (C) 2011  SAKURAI Masashi

;; Author: SAKURAI Masashi <m.sakurai at kiwanami.net>
;; Keywords: calendar

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A bridge from ical to calfw.
;; The API and interfaces have not been confirmed yet.

;;; Installation:

;; Here is a minimum sample code:
;; (require 'calfw-ical)
;; To open a calendar buffer, execute the following function.
;; (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")

;; Executing the following command, this program clears caches to refresh the ICS data.
;; (cfw:ical-data-cache-clear-all)

;;; Code:

(require 'calfw)
(require 'icalendar)
(require 'url)

(defun cfw:decode-to-calendar (dec)
  (cfw:date
   (nth 4 dec) (nth 3 dec) (nth 5 dec)))

(defun cfw:ical-event-get-dates (event)
  "Return date-time information from iCalendar event object:
period event (list 'period start-date end-date), time span
event (list 'time date start-time end-time).  The period includes
end-date.  This function is copied from
`icalendar--convert-ical-to-diary' and modified.  Recursive
events have not been supported yet."
  (let*
      ((dtstart (icalendar--get-event-property event 'DTSTART))
       (dtstart-zone (icalendar--find-time-zone
                      (icalendar--get-event-property-attributes event 'DTSTART) zone-map))
       (dtstart-dec (icalendar--decode-isodatetime dtstart nil dtstart-zone))
       (start-d (cfw:decode-to-calendar dtstart-dec))
       (start-t (cfw:time (nth 2 dtstart-dec) (nth 1 dtstart-dec)))

       (dtend (icalendar--get-event-property event 'DTEND))
       (dtend-zone (icalendar--find-time-zone
                    (icalendar--get-event-property-attributes event 'DTEND) zone-map))
       (dtend-dec (icalendar--decode-isodatetime dtend nil dtend-zone))
       (dtend-1-dec (icalendar--decode-isodatetime dtend -1 dtend-zone))

       (duration (icalendar--get-event-property event 'DURATION))

       end-d end-1-d end-t)

    (when (and dtstart
               (string=
                (cadr (icalendar--get-event-property-attributes
                       event 'DTSTART))
                "DATE"))
      (setq start-t nil))

    (when duration
      (let ((dtend-dec-d (icalendar--add-decoded-times
                          dtstart-dec
                          (icalendar--decode-isoduration duration)))
            (dtend-1-dec-d (icalendar--add-decoded-times
                            dtstart-dec
                            (icalendar--decode-isoduration duration t))))
        (if (and dtend-dec (not (eq dtend-dec dtend-dec-d)))
            (message "Inconsistent endtime and duration for %s" dtend-dec))
        (setq dtend-dec dtend-dec-d)
        (setq dtend-1-dec dtend-1-dec-d)))
    (setq end-d (if dtend-dec
                    (cfw:decode-to-calendar dtend-dec)
                  start-d))
    (setq end-1-d (if dtend-1-dec
                      (cfw:decode-to-calendar dtend-1-dec)
                    start-d))
    (setq end-t (if (and
                     dtend-dec
                     (not (string=
                           (cadr
                            (icalendar--get-event-property-attributes
                             event 'DTEND))
                           "DATE")))
                    (cfw:time (nth 2 dtend-dec) (nth 1 dtend-dec))
                  start-t))
    (cond
     ((and start-t (equal start-d end-d))
      (list 'time start-d start-t end-t))
     ((equal start-d end-1-d)
      (list 'time start-d nil nil))
     (t
      (list 'period start-d nil end-1-d)))))

(defun cfw:ical-sanitize-string (string)
  (when (and string
             (> (length string) 0))
    (replace-regexp-in-string "\\\\n" "\n"
                              (replace-regexp-in-string "\\\\," "," string))))

(defun cfw:ical-convert-recurring-event (zone-map ical-event)
  (let* ((dtstart (icalendar--get-event-property ical-event 'DTSTART))
         (dtstart-zone (icalendar--find-time-zone
                        (icalendar--get-event-property-attributes ical-event 'DTSTART)
                        zone-map))
         (dtstart-dec (icalendar--decode-isodatetime dtstart nil dtstart-zone))
         (dtend (icalendar--get-event-property ical-event 'DTEND))
         (dtend-zone (icalendar--find-time-zone
                      (icalendar--get-event-property-attributes ical-event 'DTEND) zone-map))
         (dtend-dec (icalendar--decode-isodatetime dtend nil dtend-zone))
         (start-t (and dtstart
                       (> (length dtstart) 8)
                       dtstart-dec))
         (summary (icalendar--convert-string-for-import
                   (or (icalendar--get-event-property ical-event 'SUMMARY)
                       "No summary")))
         end-d
         end-1-d
         end-t)
    (let* ((rrule        (icalendar--get-event-property ical-event 'RRULE))
           (rrule-props  (icalendar--split-value rrule))
           (frequency    (cadr (assoc 'FREQ rrule-props)))
           (until        (cadr (assoc 'UNTIL rrule-props)))
           (count        (cadr (assoc 'COUNT rrule-props)))
           (interval     (read (or (cadr (assoc 'INTERVAL rrule-props)) "1")))
           (until-conv   (icalendar--decode-isodatetime until))
           (until-1-conv (icalendar--decode-isodatetime until -1))
           (result nil))
      (when count
        (if until
            (message "Must not have UNTIL and COUNT -- ignoring COUNT element!")
          (let ((until-1 0))
            (cond ((string-equal frequency "DAILY")
                   (setq until (icalendar--add-decoded-times
                                dtstart-dec
                                (list 0 0 0 (* (read count) interval) 0 0)))
                   (setq until-1 (icalendar--add-decoded-times
                                  dtstart-dec
                                  (list 0 0 0 (* (- (read count) 1) interval)
                                        0 0)))
                   )
                  ((string-equal frequency "WEEKLY")
                   (setq until (icalendar--add-decoded-times
                                dtstart-dec
                                (list 0 0 0 (* (read count) 7 interval) 0 0)))
                   (setq until-1 (icalendar--add-decoded-times
                                  dtstart-dec
                                  (list 0 0 0 (* (- (read count) 1) 7
                                                 interval) 0 0)))
                   )
                  ((string-equal frequency "MONTHLY")
                   (setq until (icalendar--add-decoded-times
                                dtstart-dec (list 0 0 0 0 (* (- (read count) 1)
                                                             interval) 0)))
                   (setq until-1 (icalendar--add-decoded-times
                                  dtstart-dec (list 0 0 0 0 (* (- (read count) 1)
                                                               interval) 0)))
                   )
                  ((string-equal frequency "YEARLY")
                   (setq until (icalendar--add-decoded-times
                                dtstart-dec (list 0 0 0 0 0 (* (- (read count) 1)
                                                               interval))))
                   (setq until-1 (icalendar--add-decoded-times
                                  dtstart-dec
                                  (list 0 0 0 0 0 (* (- (read count) 1)
                                                     interval))))
                   )
                  (t
                   (message "Cannot handle COUNT attribute for `%s' events."
                            frequency)))
            (setq until-conv until)
            (setq until-1-conv until-1))))

      (cond ((and (string-equal frequency "DAILY"))
             (if until
                 (let ((event-out
                        (list
                         (make-cfw:event
                          :start-date  (cfw:decode-to-calendar dtstart-dec)
                          :start-time  (cfw:time (nth 2 dtstart-dec) (nth 1 dtstart-dec))
                          :end-date    nil; (cfw:decode-to-calendar dtstart-dec)
                          :end-time    (cfw:time (nth 2 dtend-dec) (nth 1 dtend-dec))
                          :title       (cfw:ical-sanitize-string
                                        (icalendar--get-event-property event 'SUMMARY))
                          :location    (cfw:ical-sanitize-string
                                        (icalendar--get-event-property event 'LOCATION))
                          :description (cfw:ical-sanitize-string
                                        (icalendar--get-event-property event 'DESCRIPTION)))
                         (make-cfw:event
                          :start-date  (cfw:decode-to-calendar (if count until-1-conv until-conv))
                          :start-time  (cfw:time (nth 2 dtstart-dec) (nth 1 dtstart-dec))
                          :end-date    nil ;(cfw:decode-to-calendar (if count until-1-conv until-conv))
                          :end-time    (cfw:time (nth 2 dtend-dec) (nth 1 dtend-dec))
                          :title       (cfw:ical-sanitize-string
                                        (icalendar--get-event-property event 'SUMMARY))
                          :location    (cfw:ical-sanitize-string
                                        (icalendar--get-event-property event 'LOCATION))
                          :description (cfw:ical-sanitize-string
                                        (icalendar--get-event-property event 'DESCRIPTION))))))
                   event-out)
               nil))))))

(defun cfw:ical-convert-event (zone-map event)
  (if (icalendar--get-event-property event 'RRULE)
      (cfw:ical-convert-recurring-event zone-map event)
    (destructuring-bind (dtag date start end) (cfw:ical-event-get-dates event)
      (make-cfw:event
       :start-date  date
       :start-time  start
       :end-date    (when (equal dtag 'period) end)
       :end-time    (when (equal dtag 'time)   end)
       :title       (cfw:ical-sanitize-string
                     (icalendar--get-event-property event 'SUMMARY))
       :location    (cfw:ical-sanitize-string
                     (icalendar--get-event-property event 'LOCATION))
       :description (cfw:ical-sanitize-string
                     (icalendar--get-event-property event 'DESCRIPTION))))))

(defun cfw:ical-convert-ical-to-calfw (ical-list)
  (let* ((zone-map (icalendar--convert-all-timezones ical-list))
         (ical-events (icalendar--all-events ical-list)))
    (let (events-out)
      (dolist (ical-event ical-events)
        (let ((conv-events (cfw:ical-convert-event zone-map ical-event)))
          (if (listp conv-events)
              (dolist (conv-event conv-events)
                (setq events-out (cons conv-event events-out)))
            (setq events-out (cons conv-events events-out)))))
      (cl-loop for event in events-out
               if event
               if (cfw:event-end-date event)
               collect event into periods
               else
               collect event into contents
               else do
               (progn
                 (message "Ignoring event \"%s\"" e)
                 (message "Cannot handle this event, tag: %s" e))
               finally (return `((periods ,periods) ,@contents))))))

(defun cfw:ical-debug (f)
  (interactive)
  (let ((buf (cfw:ical-url-to-buffer f)))
    (unwind-protect
        (pp-display-expression
         (with-current-buffer buf
           (cfw:ical-normalize-buffer)
           (cfw:ical-convert-ical-to-calfw
            (icalendar--read-element nil nil)))
         "*ical-debug*")
      (kill-buffer buf))))

(defvar cfw:ical-calendar-external-shell-command "wget -q -O - ")
(defvar cfw:ical-calendar-tmpbuf " *calfw-tmp*")
(defvar cfw:ical-url-to-buffer-get 'cfw:ical-url-to-buffer-internal)

(defun cfw:ical-url-to-buffer-external (url)
  "Retrieve ICS file with an external command."
  (let ((buf (get-buffer-create cfw:ical-calendar-tmpbuf)))
    (buffer-disable-undo buf)
    (with-current-buffer buf
      (erase-buffer))
    (call-process-shell-command
     cfw:ical-calendar-external-shell-command nil buf nil url)
    buf))

(defun cfw:ical-url-to-buffer-internal (url)
  "Retrieve ICS file with the url package."
  (let ((buf (url-retrieve-synchronously url))
        (dbuf (get-buffer-create cfw:ical-calendar-tmpbuf))
        pos)
    (unwind-protect
        (when (setq pos (url-http-symbol-value-in-buffer
                         'url-http-end-of-headers buf))
          (with-current-buffer dbuf
            (erase-buffer)
            (decode-coding-string
             (with-current-buffer buf
               (buffer-substring (1+ pos) (point-max)))
             'utf-8 nil dbuf)))
      (kill-buffer buf))
    dbuf))

(defun cfw:ical-url-to-buffer (url)
  (let* ((url-code (url-generic-parse-url url))
         (type (url-type url-code)))
    (cond
     (type
      (funcall cfw:ical-url-to-buffer-get url))
     (t ; assume local file
      (let ((buf (find-file-noselect (expand-file-name url) t)))
        (with-current-buffer buf (set-visited-file-name nil))
        buf)))))

(defmacro cfw:ical-with-buffer (url &rest body)
  (let (($buf (gensym)))
    `(let ((,$buf (cfw:ical-url-to-buffer ,url)))
       (unwind-protect
           (with-current-buffer ,$buf
             (goto-char (point-min))
             ,@body)
         (kill-buffer ,$buf)))))
(put 'cfw:ical-with-buffer 'lisp-indent-function 1)

(defun cfw:ical-normalize-buffer ()
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\n " nil t)
      (replace-match "")))
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "DT\\(START\\|END\\);VALUE=DATE:" nil t)
      (replace-match "DT\\1:")))
  (set-buffer-modified-p nil))

(defvar cfw:ical-data-cache nil "a list of (url . ics-data)")

(defun cfw:ical-data-cache-clear (url)
  (setq cfw:ical-data-cache
        (loop for i in cfw:ical-data-cache
              for (u . d) = i
              unless (equal u url)
              collect i)))

(defun cfw:ical-data-cache-clear-all ()
  (interactive)
  (setq cfw:ical-data-cache nil))

(defun cfw:ical-get-data (url)
  (let ((data (assoc url cfw:ical-data-cache)))
    (unless data
      (setq data (let ((cal-list
                        (cfw:ical-with-buffer url
                          (cfw:ical-normalize-buffer)
                          (cfw:ical-convert-ical-to-calfw
                           (icalendar--read-element nil nil)))))
                   (cons url cal-list)))
      (push data cfw:ical-data-cache))
    (cdr data)))

(defun cfw:ical-to-calendar (url begin end)
  (loop for event in (cfw:ical-get-data url)
        if (and (listp event)
                (equal 'periods (car event)))
        collect
        (cons
         'periods
         (loop for evt in (cadr event)
               if (and
                   (cfw:date-less-equal-p begin (cfw:event-end-date evt))
                   (cfw:date-less-equal-p (cfw:event-start-date evt) end))
               collect evt))
        else if (cfw:date-between begin end (cfw:event-start-date event))
        collect event))

(defun cfw:ical-create-source (name url color)
  (lexical-let ((url url))
    (make-cfw:source
     :name (concat "iCal:" name)
     :color color
     :update (lambda () (cfw:ical-data-cache-clear url))
     :data (lambda (begin end)
             (cfw:ical-to-calendar url begin end)))))

(defun cfw:open-ical-calendar (url)
  "Simple calendar interface. This command displays just one
calendar source."
  (interactive)
  (save-excursion
    (let ((cp (cfw:create-calendar-component-buffer
               :view 'month
               :contents-sources
               (list (cfw:ical-create-source "ical" url "#2952a3")))))
      (switch-to-buffer (cfw:cp-get-buffer cp)))))

;; (progn (eval-current-buffer) (cfw:open-ical-calendar "./ics/test.ics"))

(provide 'calfw-ical)
;;; calfw-ical.el ends here
