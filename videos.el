(require 'parse-time)

(defun download (to-folder)
  "Download videos of videos.json
in to to-folder"
  (download
   (retrieve-video-list (json-read))
   to-folder))

;; data processing
(defun retrieve-video-list (data)
  "Retrieve list of pair (video ID, name)
from data like content of videos.json"
  (retrieve-v-n-pairs
   (cdar data)))

(defun retrieve-v-n-pairs (data)
  "Retrieve pairs (video ID, name)"
  (let ((end (length data))
        (ret nil))
    (do ((i 0 (+ i 1)))
        ((>= i end) ret)
      (setq ret
            (cons ret
                  (produce-v-n-pair
                   (svref data i)))))))

(defun produce-v-n-pair (data)
  "Produce pair (video ID, name)
according to ((id . _) (updated_time .
 _) (description . _))"
  (list
   (cdr (assoc 'id data))
   (produce-name
    (cdr (assoc 'updated_time data))
    (cdr (assoc 'description data)))))

(defun produce-name
    (updated_time description)
  ""
  (let ((decoded-time (decode-time (parse-iso8601-time-string updated_time)))
        (year (caaaaaar decoded-time))
        (month (caaaaar decoded-time))
        (day    (caaaar decoded-time))
        (hour    (caaar decoded-time))
        (minute   (caar decoded-time))
        (second    (car decoded-time))
        (date-string (format-date-string year month day hour minute second)))
    (cond
     ((null description) nil)
     ((and (>= hour 12) (<= hour 15)) (get-file-name date-string description))
     (t nil))))

;;test
(defun test (data)
  ""
  (parse-time-string
   "\d"
   ;"(\d\d\d\d)-(\d\d)-(\d\d)T(\d\d):(\d\d):(\d\d)[+-]\d+"
   (cdr (assoc 'updated_time data))))

;;rest
