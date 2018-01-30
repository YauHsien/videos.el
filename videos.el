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
        (date-string (format "%04d%02d%02d%02d%02d%02d" year month day hour minute second))
        (has-keyword (not (nil (string-match "月亮 " description))))
        (distilled-desc (distill-description (split-string description "\n"))))
    (cond
     ((null description) nil)
     (has-keyword (get-file-name date-string distilled-desc))
     ((and (>= hour 12) (<= hour 15)) (get-file-name date-string distilled-desc))
     (t nil))))

(defun distill-description (splited-description)
  "distill lecturer and subject
講者與標題格式可能有二種：一種是倒數第二行是講者，而最後一行是標題。
另一種則是最後幾行以 \"+\" 開頭的行是講者們，而在講者們之前一行或隔一行是標題。"
  (let ((pull-reversed (reverse (strip-empty-string splited-description nil)))
        (lecturer (find-lecturer pull-reversed))
        (subject (find-subject pull-reversed)))
    (append lecturer (list subject))))

(defun strip-empty-string (data acc)
  "Tick out empty string from data"
  (cond ((null data) (reverse acc))
        (t (cond ((equal "" (car data)) (strip-empty-string (cdr data) acc))
                 (t (strip-empty-string (cdr data) (cons (car data) acc)))))))

(defun get-file-name (date-string description)
  "Get a file-name with date and description"
  (format "%s_%s" date-string (trans-desc description)))

(defun find-lecturer (data)
  "講者與標題格式可能有二種：一種是倒數第二行是講者，而最後一行是標題。
另一種則是最後幾行以 \"+\" 開頭的行是講者們，而在講者們之前一行或隔一行是標題。"
  (cond ((nil (string-match "^+" (car data))) (list (cdar data)))
        (t (take-lecturers data))))

(defun find-subject (data)
  "講者與標題格式可能有二種：一種是倒數第二行是講者，而最後一行是標題。
另一種則是最後幾行以 \"+\" 開頭的行是講者們，而在講者們之前一行或隔一行是標題。"
  (cond ((nil (string-match "^+" (car data))) (list (car data)))
        (t (take-subject-behind-lecturers data))))

;;TODO (defun take-lecturers (data))
;;TODO (defun take-subject-behind-lecturers (data))

;;TODO (defun trans-desc)

;;test

;;rest
