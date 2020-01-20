(ql:quickload :parseq)

(use-package :parseq)

(parseq:defrule hetzner () (and "Hetzner" #\_ (rep 4 digit) #\- (rep 2 digit) #\- (rep 2 digit) #\_ (and #\R (rep 10 digit) ".csv")))

(parseq:trace-rule 'hetzner)

(parseq:parseq 'hetzner "Hetzner_2018-02-19_R0007818495.csv")

(princ "--------------------")
(terpri)

(parseq:defrule iso-date () (and (rep 4 digit) #\- (rep 2 digit) #\- (rep 2 digit)) (:string))

(parseq:defrule yyyymm () (and (rep 4 digit) (rep 2 digit)) (:string))

(parseq:defrule hetzner-ref () (and #\R (rep 10 digit)) (:string))

(parseq:defrule hetzner2 () (and "Hetzner" #\_ iso-date #\_ hetzner-ref #\. "csv"))

(parseq:trace-rule 'hetzner2)

(parseq:parseq 'hetzner2 "Hetzner_2019-02-19_R0007818434.csv")

(princ "--------------------")
(terpri)

(parseq:defrule hetzner3 () (and "Hetzner" #\_ iso-date #\_ hetzner-ref #\. "csv") (:choose 0 2 4))

;; (parseq:trace-rule 'hetzner3)

;; (parseq:untrace-rule 'hetzner3)

(let ((res (parseq:parseq 'hetzner3 "Hetzner_2019-02-19_R0007818434.csv")))
  (if res
      (format t
              "~a_Hetzner-~a-~a~a.csv~%"
              "2020-01-08"
              (car res)
              (cadr res)
              "_accounting_revecloud_computing_bill")
      (progn
        (princ "Not an hetzner file!")
        (terpri))))

(princ "--------------------")
(terpri)

(let ((res (parseq:parseq 'hetzner3 "Hetzner-2019-02-19_R0007818434.csv")))
  (if res
      (format t
              "~a_Hetzner-~a-~a~a.csv~%"
              "2020-01-08"
              (car res)
              (cadr res)
              "_accounting_revecloud_computing_bill")
      (progn
        (princ "Not an hetzner file!")
        (terpri))))

(princ "--------------------")
(terpri)

(parseq:defrule smals-report () (and "smals-report" #\- yyyymm #\. "csv") (:choose 0 2))

(parseq:with-saved-rules
  (parseq:defrule filename-patterns () (or hetzner3 smals-report))
  
  (parseq:trace-rule 'filename-patterns)

  (dolist (filename '("smals-report-201908.csv" "Hetzner_2019-02-19_R0007818434.csv"))
    (let ((res (parseq:parseq 'filename-patterns filename)))
      (if res
          (cond ((string= (car res) "Hetzner")
                 (format t
                         "~a_Hetzner-~a-~a~a.csv~%"
                         "2020-01-08"
                         (second res)
                         (third res)
                         "_accounting_revecloud_computing_bill"))
                ((string= (car res) "smals-report")
                 (format t
                         "~a_smals-report-~a~a.csv~%"
                         "2020-01-08"
                         (cadr res)
                         "_accounting_work_computing_overtime"))
                (t
                 (format t "No processing defined for ~a~%" filename)))
          (format t "No known file pattern for ~a!~%" filename))))

  (parseq:untrace-rule 'filename-patterns))
