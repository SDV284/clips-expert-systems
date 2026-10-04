;;; Завдання 3 - прості правила
(defrule print-day
   (Today is ?day)
=>
   (printout t "Today is " ?day crlf)
)
