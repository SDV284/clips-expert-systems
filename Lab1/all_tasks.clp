;;; Завдання 2 & 3: Робота з простими фактами та правилами
(deffacts what-day-is-it?
   (Today is Thursday)
   (Tomorrow is Friday)
)

(defrule print-day
   (Today is ?day)
=>
   (printout t "[Завдання 3] Сьогодні: Today is " ?day crlf)
)

;;; Завдання 4 & 4*: Впорядковані факти (шаблони з обмеженнями)
(deftemplate person
   (multislot name)
   (slot age (type INTEGER))
   (slot gender (allowed-values male female))
)

(deffacts people
   (person (name John Smith) (age 25) (gender male))
   (person (name Mary Ann Clark) (age 22) (gender female))
   (person (name Bob Brown) (age 30) (gender male))
)

;;; Завдання 5: Правила пошуку
(defrule find-men
   (person (name $?name) (gender male))
=>
   (printout t "[Завдання 5: Чоловік] " (implode$ ?name) crlf)
)

(defrule find-woman
   (person (name $?name) (gender female))
=>
   (printout t "[Завдання 5: Жінка] " (implode$ ?name) crlf)
)

(defrule find-by-name
   (person (name Mary Ann Clark) (age ?age) (gender ?gender))
=>
   (printout t "[Завдання 5: Пошук за ім'ям Mary Ann Clark] Вік: " ?age ", стать: " ?gender crlf)
)

(defrule find-by-age
   (person (name $?name) (age 25) (gender ?gender))
=>
   (printout t "[Завдання 5: Пошук за віком 25] Ім'я: " (implode$ ?name) ", стать: " ?gender crlf)
)

(defrule find-person-by-name-and-age
   (person (name John Smith) (age 25) (gender ?gender))
=>
   (printout t "[Завдання 5: Пошук за ім'ям John Smith та віком 25] Стать: " ?gender crlf)
)
