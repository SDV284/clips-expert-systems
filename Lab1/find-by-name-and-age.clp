;;; Завдання 5 - правила для пошуку людей з певним ім'ям і певним віком
(defrule find-by-name
   (person (name Mary Ann Clark) (age ?age) (gender ?gender))
=>
   (printout t "Знайдено за ім'ям (Mary Ann Clark): вік " ?age ", стать " ?gender crlf)
)

(defrule find-by-age
   (person (name $?name) (age 25) (gender ?gender))
=>
   (printout t "Знайдено за віком (25 років): " (implode$ ?name) ", стать " ?gender crlf)
)

(defrule find-person-by-name-and-age
   (person (name John Smith) (age 25) (gender ?gender))
=>
   (printout t "Знайдено за ім'ям та віком (John Smith, 25): стать " ?gender crlf)
)
