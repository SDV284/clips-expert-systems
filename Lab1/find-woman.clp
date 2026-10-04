;;; Завдання 5 - правило пошуку жінок
(defrule find-woman
   (person (name $?name) (gender female))
=>
   (printout t "Знайдено жінку: " (implode$ ?name) crlf)
)
