;;; Завдання 5 - правило пошуку чоловіків
(defrule find-men
   (person (name $?name) (gender male))
=>
   (printout t "Знайдено чоловіка: " (implode$ ?name) crlf)
)
