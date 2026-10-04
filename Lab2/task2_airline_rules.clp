;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №2
;;; Завдання 2 (Варіант 3): Правила виведення та пошуку маршрутів
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; Правило 1: Виведення всіх прямих рейсів (дуг графа)
(defrule print-direct-flight
   (declare (salience 20))
   (flight (from ?origin) (to ?dest))
=>
   (printout t "[Завдання 2: Прямий рейс] " ?origin " --> " ?dest crlf)
)

;;; Правило 2: Пошук рейсів з 1 пересадкою (транзитні сполучення довжиною 2)
(defrule find-connecting-flights
   (declare (salience 10))
   (flight (from ?origin) (to ?transfer))
   (flight (from ?transfer) (to ?dest))
   (test (neq ?origin ?dest))
=>
   (printout t "[Завдання 2: Рейс з 1 пересадкою] " ?origin " --> " ?transfer " --> " ?dest crlf)
)

;;; Правило 3: Виявлення циклічних маршрутів довжиною 3
(defrule find-cycle-route
   (declare (salience 5))
   (flight (from ?c1) (to ?c2))
   (flight (from ?c2) (to ?c3))
   (flight (from ?c3) (to ?c1))
   (test (and (neq ?c1 ?c2) (neq ?c2 ?c3) (neq ?c1 ?c3)))
=>
   (printout t "[Завдання 2: Кільцевий маршрут] " ?c1 " -> " ?c2 " -> " ?c3 " -> " ?c1 crlf)
)
