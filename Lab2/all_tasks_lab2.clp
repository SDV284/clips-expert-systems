;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №2
;;; Комплексний програмний файл усіх завдань (Завдання 1 та Завдання 2)
;;; Студент: Святішенко, група ІН-32-2
;;; Варіант 2-го завдання: Варіант 3 (Мережа авіаліній)
;;; =========================================================

;;; ---------------------------------------------------------
;;; ЧАСТИНА 1. ЗАВДАННЯ 1: РОДИННІ ЗВ'ЯЗКИ ТА СТРУКТУРОВАНИЙ ОПИС
;;; ---------------------------------------------------------

;; Шаблон 1: Стать особи
(deftemplate person-gender
   (slot name (type SYMBOL))
   (slot gender (type SYMBOL) (allowed-values male female))
)

;; Шаблон 2: Роль у родині
(deftemplate family-role
   (slot name (type SYMBOL))
   (slot role (type SYMBOL) (allowed-values father mother son daughter parent child))
)

;; Шаблон 3: Батьківське відношення
(deftemplate parent-child
   (slot parent (type SYMBOL))
   (slot child (type SYMBOL))
   (slot relationship (type SYMBOL) (allowed-values father mother))
)

;; Шаблон 4: Спільні батьки
(deftemplate parents-of
   (slot child (type SYMBOL))
   (multislot parents (type SYMBOL))
)

;; Ініціалізація фактів Завдання 1
(deffacts family-facts
   (person-gender (name Tom) (gender male))
   (person-gender (name Susan) (gender female))
   (person-gender (name John) (gender male))
   (family-role (name Tom) (role father))
   (family-role (name Susan) (role mother))
   (family-role (name John) (role son))
   (parent-child (parent Tom) (child John) (relationship father))
   (parent-child (parent Susan) (child John) (relationship mother))
   (parents-of (child John) (parents Tom Susan))
)

;; ---------------------------------------------------------
;; ЧАСТИНА 2. ЗАВДАННЯ 2 (ВАРІАНТ 3): МЕРЕЖА АВІАЛІНІЙ (РИС. 1)
;; ---------------------------------------------------------

;; Єдина конструкція deftemplate для опису сполучень
(deftemplate flight
   (slot from (type SYMBOL))
   (slot to (type SYMBOL))
)

;; Ініціалізація фактів орієнтованого графа авіаліній
(deffacts airline-network
   (flight (from San-Francisco) (to Chicago))
   (flight (from San-Francisco) (to Los-Angeles))
   (flight (from Chicago) (to Indianapolis))
   (flight (from Indianapolis) (to Houston))
   (flight (from Los-Angeles) (to Houston))
   (flight (from Houston) (to San-Francisco))
   (flight (from Houston) (to New-York))
   (flight (from New-York) (to Chicago))
)

;; ---------------------------------------------------------
;; ЧАСТИНА 3. ПРАВИЛА ЛОГІЧНОГО ВИВЕДЕННЯ
;; ---------------------------------------------------------

;; Службове правило для виведення заголовка Завдання 1
(defrule banner-task1
   (declare (salience 100))
=>
   (printout t "==================================================" crlf)
   (printout t "  РЕЗУЛЬТАТИ ВИКОНАННЯ ЗАВДАННЯ 1 (РОДИННІ ЗВ'ЯЗКИ)" crlf)
   (printout t "==================================================" crlf)
)

;; Правила Завдання 1
(defrule print-parents-info
   (declare (salience 90))
   (parents-of (child ?c) (parents $?p))
=>
   (printout t "[Батьки] Батьками " ?c " є: " (implode$ ?p) crlf)
)

(defrule print-parent-child-info
   (declare (salience 80))
   (parent-child (parent ?p) (child ?c) (relationship ?rel))
=>
   (printout t "[Родина] " ?rel " дитини " ?c " -> " ?p crlf)
)

(defrule print-person-role-and-gender
   (declare (salience 70))
   (family-role (name ?n) (role ?r))
   (person-gender (name ?n) (gender ?g))
=>
   (printout t "[Особа] " ?n " має роль: " ?r ", стать: " ?g crlf)
)

;; Службове правило для виведення заголовка Завдання 2
(defrule banner-task2
   (declare (salience 50))
=>
   (printout t "==================================================" crlf)
   (printout t "  РЕЗУЛЬТАТИ ВИКОНАННЯ ЗАВДАННЯ 2 (МЕРЕЖА АВІАЛІНІЙ)" crlf)
   (printout t "==================================================" crlf)
)

;; Правила Завдання 2
(defrule print-direct-flight
   (declare (salience 40))
   (flight (from ?origin) (to ?dest))
=>
   (printout t "[Прямий рейс] " ?origin " --> " ?dest crlf)
)

(defrule banner-transit
   (declare (salience 30))
=>
   (printout t "--- Транзитні маршрути з 1 пересадкою ---" crlf)
)

(defrule find-connecting-flights
   (declare (salience 20))
   (flight (from ?origin) (to ?transfer))
   (flight (from ?transfer) (to ?dest))
   (test (neq ?origin ?dest))
=>
   (printout t "[Рейс з 1 пересадкою] " ?origin " --> " ?transfer " --> " ?dest crlf)
)

(defrule banner-finish
   (declare (salience 10))
=>
   (printout t "==================================================" crlf)
)
