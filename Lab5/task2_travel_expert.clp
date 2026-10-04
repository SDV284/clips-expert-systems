;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №5
;;; Індивідуальне завдання 3 (Варіант 3): Експертна система вибору
;;; подорожі на основі правил IF-THEN (задача 3.5, с. 289)
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; ---------------------------------------------------------
;;; Глобальні параметри
;;; ---------------------------------------------------------
;; Населений пункт мандрівника
(defglobal ?*home-city* = "Суми")

;;; ---------------------------------------------------------
;;; Шаблони даних (deftemplate)
;;; ---------------------------------------------------------

;; Відповідь користувача на питання системи
(deftemplate answer
   (slot question (type SYMBOL) (allowed-values payment resort))
   (slot value (type SYMBOL)))

;; Фінансові можливості мандрівника, виведені з методу оплати
(deftemplate finance
   ;; 1 - обмежені, 2 - середні, 3 - високі
   (slot level (type INTEGER) (range 1 3))
   (slot label (type STRING)))

;; Місце призначення (4 варіанти)
(deftemplate destination
   (slot id (type SYMBOL))
   (slot name (type STRING))
   (slot resort-type (type SYMBOL) (allowed-values sea ski))
   ;; Мінімальний фінансовий рівень
   (slot min-level (type INTEGER) (range 1 3))
   (slot note (type STRING)))

;; Вид транспорту (3 варіанти)
(deftemplate transport
   (slot type (type SYMBOL) (allowed-values bus train plane))
   (slot name (type STRING))
   ;; Фінансовий рівень, з якого він доступний
   (slot level (type INTEGER) (range 1 3)))

;; Маршрут з міста мандрівника до місця призначення
(deftemplate route
   (slot destination (type SYMBOL))
   (slot transport (type SYMBOL))
   ;; Вартість проїзду туди й назад, грн
   (slot cost (type INTEGER))
   ;; Тривалість дороги в один бік, год
   (slot hours (type NUMBER))
   (slot details (type STRING)))

;; Рекомендована поїздка (результат роботи системи)
(deftemplate recommendation
   (slot destination (type SYMBOL))
   (slot transport (type SYMBOL))
   (slot cost (type INTEGER))
   (slot printed (type SYMBOL) (default no) (allowed-values yes no)))

;;; ---------------------------------------------------------
;;; База знань: місця призначення, транспорт, маршрути
;;; ---------------------------------------------------------
(deffacts travel-knowledge
   (destination (id zatoka) (name "Затока (Одеська обл., Україна)")
      (resort-type sea) (min-level 1)
      (note "піщаний пляж Чорного моря, бюджетне розміщення"))
   (destination (id antalya) (name "Анталія (Туреччина)")
      (resort-type sea) (min-level 3)
      (note "Середземне море, готелі all inclusive"))
   (destination (id slavske) (name "Славське (Львівська обл., Україна)")
      (resort-type ski) (min-level 1)
      (note "Карпати, траси г. Тростян, недороге прокатне спорядження"))
   (destination (id innsbruck) (name "Інсбрук (Тіроль, Австрія)")
      (resort-type ski) (min-level 2)
      (note "Альпи, льодовикові траси, європейський сервіс"))

   (transport (type bus)   (name "автобус") (level 1))
   (transport (type train) (name "поїзд")   (level 2))
   (transport (type plane) (name "літак")   (level 3))

   (route (destination zatoka) (transport bus) (cost 1800) (hours 13)
      (details "прямий автобус Суми - Затока"))
   (route (destination zatoka) (transport train) (cost 2300) (hours 15)
      (details "поїзд Суми - Одеса + електричка до Затоки"))
   (route (destination antalya) (transport plane) (cost 19500) (hours 22)
      (details "автобус до Кишинева + переліт Кишинів - Анталія"))
   (route (destination slavske) (transport bus) (cost 2100) (hours 17)
      (details "автобус Суми - Львів + маршрутка до Славського"))
   (route (destination slavske) (transport train) (cost 2600) (hours 16)
      (details "поїзд Суми - Львів + приміський поїзд до Славського"))
   (route (destination innsbruck) (transport bus) (cost 7400) (hours 34)
      (details "автобус Суми - Львів - Відень - Інсбрук"))
   (route (destination innsbruck) (transport plane) (cost 16800)
      (hours 24)
      (details "поїзд до Перемишля + переліт Жешув - Інсбрук")))

;;; ---------------------------------------------------------
;;; Функція діалогу з користувачем (printout + read)
;;; ---------------------------------------------------------
(deffunction ask-question (?question $?allowed)
   "Ставить питання і повторює його до введення допустимої відповіді"
   (printout t ?question crlf "   Варіанти " ?allowed ": ")
   (bind ?answer (read))
   (if (lexemep ?answer) then (bind ?answer (lowcase ?answer)))
   (while (not (member$ ?answer ?allowed)) do
      (printout t "   Некоректна відповідь. Введіть одне з "
                  ?allowed ": ")
      (bind ?answer (read))
      (if (lexemep ?answer) then (bind ?answer (lowcase ?answer))))
   ?answer)

;;; ---------------------------------------------------------
;;; Правила діалогу
;;; ---------------------------------------------------------

;; R0: привітання
(defrule start-consultation
   (declare (salience 100))
   (not (answer))
   =>
   (printout t crlf "=== Експертна система вибору подорожі ===" crlf)
   (printout t "Місто відправлення: " ?*home-city* crlf crlf))

;; Питання 1: метод оплати
(defrule ask-payment-method
   (declare (salience 90))
   (not (answer (question payment)))
   =>
   (bind ?v (ask-question
      (str-cat "1) Який метод оплати для вас кращий: готівка (cash), "
               "чек (check) чи розрахунковий рахунок (account)?")
      cash check account))
   (assert (answer (question payment) (value ?v))))

;; Питання 2: тип відпочинку
(defrule ask-resort-type
   (declare (salience 80))
   (answer (question payment))
   (not (answer (question resort)))
   =>
   (bind ?v (ask-question
      (str-cat "2) Яка подорож вас цікавить: морський курорт (sea) "
               "чи гірськолижний курорт (ski)?")
      sea ski))
   (assert (answer (question resort) (value ?v))))

;;; ---------------------------------------------------------
;;; Правила IF-THEN: метод оплати -> фінансові можливості
;;; ---------------------------------------------------------

;; R1: IF оплата готівкою THEN фінансові можливості обмежені
(defrule payment-cash
   (answer (question payment) (value cash))
   =>
   (assert (finance (level 1) (label "обмежені (оплата готівкою)"))))

;; R2: IF оплата чеком THEN фінансові можливості середні
(defrule payment-check
   (answer (question payment) (value check))
   =>
   (assert (finance (level 2) (label "середні (оплата чеком)"))))

;; R3: IF оплата з розрахункового рахунку THEN фінансові можливості високі
(defrule payment-account
   (answer (question payment) (value account))
   =>
   (assert (finance (level 3) (label "високі (розрахунковий рахунок)"))))

;;; ---------------------------------------------------------
;;; Правило IF-THEN підбору поїздок
;;; IF тип курорту збігається з інтересом мандрівника
;;;    AND місце призначення доступне за фінансовим рівнем
;;;    AND вид транспорту доступний за фінансовим рівнем
;;; THEN запропонувати поїздку
;;; ---------------------------------------------------------
(defrule match-trip
   (declare (salience 50))
   (answer (question resort) (value ?type))
   (finance (level ?fl))
   (destination (id ?d) (resort-type ?type) (min-level ?dl&:(<= ?dl ?fl)))
   (route (destination ?d) (transport ?t) (cost ?c))
   (transport (type ?t) (level ?tl&:(<= ?tl ?fl)))
   =>
   (assert (recommendation (destination ?d) (transport ?t) (cost ?c))))

;;; ---------------------------------------------------------
;;; Правила виведення результатів
;;; ---------------------------------------------------------

;; Заголовок результатів
(defrule print-header
   (declare (salience 30))
   (answer (question resort) (value ?type))
   (finance (label ?label))
   =>
   (printout t crlf "Фінансові можливості: " ?label crlf)
   (printout t "Тип відпочинку: "
               (if (eq ?type sea)
                  then "морський курорт"
                  else "гірськолижний курорт") crlf)
   (printout t "Можливі поїздки (за зростанням вартості):" crlf))

;; Друк рекомендацій у порядку зростання вартості:
;; щоразу обирається найдешевша ще не надрукована поїздка
(defrule print-cheapest-recommendation
   (declare (salience 20))
   ?r <- (recommendation (destination ?d) (transport ?t) (cost ?c)
                         (printed no))
   (not (recommendation (printed no) (cost ?c2&:(< ?c2 ?c))))
   (destination (id ?d) (name ?dname) (note ?note))
   (transport (type ?t) (name ?tname))
   (route (destination ?d) (transport ?t) (hours ?h) (details ?det))
   =>
   (printout t " * " ?dname " - " ?tname ", " ?c " грн туди й назад, ~"
               ?h " год в один бік" crlf)
   (printout t "     маршрут: " ?det crlf)
   (printout t "     курорт:  " ?note crlf)
   (modify ?r (printed yes)))

;; Підсумок, якщо знайдено хоча б одну поїздку
(defrule print-summary
   (declare (salience 10))
   (answer (question resort))
   (exists (recommendation))
   (not (recommendation (printed no)))
   =>
   (bind ?n (length$ (find-all-facts ((?r recommendation)) TRUE)))
   (printout t "Усього запропоновано поїздок: " ?n crlf))

;; Повідомлення, якщо жодна поїздка не відповідає умовам
(defrule no-trips-found
   (declare (salience 10))
   (answer (question resort))
   (finance)
   (not (recommendation))
   =>
   (printout t "На жаль, для заданих умов поїздок не знайдено." crlf))
