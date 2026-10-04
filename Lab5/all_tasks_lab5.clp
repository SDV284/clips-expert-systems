;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №5
;;; Консолідований файл: загальне завдання (префіксні вирази)
;;; та індивідуальне завдання 3 (експертна система вибору подорожі)
;;; Студент: Святішенко, група ІН-32-2
;;;
;;; Порядок виконання задається салієнтністю правил:
;;;   400       - заголовок модуля 1
;;;   310..300  - модуль 1: перетворення та перевірка виразів
;;;   200       - заголовок модуля 2
;;;   100..10   - модуль 2: діалог, виведення та друк поїздок
;;;   -10       - завершення роботи
;;; =========================================================

;;; ---------------------------------------------------------
;;; Службові правила-заголовки
;;; ---------------------------------------------------------
(defrule banner-module-1
   (declare (salience 400))
   =>
   (printout t "================================================" crlf)
   (printout t " МОДУЛЬ 1. Інфіксні вирази -> префіксні" crlf)
   (printout t "================================================" crlf))

(defrule banner-module-2
   (declare (salience 200))
   =>
   (printout t "================================================" crlf)
   (printout t " МОДУЛЬ 2. ЕС вибору подорожі (варіант 3)" crlf)
   (printout t "================================================" crlf))

(defrule finish-lab5
   (declare (salience -10))
   =>
   (printout t "================================================" crlf)
   (printout t " Усі завдання лабораторної роботи №5 виконано." crlf)
   (printout t "================================================" crlf))

;;; #########################################################
;;; МОДУЛЬ 1. ЗАГАЛЬНЕ ЗАВДАННЯ
;;; #########################################################

;;; ---------------------------------------------------------
;;; Шаблон виразу (deftemplate)
;;; ---------------------------------------------------------
(deftemplate expression
   ;; Позначення виразу (а, б, в)
   (slot id (type SYMBOL))
   ;; Вихідний інфіксний запис
   (slot infix (type STRING))
   ;; Префіксний запис, складений вручну
   (slot manual-prefix (type STRING))
   ;; Префіксний запис, отриманий програмно
   (slot auto-prefix (type STRING) (default ""))
   ;; Етап обробки виразу
   (slot status (type SYMBOL) (default new)
      (allowed-values new converted checked)))

;;; ---------------------------------------------------------
;;; Глобальні змінні синтаксичного аналізатора
;;; ---------------------------------------------------------
;; Список лексем поточного виразу
(defglobal ?*tokens* = (create$)
           ;; Номер поточної лексеми
           ?*pos* = 1)

;;; ---------------------------------------------------------
;;; Лексичний аналіз: розбиття рядка на лексеми
;;; ---------------------------------------------------------
(deffunction digit-p (?ch)
   "Перевіряє, чи є символ цифрою або десятковою крапкою"
   (if (member$ ?ch (create$ "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "."))
      then TRUE
      else FALSE))

(deffunction tokenize (?str)
   "Повертає список лексем: числа, знаки операцій та дужки"
   (bind ?result (create$))
   (bind ?num "")
   (loop-for-count (?i 1 (str-length ?str)) do
      (bind ?ch (sub-string ?i ?i ?str))
      (if (digit-p ?ch)
         then (bind ?num (str-cat ?num ?ch))
         else (if (neq ?num "")
                 then (bind ?result (create$ ?result ?num))
                      (bind ?num ""))
              (if (member$ ?ch (create$ "+" "-" "*" "/" "(" ")"))
                 then (bind ?result (create$ ?result ?ch)))))
   (if (neq ?num "") then (bind ?result (create$ ?result ?num)))
   ?result)

;;; ---------------------------------------------------------
;;; Синтаксичний аналіз методом рекурсивного спуску
;;;   <вираз>    ::= <доданок> { (+ | -) <доданок> }
;;;   <доданок>  ::= <множник> { (* | /) <множник> }
;;;   <множник>  ::= <число> | ( <вираз> )
;;; ---------------------------------------------------------
(deffunction current-token ()
   "Повертає поточну лексему або EOF, якщо лексеми закінчилися"
   (if (<= ?*pos* (length$ ?*tokens*))
      then (nth$ ?*pos* ?*tokens*)
      else "EOF"))

(deffunction advance ()
   "Переходить до наступної лексеми"
   (bind ?*pos* (+ ?*pos* 1)))

;; Попереднє оголошення для взаємної рекурсії parse-expr <-> parse-factor
(deffunction parse-expr ())

(deffunction parse-factor ()
   "Розбирає число або вираз у дужках"
   (bind ?tok (current-token))
   (if (eq ?tok "(")
      then (advance)
           (bind ?inner (parse-expr))
           (if (neq (current-token) ")")
              then (printout t "[Помилка] Очікувалась дужка ')'" crlf))
           (advance)
           ?inner
      else (advance)
           ?tok))

(deffunction parse-term ()
   "Розбирає послідовність множення та ділення (лівоасоціативно)"
   (bind ?left (parse-factor))
   (while (member$ (current-token) (create$ "*" "/")) do
      (bind ?op (current-token))
      (advance)
      (bind ?right (parse-factor))
      (bind ?left (str-cat "(" ?op " " ?left " " ?right ")")))
   ?left)

(deffunction parse-expr ()
   "Розбирає послідовність додавання та віднімання (лівоасоціативно)"
   (bind ?left (parse-term))
   (while (member$ (current-token) (create$ "+" "-")) do
      (bind ?op (current-token))
      (advance)
      (bind ?right (parse-term))
      (bind ?left (str-cat "(" ?op " " ?left " " ?right ")")))
   ?left)

(deffunction infix-to-prefix (?str)
   "Перетворює інфіксний рядок у префіксний запис CLIPS"
   (bind ?*tokens* (tokenize ?str))
   (bind ?*pos* 1)
   (parse-expr))

;;; ---------------------------------------------------------
;;; Вихідні дані: три вирази загального завдання
;;; ---------------------------------------------------------
(deffacts infix-expressions
   (expression (id a)
      (infix "(3 + 4) * (5 + 6) + 7")
      (manual-prefix "(+ (* (+ 3 4) (+ 5 6)) 7)"))
   (expression (id b)
      (infix "(5 * (5 + 6 + 7)) - ((3 * (4/9) + 2) / 8)")
      (manual-prefix "(- (* 5 (+ 5 6 7)) (/ (+ (* 3 (/ 4 9)) 2) 8))"))
   (expression (id c)
      (infix "6 - 9 * 8/3 + 4 - (8 - 2 - 3) * 6/7")
      (manual-prefix
         "(- (+ (- 6 (/ (* 9 8) 3)) 4) (/ (* (- 8 2 3) 6) 7))")))

;;; ---------------------------------------------------------
;;; Правила (defrule)
;;; ---------------------------------------------------------

;; Правило 1: програмне перетворення інфіксного виразу в префіксний
(defrule convert-infix-to-prefix
   (declare (salience 310))
   ?e <- (expression (id ?id) (infix ?inf) (status new))
   =>
   (bind ?pref (infix-to-prefix ?inf))
   (modify ?e (auto-prefix ?pref) (status converted)))

;; Правило 2: обчислення обох префіксних записів та порівняння результатів
(defrule evaluate-and-verify
   (declare (salience 300))
   ?e <- (expression (id ?id) (infix ?inf) (manual-prefix ?man)
                     (auto-prefix ?auto) (status converted))
   ;; вирази виводяться за алфавітним порядком позначень (а, б, в)
   (not (expression (status converted)
                    (id ?other&:(< (str-compare ?other ?id) 0))))
   =>
   (bind ?v1 (eval ?man))
   (bind ?v2 (eval ?auto))
   (printout t "Вираз " ?id ")  інфіксний:  " ?inf crlf)
   (printout t "   префіксний (вручну):    " ?man crlf)
   (printout t "   префіксний (програмно): " ?auto crlf)
   (printout t "   значення: " ?v1 " | " ?v2
               (if (< (abs (- ?v1 ?v2)) 1.0e-9)
                  then "  -> збігаються"
                  else "  -> РОЗБІЖНІСТЬ")
               crlf crlf)
   (modify ?e (status checked)))

;;; #########################################################
;;; МОДУЛЬ 2. ІНДИВІДУАЛЬНЕ ЗАВДАННЯ 3
;;; #########################################################

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
