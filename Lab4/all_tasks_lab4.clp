;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №4
;;; КОМПЛЕКСНИЙ ВИКОНУВАНИЙ ФАЙЛ: ВСІ ЗАВДАННЯ (all_tasks_lab4.clp)
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; #########################################################
;;; ЧАСТИНА 1. ЗАВДАННЯ 1: ФУНКЦІЇ КОРИСТУВАЧА, РЕКУРСІЯ ТА ГРУПОВІ ПАРАМЕТРИ
;;; #########################################################

;; 1.1. Функція з обов'язковими та груповими (wildcard) параметрами
(deffunction print-args (?A ?B $?C)
   "Демонструє обробку обов'язкових та групових параметрів"
   (printout t "[print-args] Обов'язкові: A=" ?A ", B=" ?B "; Додаткових: " (length$ ?C) ", список: " ?C crlf))

;; 1.2. Саморекурсивна функція факторіалу з валідацією
(deffunction factorial (?A)
   "Рекурсивне обчислення факторіалу"
   (if (or (not (integerp ?A)) (< ?A 0))
      then (printout t "[Помилка] Аргумент повинен бути невід'ємним цілим!" crlf)
           FALSE
      else (if (= ?A 0)
              then 1
              else (* ?A (factorial (- ?A 1))))))

;; 1.3. Взаємна рекурсія та попереднє оголошення (Forward Declaration)
(deffunction is-even (?N))

(deffunction is-odd (?N)
   "Перевірка непарності через взаємну рекурсію"
   (if (= ?N 0)
      then FALSE
      else (if (< ?N 0)
              then (is-odd (- 0 ?N))
              else (is-even (- ?N 1)))))

(deffunction is-even (?N)
   "Перевірка парності через взаємну рекурсію"
   (if (= ?N 0)
      then TRUE
      else (if (< ?N 0)
              then (is-even (- 0 ?N))
              else (is-odd (- ?N 1)))))

;; 1.4. Обчислювальна геометрична функція (відстань між точками)
(deffunction euclidean-distance (?x1 ?y1 ?x2 ?y2)
   "Обчислює евклідову відстань"
   (bind ?dx (- ?x2 ?x1))
   (bind ?dy (- ?y2 ?y1))
   (sqrt (+ (* ?dx ?dx) (* ?dy ?dy))))

;; Шаблони та факти Завдання 1
(deftemplate number-item
   (slot id (type SYMBOL))
   (slot value (type INTEGER))
)

(deffacts task1-data
   (number-item (id item-1) (value 5))
   (number-item (id item-2) (value 6))
   (number-item (id item-3) (value 7))
)

;; Правила Завдання 1
(defrule task1-banner
   (declare (salience 300))
=>
   (printout t "================================================================================" crlf)
   (printout t "     МОДУЛЬ 1. ДОСЛІДЖЕННЯ ФУНКЦІЙ КОРИСТУВАЧА ТА РЕКУРСІЇ (ЗАВДАННЯ 1)          " crlf)
   (printout t "================================================================================" crlf)
   (print-args 10 20)
   (print-args alpha beta gamma delta epsilon)
   (printout t "[Евклідова відстань (0,0)-(3,4)] Відстань = " (euclidean-distance 0 0 3 4) crlf)
   (printout t "--------------------------------------------------------------------------------" crlf)
)

(defrule task1-compute-factorials
   (declare (salience 250))
   (number-item (id ?id) (value ?v))
=>
   (bind ?f (factorial ?v))
   (bind ?parity (if (is-even ?v) then "парне" else "непарне"))
   (printout t "[RHS Виклик] " ?id ": число = " ?v " (" ?parity "), факторіал " ?v "! = " ?f crlf)
)

(defrule task1-filter-even-numbers
   (declare (salience 240))
   (number-item (id ?id) (value ?v))
   (test (is-even ?v))
=>
   (printout t "[LHS Фільтр: is-even] Знайдено парний елемент: " ?id " (значення: " ?v ")" crlf)
)

;;; #########################################################
;;; ЧАСТИНА 2. ЗАВДАННЯ 2 (ВАРІАНТ 3): ЕКСПЕРТНА СИСТЕМА ІДЕНТИФІКАЦІЇ ХМАР
;;; #########################################################

;; 2.1. Шаблони даних
(deftemplate cloud
   (slot name (type SYMBOL))
   (slot altitude (type NUMBER) (default -1))
   (slot layer (type SYMBOL) (default unknown)
      (allowed-values low middle high vertical unknown))
   (slot shape (type SYMBOL)
      (allowed-values flat-sheets rounded-heaps wispy))
   (slot precipitation (type SYMBOL) (default no)
      (allowed-values yes no))
   (slot color (type SYMBOL) (default light)
      (allowed-values dark-gray white light other))
)

(deftemplate cloud-result
   (slot name (type SYMBOL))
   (slot ukr-name (type STRING))
   (slot latin-name (type STRING))
   (slot layer-desc (type STRING))
   (slot characteristics (type STRING))
)

;; 2.2. Функції експертної системи
(deffunction altitude-to-layer (?alt)
   "Обчислює висотний ярус за висотою"
   (if (<= ?alt 0)
      then unknown
      else (if (<= ?alt 1800)
              then low
              else (if (<= ?alt 6000)
                      then middle
                      else high))))

(deffunction is-stormy-condition (?precip ?color)
   "Предикат штормових умов"
   (and (eq ?precip yes) (eq ?color dark-gray)))

(deffunction classify-cloud-type (?layer ?shape ?precip ?color)
   "Метеорологічна класифікація 10 типів хмар"
   (if (and (eq ?layer low) (eq ?shape flat-sheets) (eq ?precip no))
      then Stratus
   else (if (and (eq ?layer low) (eq ?shape rounded-heaps) (eq ?precip no))
      then Stratocumulus
   else (if (and (or (eq ?layer middle) (eq ?layer low)) (eq ?shape flat-sheets) (eq ?precip yes) (eq ?color dark-gray))
      then Nimbostratus
   else (if (and (eq ?layer middle) (eq ?shape flat-sheets) (eq ?precip no))
      then Altostratus
   else (if (and (eq ?layer middle) (eq ?shape rounded-heaps) (eq ?precip no))
      then Altocumulus
   else (if (and (eq ?layer high) (eq ?shape wispy) (eq ?precip no))
      then Cirrus
   else (if (and (eq ?layer high) (eq ?shape flat-sheets) (eq ?precip no))
      then Cirrostratus
   else (if (and (eq ?layer high) (eq ?shape rounded-heaps) (eq ?precip no))
      then Cirrocumulus
   else (if (and (eq ?layer vertical) (eq ?shape rounded-heaps) (eq ?precip no))
      then Cumulus
   else (if (and (eq ?layer vertical) (eq ?shape rounded-heaps) (eq ?precip yes) (eq ?color dark-gray))
      then Cumulonimbus
   else Unknown)))))))))))

(deffunction get-cloud-ukr-name (?type)
   (switch ?type
      (case Stratus then "Шаруваті хмари")
      (case Stratocumulus then "Шарувато-купчасті хмари")
      (case Nimbostratus then "Шарувато-дощові хмари")
      (case Altostratus then "Високо-шаруваті хмари")
      (case Altocumulus then "Високо-купчасті хмари")
      (case Cirrus then "Перисті хмари")
      (case Cirrostratus then "Перисто-шаруваті хмари")
      (case Cirrocumulus then "Перисто-купчасті хмари")
      (case Cumulus then "Купчасті хмари")
      (case Cumulonimbus then "Купчасто-дощові хмари")
      (default "Невідомий тип хмари")))

(deffunction get-cloud-latin-name (?type)
   (switch ?type
      (case Stratus then "Stratus (St)")
      (case Stratocumulus then "Stratocumulus (Sc)")
      (case Nimbostratus then "Nimbostratus (Ns)")
      (case Altostratus then "Altostratus (As)")
      (case Altocumulus then "Altocumulus (Ac)")
      (case Cirrus then "Cirrus (Ci)")
      (case Cirrostratus then "Cirrostratus (Cs)")
      (case Cirrocumulus then "Cirrocumulus (Cc)")
      (case Cumulus then "Cumulus (Cu)")
      (case Cumulonimbus then "Cumulonimbus (Cb)")
      (default "Incognita (Inc)")))

(deffunction get-cloud-layer-desc (?layer)
   (switch ?layer
      (case low then "Нижній ярус (<= 1800 м)")
      (case middle then "Середній ярус (1800-6000 м)")
      (case high then "Верхній ярус (> 6000 м)")
      (case vertical then "Хмари вертикального розвитку (від нижнього до верхнього ярусу)")
      (default "Ярус невизначено")))

(deffunction get-cloud-characteristics (?type)
   (switch ?type
      (case Stratus then "Однорідний низький шар сірого кольору, нагадує гладкі рівні листи; опади відсутні.")
      (case Stratocumulus then "Сірі або білуваті хвилі, вали чи округлені купи у нижньому ярусі; без істотних опадів.")
      (case Nimbostratus then "Суцільний темно-сірий покрив великої товщини у формі гладких листів; несе за собою тривалі опади.")
      (case Altostratus then "Сірувата або синювата однорідна пелена (гладкі рівні листи); Сонце просвічує як крізь матове скло.")
      (case Altocumulus then "Білі або сірі пластівці чи округлені купи середнього ярусу, часто вишикувані паралельними рядами.")
      (case Cirrus then "Тонкі білі волокнисті нитки, що виглядають як гінкі пучки волосся або пір'я; крижана структура.")
      (case Cirrostratus then "Прозора білувата гладка пелена верхнього ярусу; часто спричиняє оптичне явище гало.")
      (case Cirrocumulus then "Дрібні тонкі білі округлені купки або хвилі без власних тіней ('баранці') у верхньому ярусі.")
      (case Cumulus then "Окремі щільні яскраво-білі хмари з куполоподібними вершинами у вигляді великих округлених куп.")
      (case Cumulonimbus then "Величезні темно-сірі грозові гори з ковадлом, що несуть зливи, грози та шквали.")
      (default "Характеристики відсутні.")))

(deffunction print-cloud-card (?name ?alt ?layer ?shape ?precip ?color ?ukr ?lat ?layer-desc ?chars $?notes)
   (printout t "-> Спостереження: " ?name crlf)
   (if (> ?alt 0)
      then (printout t "   Параметри: висота = " ?alt " м, ярус = " ?layer ", форма = " ?shape ", опади = " ?precip ", колір = " ?color crlf)
      else (printout t "   Параметри: ярус = " ?layer ", форма = " ?shape ", опади = " ?precip ", колір = " ?color crlf))
   (printout t "   Тип хмари: " ?ukr " [" ?lat "]" crlf)
   (printout t "   Ярус:      " ?layer-desc crlf)
   (printout t "   Опис:      " ?chars crlf)
   (if (> (length$ ?notes) 0)
      then (printout t "   Примітки:  " (nth$ 1 ?notes) crlf))
   (printout t "--------------------------------------------------------------------------------" crlf)
)

(deffunction print-statistics-summary (?total ?low ?mid ?high ?vert $?extra)
   (printout t crlf)
   (printout t "================================================================================" crlf)
   (printout t "                   ПІДСУМКОВА СТАТИСТИКА СПОСТЕРЕЖЕНЬ                           " crlf)
   (printout t "================================================================================" crlf)
   (printout t " Всього опрацьовано хмар:         " ?total crlf)
   (printout t " - Хмари нижнього ярусу:          " ?low " (Stratus, Stratocumulus)" crlf)
   (printout t " - Хмари середнього ярусу:        " ?mid " (Altostratus, Altocumulus, Nimbostratus)" crlf)
   (printout t " - Хмари верхнього ярусу:          " ?high " (Cirrus, Cirrostratus, Cirrocumulus)" crlf)
   (printout t " - Хмари вертикального розвитку:  " ?vert " (Cumulus, Cumulonimbus)" crlf)
   (if (> (length$ ?extra) 0)
      then (printout t " Додаткова інформація:            " (nth$ 1 ?extra) crlf))
   (printout t "================================================================================" crlf)
)

;; 2.3. Вхідні спостереження за хмарами
(deffacts task2-cloud-observations
   (cloud (name Cloud-1) (altitude 1100) (shape flat-sheets) (precipitation no) (color light))
   (cloud (name Cloud-2) (altitude 1600) (shape rounded-heaps) (precipitation no) (color light))
   (cloud (name Cloud-3) (altitude 2800) (shape flat-sheets) (precipitation yes) (color dark-gray))
   (cloud (name Cloud-4) (altitude 3600) (shape flat-sheets) (precipitation no) (color light))
   (cloud (name Cloud-5) (altitude 4200) (shape rounded-heaps) (precipitation no) (color white))
   (cloud (name Cloud-6) (altitude 8200) (shape wispy) (precipitation no) (color white))
   (cloud (name Cloud-7) (altitude 7200) (shape flat-sheets) (precipitation no) (color white))
   (cloud (name Cloud-8) (altitude 6800) (shape rounded-heaps) (precipitation no) (color white))
   (cloud (name Cloud-9) (layer vertical) (shape rounded-heaps) (precipitation no) (color white))
   (cloud (name Cloud-10) (layer vertical) (shape rounded-heaps) (precipitation yes) (color dark-gray))
)

;; 2.4. Продукційні правила експертної системи
(defrule task2-banner
   (declare (salience 150))
=>
   (printout t crlf)
   (printout t "================================================================================" crlf)
   (printout t "   МОДУЛЬ 2. ЕКСПЕРТНА СИСТЕМА ІДЕНТИФІКАЦІЇ ХМАР З ФУНКЦІЯМИ (ЗАВДАННЯ 2)       " crlf)
   (printout t "================================================================================" crlf)
)

(defrule task2-determine-layer-via-function
   (declare (salience 120))
   ?c <- (cloud (name ?n) (altitude ?a&:(> ?a 0)) (layer unknown))
=>
   (bind ?computed-layer (altitude-to-layer ?a))
   (modify ?c (layer ?computed-layer))
   (printout t "[Процедурна абстракція: altitude-to-layer] " ?n " (" ?a " м) -> " ?computed-layer crlf)
)

(defrule task2-alert-storm-condition
   (declare (salience 110))
   (cloud (name ?n) (precipitation ?p) (color ?col))
   (test (is-stormy-condition ?p ?col))
=>
   (printout t "[LHS Предикатний виклик: is-stormy-condition] Увага! Спостереження " ?n " зафіксувало штормові умови!" crlf)
)

(defrule task2-identify-cloud-via-functions
   (declare (salience 80))
   (cloud (name ?n) (altitude ?alt) (layer ?l&~unknown) (shape ?s) (precipitation ?p) (color ?col))
   (not (cloud-result (name ?n)))
=>
   (bind ?type (classify-cloud-type ?l ?s ?p ?col))
   (bind ?ukr (get-cloud-ukr-name ?type))
   (bind ?lat (get-cloud-latin-name ?type))
   (bind ?layer-desc (get-cloud-layer-desc ?l))
   (bind ?chars (get-cloud-characteristics ?type))
   (assert (cloud-result (name ?n) (ukr-name ?ukr) (latin-name ?lat) (layer-desc ?layer-desc) (characteristics ?chars)))
)

(defrule task2-print-results
   (declare (salience 40))
   (cloud-result (name ?n) (ukr-name ?u) (latin-name ?lat) (layer-desc ?ld) (characteristics ?ch))
   (cloud (name ?n) (altitude ?alt) (layer ?l) (shape ?s) (precipitation ?p) (color ?col))
=>
   (print-cloud-card ?n ?alt ?l ?s ?p ?col ?u ?lat ?ld ?ch "Метеорологічна станція WMO-034")
)

(defrule task2-print-summary
   (declare (salience 10))
=>
   (print-statistics-summary 10 2 3 3 2 "Класифікацію всіх 10 спостережень виконано успішно.")
)

(defrule task-final-finish
   (declare (salience 1))
=>
   (printout t crlf)
   (printout t "================================================================================" crlf)
   (printout t "              ВИКОНАННЯ ВСІХ ЗАВДАНЬ ЛАБОРАТОРНОЇ РОБОТИ №4 ЗАВЕРШЕНО           " crlf)
   (printout t "================================================================================" crlf)
)
