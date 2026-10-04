;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №4
;;; Завдання 2 (Варіант 3): Експертна система ідентифікації типів хмар
;;; з використанням функцій користувача (deffunction)
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; ---------------------------------------------------------
;;; Шаблони даних (deftemplate)
;;; ---------------------------------------------------------

;; Шаблон спостереження хмари з виміряними або спостережуваними атрибутами
(deftemplate cloud
   (slot name (type SYMBOL))                                   ; Унікальний ідентифікатор спостереження
   (slot altitude (type NUMBER) (default -1))                  ; Висота основи хмари в метрах (-1 якщо не виміряна)
   (slot layer (type SYMBOL) (default unknown)                 ; Висотний ярус хмари
      (allowed-values low middle high vertical unknown))
   (slot shape (type SYMBOL)                                   ; Візуальна геометрична форма / морфологія
      (allowed-values flat-sheets rounded-heaps wispy))
   (slot precipitation (type SYMBOL) (default no)              ; Наявність або відсутність опадів
      (allowed-values yes no))
   (slot color (type SYMBOL) (default light)                   ; Колір хмари
      (allowed-values dark-gray white light other))
)

;; Шаблон результату класифікації експертною системою
(deftemplate cloud-result
   (slot name (type SYMBOL))                                   ; Ідентифікатор спостереження
   (slot ukr-name (type STRING))                               ; Назва типу хмари українською мовою
   (slot latin-name (type STRING))                             ; Міжнародне латинське позначення (WMO)
   (slot layer-desc (type STRING))                             ; Опис висотного діапазону / ярусу
   (slot characteristics (type STRING))                        ; Детальні морфологічні ознаки
)

;;; ---------------------------------------------------------
;;; Функції користувача (deffunction)
;;; ---------------------------------------------------------

;; 1. Функція обчислення висотного ярусу за висотою основи хмари
(deffunction altitude-to-layer (?alt)
   "Перетворює числове значення висоти у метрах у символьне позначення ярусу"
   (if (<= ?alt 0)
      then unknown
      else (if (<= ?alt 1800)
              then low
              else (if (<= ?alt 6000)
                      then middle
                      else high))))

;; 2. Предикатна функція виявлення штормових / грозових умов
(deffunction is-stormy-condition (?precip ?color)
   "Повертає TRUE, якщо зафіксовано опади за темно-сірого кольору хмари"
   (and (eq ?precip yes) (eq ?color dark-gray)))

;; 3. Функція експертної класифікації типу хмари
(deffunction classify-cloud-type (?layer ?shape ?precip ?color)
   "Здійснює експертну класифікацію 10 типів хмар за комплексом метеорологічних ознак"
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

;; 4. Допоміжні функції формування метеорологічного опису
(deffunction get-cloud-ukr-name (?type)
   "Повертає україномовну назву типу хмари"
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
   "Повертає міжнародне латинське найменування та кодове позначення"
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
   "Повертає текстовий опис висотного ярусу"
   (switch ?layer
      (case low then "Нижній ярус (<= 1800 м)")
      (case middle then "Середній ярус (1800-6000 м)")
      (case high then "Верхній ярус (> 6000 м)")
      (case vertical then "Хмари вертикального розвитку (від нижнього до верхнього ярусу)")
      (default "Ярус невизначено")))

(deffunction get-cloud-characteristics (?type)
   "Повертає вичерпні морфологічні характеристики типу хмари"
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

;; 5. Функція форматованого друку картки ідентифікації (обов'язкові та груповий параметр)
(deffunction print-cloud-card (?name ?alt ?layer ?shape ?precip ?color ?ukr ?lat ?layer-desc ?chars $?notes)
   "Виводить структурований паспорт ідентифікованого спостереження"
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

;; 6. Функція формування зведеної метеорологічної статистики
(deffunction print-statistics-summary (?total ?low ?mid ?high ?vert $?extra)
   "Формує зведений аналітичний звіт за всіма спостереженнями"
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

;;; ---------------------------------------------------------
;;; Вхідні факти (deffacts): Набір метеорологічних спостережень
;;; ---------------------------------------------------------
(deffacts cloud-observations
   ;; 1. Хмара-1: висота 1100 м (<=1800), гладкі листи, без опадів, світла
   (cloud (name Cloud-1) (altitude 1100) (shape flat-sheets) (precipitation no) (color light))

   ;; 2. Хмара-2: висота 1600 м (<=1800), округлені купи, без опадів, світла
   (cloud (name Cloud-2) (altitude 1600) (shape rounded-heaps) (precipitation no) (color light))

   ;; 3. Хмара-3: висота 2800 м (1800-6000), гладкі листи, затяжний дощ, темно-сіра
   (cloud (name Cloud-3) (altitude 2800) (shape flat-sheets) (precipitation yes) (color dark-gray))

   ;; 4. Хмара-4: висота 3600 м (1800-6000), гладкі рівні листи, без опадів, світла
   (cloud (name Cloud-4) (altitude 3600) (shape flat-sheets) (precipitation no) (color light))

   ;; 5. Хмара-5: висота 4200 м (1800-6000), округлені купи, без опадів, біла
   (cloud (name Cloud-5) (altitude 4200) (shape rounded-heaps) (precipitation no) (color white))

   ;; 6. Хмара-6: висота 8200 м (>6000), гінкі пучки волосся, без опадів, біла
   (cloud (name Cloud-6) (altitude 8200) (shape wispy) (precipitation no) (color white))

   ;; 7. Хмара-7: висота 7200 м (>6000), гладка пелена, без опадів, біла
   (cloud (name Cloud-7) (altitude 7200) (shape flat-sheets) (precipitation no) (color white))

   ;; 8. Хмара-8: висота 6800 м (>6000), округлені купи, без опадів, біла
   (cloud (name Cloud-8) (altitude 6800) (shape rounded-heaps) (precipitation no) (color white))

   ;; 9. Хмара-9: вертикальний розвиток від низу до верху, округлені купи, без опадів, біла
   (cloud (name Cloud-9) (layer vertical) (shape rounded-heaps) (precipitation no) (color white))

   ;; 10. Хмара-10: вертикальний розвиток, округлені купи, шквалиста злива/гроза, темно-сіра
   (cloud (name Cloud-10) (layer vertical) (shape rounded-heaps) (precipitation yes) (color dark-gray))
)

;;; ---------------------------------------------------------
;;; Продукційні правила з використанням функцій
;;; ---------------------------------------------------------

;; 1. Процедурне визначення ярусу через функцію altitude-to-layer (salience 100)
(defrule rule-determine-layer-via-function
   (declare (salience 100))
   ?c <- (cloud (name ?n) (altitude ?a&:(> ?a 0)) (layer unknown))
=>
   (bind ?computed-layer (altitude-to-layer ?a))
   (modify ?c (layer ?computed-layer))
   (printout t "[Процедурна абстракція: altitude-to-layer] " ?n " (" ?a " м) -> " ?computed-layer crlf)
)

;; 2. Попередження про штормові хмари з викликом функції у LHS (salience 90)
(defrule rule-alert-storm-condition
   (declare (salience 90))
   (cloud (name ?n) (precipitation ?p) (color ?col))
   (test (is-stormy-condition ?p ?col))
=>
   (printout t "[LHS Предикатний виклик: is-stormy-condition] Увага! Спостереження " ?n " зафіксувало штормові/дощові умови!" crlf)
)

;; 3. Друк інформаційного заголовка (salience 70)
(defrule rule-print-header
   (declare (salience 70))
=>
   (printout t crlf)
   (printout t "================================================================================" crlf)
   (printout t "      РЕЗУЛЬТАТИ ДІАГНОСТИКИ ХМАР З ВИКОРИСТАННЯМ ФУНКЦІЙ У СИСТЕМІ CLIPS       " crlf)
   (printout t "================================================================================" crlf)
)

;; 4. Експертна класифікація з викликом функцій у RHS (salience 50)
(defrule rule-identify-cloud-via-functions
   (declare (salience 50))
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

;; 5. Виведення карток спостережень через функцію print-cloud-card (salience 20)
(defrule rule-print-results
   (declare (salience 20))
   (cloud-result (name ?n) (ukr-name ?u) (latin-name ?lat) (layer-desc ?ld) (characteristics ?ch))
   (cloud (name ?n) (altitude ?alt) (layer ?l) (shape ?s) (precipitation ?p) (color ?col))
=>
   (print-cloud-card ?n ?alt ?l ?s ?p ?col ?u ?lat ?ld ?ch "Метеорологічна станція WMO-034")
)

;; 6. Підсумкова статистика через функцію print-statistics-summary (salience 5)
(defrule rule-print-summary
   (declare (salience 5))
=>
   (print-statistics-summary 10 2 3 3 2 "Класифікацію всіх 10 спостережень виконано успішно.")
)
