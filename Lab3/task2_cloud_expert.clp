;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №3
;;; Завдання 2 (Варіант 3): Експертна система ідентифікації типів хмар
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; ---------------------------------------------------------
;;; Шаблони даних (deftemplate)
;;; ---------------------------------------------------------

;; Шаблон спостереження хмари з атрибутами
(deftemplate cloud
   (slot name (type SYMBOL))                                   ; Ідентифікатор спостереження
   (slot altitude (type NUMBER) (default -1))                  ; Висота основи в метрах (-1 якщо не вказано)
   (slot layer (type SYMBOL) (default unknown)                 ; Ярус хмари
      (allowed-values low middle high vertical unknown))
   (slot shape (type SYMBOL)                                   ; Зовнішній вигляд / форма
      (allowed-values flat-sheets rounded-heaps wispy))
   (slot precipitation (type SYMBOL) (default no)              ; Наявність опадів
      (allowed-values yes no))
   (slot color (type SYMBOL) (default light)                   ; Колір хмари
      (allowed-values dark-gray white light other))
)

;; Шаблон результату ідентифікації експертною системою
(deftemplate cloud-result
   (slot name (type SYMBOL))                                   ; Ідентифікатор спостереження
   (slot ukr-name (type STRING))                               ; Назва типу хмари українською
   (slot latin-name (type STRING))                             ; Міжнародне латинське позначення
   (slot layer-desc (type STRING))                             ; Опис висотного ярусу
   (slot characteristics (type STRING))                        ; Характерні морфологічні ознаки
)

;;; ---------------------------------------------------------
;;; Вхідні факти (deffacts): Набір спостережень за хмарами
;;; ---------------------------------------------------------
(deffacts cloud-observations
   ;; 1. Хмара-1: висота 1100 м (<=1800), форма - гладкі листи, без опадів
   (cloud (name Cloud-1) (altitude 1100) (shape flat-sheets) (precipitation no) (color light))

   ;; 2. Хмара-2: висота 1600 м (<=1800), форма - округлені купи, без опадів
   (cloud (name Cloud-2) (altitude 1600) (shape rounded-heaps) (precipitation no) (color light))

   ;; 3. Хмара-3: висота 2800 м (1800-6000), гладкі листи, обложний дощ, темно-сіра
   (cloud (name Cloud-3) (altitude 2800) (shape flat-sheets) (precipitation yes) (color dark-gray))

   ;; 4. Хмара-4: висота 3600 м (1800-6000), гладкі рівні листи, без опадів
   (cloud (name Cloud-4) (altitude 3600) (shape flat-sheets) (precipitation no) (color light))

   ;; 5. Хмара-5: висота 4200 м (1800-6000), округлені купи, без опадів
   (cloud (name Cloud-5) (altitude 4200) (shape rounded-heaps) (precipitation no) (color white))

   ;; 6. Хмара-6: висота 8200 м (>6000), гінкі пучки волосся, без опадів
   (cloud (name Cloud-6) (altitude 8200) (shape wispy) (precipitation no) (color white))

   ;; 7. Хмара-7: висота 7200 м (>6000), гладка пелена (листи), без опадів
   (cloud (name Cloud-7) (altitude 7200) (shape flat-sheets) (precipitation no) (color white))

   ;; 8. Хмара-8: висота 6800 м (>6000), округлені дрібні купи, без опадів
   (cloud (name Cloud-8) (altitude 6800) (shape rounded-heaps) (precipitation no) (color white))

   ;; 9. Хмара-9: вертикальний розвиток від низу до верху, округлені купи, без опадів, біла
   (cloud (name Cloud-9) (layer vertical) (shape rounded-heaps) (precipitation no) (color white))

   ;; 10. Хмара-10: вертикальний розвиток, округлені купи, злива/гроза, темно-сіра
   (cloud (name Cloud-10) (layer vertical) (shape rounded-heaps) (precipitation yes) (color dark-gray))
)

;;; ---------------------------------------------------------
;;; 1. Правила первинної обробки: визначення висотного ярусу
;;; ---------------------------------------------------------

;; Правило для нижнього ярусу (висота <= 1800 м)
(defrule determine-low-layer
   (declare (salience 100))
   ?c <- (cloud (name ?n) (altitude ?a&:(and (<= ?a 1800) (> ?a 0))) (layer unknown))
=>
   (modify ?c (layer low))
   (printout t "[Абстракція] Хмара " ?n " (висота " ?a " м) віднесена до НИЖНЬОГО ярусу" crlf)
)

;; Правило для середнього ярусу (1800 м < висота <= 6000 м)
(defrule determine-middle-layer
   (declare (salience 100))
   ?c <- (cloud (name ?n) (altitude ?a&:(and (> ?a 1800) (<= ?a 6000))) (layer unknown))
=>
   (modify ?c (layer middle))
   (printout t "[Абстракція] Хмара " ?n " (висота " ?a " м) віднесена до СЕРЕДНЬОГО ярусу" crlf)
)

;; Правило для верхнього ярусу (висота > 6000 м)
(defrule determine-high-layer
   (declare (salience 100))
   ?c <- (cloud (name ?n) (altitude ?a&:(> ?a 6000)) (layer unknown))
=>
   (modify ?c (layer high))
   (printout t "[Абстракція] Хмара " ?n " (висота " ?a " м) віднесена до ВЕРХНЬОГО ярусу" crlf)
)

;;; ---------------------------------------------------------
;;; 2. Продукційні правила класифікації та ідентифікації
;;; ---------------------------------------------------------

;; 1. Шаруваті хмари (Stratus)
(defrule identify-stratus
   (declare (salience 50))
   (cloud (name ?n) (layer low) (shape flat-sheets) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Шаруваті хмари")
      (latin-name "Stratus (St)")
      (layer-desc "Нижній ярус (<= 1800 м)")
      (characteristics "Однорідний низький шар сірого кольору, нагадує гладкі рівні листи; опади відсутні.")
   ))
)

;; 2. Шарувато-купчасті хмари (Stratocumulus)
(defrule identify-stratocumulus
   (declare (salience 50))
   (cloud (name ?n) (layer low) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Шарувато-купчасті хмари")
      (latin-name "Stratocumulus (Sc)")
      (layer-desc "Нижній ярус (<= 1800 м)")
      (characteristics "Сірі або білуваті хвилі, вали чи округлені купи у нижньому ярусі; без істотних опадів.")
   ))
)

;; 3. Шарувато-дощові хмари (Nimbostratus)
(defrule identify-nimbostratus
   (declare (salience 60))
   (cloud (name ?n) (layer middle|low) (shape flat-sheets) (precipitation yes) (color dark-gray))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Шарувато-дощові хмари")
      (latin-name "Nimbostratus (Ns)")
      (layer-desc "Середній ярус (1800-6000 м) з опусканням у нижній")
      (characteristics "Суцільний темно-сірий покрив великої товщини у формі гладких листів; несе за собою тривалі опади.")
   ))
)

;; 4. Високо-шаруваті хмари (Altostratus)
(defrule identify-altostratus
   (declare (salience 50))
   (cloud (name ?n) (layer middle) (shape flat-sheets) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Високо-шаруваті хмари")
      (latin-name "Altostratus (As)")
      (layer-desc "Середній ярус (1800-6000 м)")
      (characteristics "Сірувата або синювата однорідна пелена (гладкі рівні листи); Сонце просвічує як крізь матове скло.")
   ))
)

;; 5. Високо-купчасті хмари (Altocumulus)
(defrule identify-altocumulus
   (declare (salience 50))
   (cloud (name ?n) (layer middle) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Високо-купчасті хмари")
      (latin-name "Altocumulus (Ac)")
      (layer-desc "Середній ярус (1800-6000 м)")
      (characteristics "Білі або сірі пластівці чи округлені купи середнього ярусу, часто вишикувані паралельними рядами.")
   ))
)

;; 6. Перисті хмари (Cirrus)
(defrule identify-cirrus
   (declare (salience 50))
   (cloud (name ?n) (layer high) (shape wispy) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Перисті хмари")
      (latin-name "Cirrus (Ci)")
      (layer-desc "Верхній ярус (> 6000 м)")
      (characteristics "Тонкі білі волокнисті нитки, що виглядають як гінкі пучки волосся або пір'я; крижана структура.")
   ))
)

;; 7. Перисто-шаруваті хмари (Cirrostratus)
(defrule identify-cirrostratus
   (declare (salience 50))
   (cloud (name ?n) (layer high) (shape flat-sheets) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Перисто-шаруваті хмари")
      (latin-name "Cirrostratus (Cs)")
      (layer-desc "Верхній ярус (> 6000 м)")
      (characteristics "Прозора білувата гладка пелена верхнього ярусу; часто спричиняє оптичне явище гало.")
   ))
)

;; 8. Перисто-купчасті хмари (Cirrocumulus)
(defrule identify-cirrocumulus
   (declare (salience 50))
   (cloud (name ?n) (layer high) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Перисто-купчасті хмари")
      (latin-name "Cirrocumulus (Cc)")
      (layer-desc "Верхній ярус (> 6000 м)")
      (characteristics "Дрібні тонкі білі округлені купки або хвилі без власних тіней ('баранці') у верхньому ярусі.")
   ))
)

;; 9. Купчасті хмари (Cumulus)
(defrule identify-cumulus
   (declare (salience 50))
   (cloud (name ?n) (layer vertical) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Купчасті хмари")
      (latin-name "Cumulus (Cu)")
      (layer-desc "Хмари вертикального розвитку (від нижнього до середнього/верхнього ярусу)")
      (characteristics "Окремі щільні яскраво-білі хмари з куполоподібними вершинами у вигляді великих округлених куп.")
   ))
)

;; 10. Купчасто-дощові хмари (Cumulonimbus)
(defrule identify-cumulonimbus
   (declare (salience 60))
   (cloud (name ?n) (layer vertical) (shape rounded-heaps) (precipitation yes) (color dark-gray))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Купчасто-дощові хмари")
      (latin-name "Cumulonimbus (Cb)")
      (layer-desc "Хмари потужного вертикального розвитку (від нижнього до верхнього ярусу)")
      (characteristics "Величезні темно-сірі грозові гори з ковадлом, що несуть зливи, грози та шквали.")
   ))
)

;;; ---------------------------------------------------------
;;; 3. Правила виведення та звітності
;;; ---------------------------------------------------------

(defrule print-banner (declare (salience 20)) =>
   (printout t "================================================================================" crlf)
   (printout t "               РЕЗУЛЬТАТИ ІДЕНТИФІКАЦІЇ ТИПІВ ХМАР У СИСТЕМІ CLIPS               " crlf)
   (printout t "================================================================================" crlf)
)

(defrule print-cloud-result
   (declare (salience 10))
   (cloud-result (name ?n) (ukr-name ?ukr) (latin-name ?lat) (layer-desc ?ld) (characteristics ?ch))
   (cloud (name ?n) (altitude ?alt) (layer ?lay) (shape ?sh) (precipitation ?prec) (color ?col))
=>
   (printout t "-> Спостереження: " ?n crlf)
   (if (> ?alt 0)
      then (printout t "   Параметри: висота = " ?alt " м, ярус = " ?lay ", форма = " ?sh ", опади = " ?prec ", колір = " ?col crlf)
      else (printout t "   Параметри: ярус = " ?lay ", форма = " ?sh ", опади = " ?prec ", колір = " ?col crlf)
   )
   (printout t "   Тип хмари: " ?ukr " [" ?lat "]" crlf)
   (printout t "   Ярус:      " ?ld crlf)
   (printout t "   Опис:      " ?ch crlf)
   (printout t "--------------------------------------------------------------------------------" crlf)
)
