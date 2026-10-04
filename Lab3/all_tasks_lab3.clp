;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №3
;;; Комплексний файл виконання всіх завдань (all_tasks_lab3.clp)
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; ---------------------------------------------------------
;;; 1. ШАБЛОНИ ТА ФАКТИ ДЛЯ ЗАВДАННЯ 1 (РОДИННІ ЗВ'ЯЗКИ)
;;; ---------------------------------------------------------

(deftemplate person-gender
   (slot name (type SYMBOL))
   (slot gender (type SYMBOL) (allowed-values male female))
)

(deftemplate parent-of
   (slot parent (type SYMBOL))
   (slot child (type SYMBOL))
)

(deftemplate parent-child
   (slot parent (type SYMBOL))
   (slot child (type SYMBOL))
   (slot relationship (type SYMBOL) (allowed-values father mother))
)

(deftemplate family-role
   (slot name (type SYMBOL))
   (slot role (type SYMBOL) (allowed-values father mother son daughter parent child))
)

(deftemplate parents-of
   (slot child (type SYMBOL))
   (multislot parents (type SYMBOL))
)

(deffacts family-base-facts
   (person-gender (name Tom) (gender male))
   (person-gender (name Susan) (gender female))
   (person-gender (name John) (gender male))
   (parent-of (parent Tom) (child John))
   (parent-of (parent Susan) (child John))
)

;;; ---------------------------------------------------------
;;; 2. ШАБЛОНИ ТА ФАКТИ ДЛЯ ЗАВДАННЯ 2 (ТИПИ ХМАР - ВАРІАНТ 3)
;;; ---------------------------------------------------------

(deftemplate cloud
   (slot name (type SYMBOL))
   (slot altitude (type NUMBER) (default -1))
   (slot layer (type SYMBOL) (default unknown) (allowed-values low middle high vertical unknown))
   (slot shape (type SYMBOL) (allowed-values flat-sheets rounded-heaps wispy))
   (slot precipitation (type SYMBOL) (default no) (allowed-values yes no))
   (slot color (type SYMBOL) (default light) (allowed-values dark-gray white light other))
)

(deftemplate cloud-result
   (slot name (type SYMBOL))
   (slot ukr-name (type STRING))
   (slot latin-name (type STRING))
   (slot layer-desc (type STRING))
   (slot characteristics (type STRING))
)

(deffacts cloud-observations
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

;;; ---------------------------------------------------------
;;; 3. ПРАВИЛА ВИВЕДЕННЯ ДЛЯ ЗАВДАННЯ 1 (SALIENCE 200..150)
;;; ---------------------------------------------------------

(defrule banner-task1 (declare (salience 200)) =>
   (printout t "================================================================================" crlf)
   (printout t "     РЕЗУЛЬТАТИ ЗАВДАННЯ 1: ЛОГІЧНЕ ВИВЕДЕННЯ РОДИННИХ ЗВ'ЯЗКІВ У CLIPS         " crlf)
   (printout t "================================================================================" crlf)
)

(defrule infer-father-relation (declare (salience 190))
   (parent-of (parent ?p) (child ?c))
   (person-gender (name ?p) (gender male))
   (not (parent-child (parent ?p) (child ?c) (relationship father)))
=>
   (assert (parent-child (parent ?p) (child ?c) (relationship father)))
   (printout t "[Завдання 1: Відношення] " ?p " є батьком (father) для " ?c crlf)
)

(defrule infer-mother-relation (declare (salience 190))
   (parent-of (parent ?p) (child ?c))
   (person-gender (name ?p) (gender female))
   (not (parent-child (parent ?p) (child ?c) (relationship mother)))
=>
   (assert (parent-child (parent ?p) (child ?c) (relationship mother)))
   (printout t "[Завдання 1: Відношення] " ?p " є матір'ю (mother) для " ?c crlf)
)

(defrule infer-father-role (declare (salience 180))
   (parent-child (parent ?p) (relationship father))
   (not (family-role (name ?p) (role father)))
=>
   (assert (family-role (name ?p) (role father)))
   (printout t "[Завдання 1: Роль] " ?p " набуває соціальної ролі father" crlf)
)

(defrule infer-mother-role (declare (salience 180))
   (parent-child (parent ?p) (relationship mother))
   (not (family-role (name ?p) (role mother)))
=>
   (assert (family-role (name ?p) (role mother)))
   (printout t "[Завдання 1: Роль] " ?p " набуває соціальної ролі mother" crlf)
)

(defrule infer-son-role (declare (salience 180))
   (parent-of (parent ?p) (child ?c))
   (person-gender (name ?c) (gender male))
   (not (family-role (name ?c) (role son)))
=>
   (assert (family-role (name ?c) (role son)))
   (printout t "[Завдання 1: Роль] " ?c " набуває соціальної ролі son" crlf)
)

(defrule infer-joint-parents (declare (salience 170))
   (parent-child (parent ?f) (child ?c) (relationship father))
   (parent-child (parent ?m) (child ?c) (relationship mother))
   (not (parents-of (child ?c)))
=>
   (assert (parents-of (child ?c) (parents ?f ?m)))
   (printout t "[Завдання 1: Батьки] Батьками (parents) для " ?c " є " ?f " та " ?m crlf)
)

(defrule verify-task1 (declare (salience 160))
   (parents-of (child John) (parents Tom Susan))
   (family-role (name Tom) (role father))
   (family-role (name Susan) (role mother))
   (family-role (name John) (role son))
=>
   (printout t "[Верифікація Завдання 1] Усі 9 тверджень успішно виведені продукційними правилами!" crlf)
)

;;; ---------------------------------------------------------
;;; 4. ПРАВИЛА ВИВЕДЕННЯ ДЛЯ ЗАВДАННЯ 2 (SALIENCE 120..10)
;;; ---------------------------------------------------------

(defrule banner-task2 (declare (salience 120)) =>
   (printout t crlf)
   (printout t "================================================================================" crlf)
   (printout t "     РЕЗУЛЬТАТИ ЗАВДАННЯ 2 (ВАРІАНТ 3): ЕКСПЕРТНА СИСТЕМА ІДЕНТИФІКАЦІЇ ХМАР    " crlf)
   (printout t "================================================================================" crlf)
)

;; Абстракція висот (Salience 110)
(defrule determine-low-layer (declare (salience 110))
   ?c <- (cloud (name ?n) (altitude ?a&:(and (<= ?a 1800) (> ?a 0))) (layer unknown))
=>
   (modify ?c (layer low))
)

(defrule determine-middle-layer (declare (salience 110))
   ?c <- (cloud (name ?n) (altitude ?a&:(and (> ?a 1800) (<= ?a 6000))) (layer unknown))
=>
   (modify ?c (layer middle))
)

(defrule determine-high-layer (declare (salience 110))
   ?c <- (cloud (name ?n) (altitude ?a&:(> ?a 6000)) (layer unknown))
=>
   (modify ?c (layer high))
)

;; Класифікація (Salience 60..50)
(defrule identify-nimbostratus (declare (salience 60))
   (cloud (name ?n) (layer middle|low) (shape flat-sheets) (precipitation yes) (color dark-gray))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Шарувато-дощові хмари")
      (latin-name "Nimbostratus (Ns)")
      (layer-desc "Середній ярус (1800-6000 м) з опусканням у нижній")
      (characteristics "Суцільний темно-сірий покрив великої товщини у формі гладких листів; несе тривалі опади.")
   ))
)

(defrule identify-cumulonimbus (declare (salience 60))
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

(defrule identify-stratus (declare (salience 50))
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

(defrule identify-stratocumulus (declare (salience 50))
   (cloud (name ?n) (layer low) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Шарувато-купчасті хмари")
      (latin-name "Stratocumulus (Sc)")
      (layer-desc "Нижній ярус (<= 1800 м)")
      (characteristics "Сірі або білуваті хвилі чи пасма з округленими купами у нижньому ярусі; без істотних опадів.")
   ))
)

(defrule identify-altostratus (declare (salience 50))
   (cloud (name ?n) (layer middle) (shape flat-sheets) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Високо-шаруваті хмари")
      (latin-name "Altostratus (As)")
      (layer-desc "Середній ярус (1800-6000 м)")
      (characteristics "Сірувата однорідна пелена (гладкі рівні листи); Сонце просвічує як крізь матове скло.")
   ))
)

(defrule identify-altocumulus (declare (salience 50))
   (cloud (name ?n) (layer middle) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Високо-купчасті хмари")
      (latin-name "Altocumulus (Ac)")
      (layer-desc "Середній ярус (1800-6000 м)")
      (characteristics "Білі або сірі пластівці чи округлені купи середнього ярусу, часто вишикувані рядами.")
   ))
)

(defrule identify-cirrus (declare (salience 50))
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

(defrule identify-cirrostratus (declare (salience 50))
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

(defrule identify-cirrocumulus (declare (salience 50))
   (cloud (name ?n) (layer high) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Перисто-купчасті хмари")
      (latin-name "Cirrocumulus (Cc)")
      (layer-desc "Верхній ярус (> 6000 м)")
      (characteristics "Дрібні тонкі білі округлені купки без власних тіней ('баранці') у верхньому ярусі.")
   ))
)

(defrule identify-cumulus (declare (salience 50))
   (cloud (name ?n) (layer vertical) (shape rounded-heaps) (precipitation no))
   (not (cloud-result (name ?n)))
=>
   (assert (cloud-result 
      (name ?n)
      (ukr-name "Купчасті хмари")
      (latin-name "Cumulus (Cu)")
      (layer-desc "Хмари вертикального розвитку (від нижнього до середнього/верхнього ярусу)")
      (characteristics "Окремі щільні яскраво-білі хмари з куполоподібними вершинами у вигляді округлених куп.")
   ))
)

;; Друк результатів класифікації хмар (Salience 10)
(defrule print-cloud-result (declare (salience 10))
   (cloud-result (name ?n) (ukr-name ?ukr) (latin-name ?lat) (layer-desc ?ld) (characteristics ?ch))
   (cloud (name ?n) (altitude ?alt) (layer ?lay) (shape ?sh) (precipitation ?prec) (color ?col))
=>
   (printout t "-> [" ?n "] Тип: " ?ukr " (" ?lat ")" crlf)
   (printout t "   Атрибути: ярус = " ?lay ", форма = " ?sh ", опади = " ?prec ", колір = " ?col crlf)
   (printout t "   Ознаки: " ?ch crlf)
   (printout t "--------------------------------------------------------------------------------" crlf)
)

(defrule banner-finish (declare (salience 1)) =>
   (printout t "================================================================================" crlf)
   (printout t "              ВИКОНАННЯ ВСІХ ЗАВДАНЬ ЛАБОРАТОРНОЇ РОБОТИ №3 ЗАВЕРШЕНО           " crlf)
   (printout t "================================================================================" crlf)
)
