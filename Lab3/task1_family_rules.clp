;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №3
;;; Завдання 1: Реалізація родинних зв'язків у вигляді правил (defrule)
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; ---------------------------------------------------------
;;; Шаблони deftemplate для представлення знань про родину
;;; ---------------------------------------------------------

;; Шаблон особи з фіксацією статі
(deftemplate person-gender
   (slot name (type SYMBOL))
   (slot gender (type SYMBOL) (allowed-values male female))
)

;; Базове відношення біологічного зв'язку батьківства
(deftemplate parent-of
   (slot parent (type SYMBOL))
   (slot child (type SYMBOL))
)

;; Індивідуальне відношення батько/мати - дитина
(deftemplate parent-child
   (slot parent (type SYMBOL))
   (slot child (type SYMBOL))
   (slot relationship (type SYMBOL) (allowed-values father mother))
)

;; Соціальний статус та сімейна роль особи
(deftemplate family-role
   (slot name (type SYMBOL))
   (slot role (type SYMBOL) (allowed-values father mother son daughter parent child))
)

;; Спільне батьківство над дитиною (пара батьків)
(deftemplate parents-of
   (slot child (type SYMBOL))
   (multislot parents (type SYMBOL))
)

;;; ---------------------------------------------------------
;;; Базові (примітивні) факти: первинний опис родини
;;; ---------------------------------------------------------
(deffacts family-base-facts
   ;; 1. Біологічна стать осіб (первинні твердження)
   (person-gender (name Tom) (gender male))
   (person-gender (name Susan) (gender female))
   (person-gender (name John) (gender male))

   ;; 2. Базовий зв'язок батьківства (генеалогічні дуги)
   (parent-of (parent Tom) (child John))
   (parent-of (parent Susan) (child John))
)

;;; ---------------------------------------------------------
;;; Продукційні правила (defrule) логічного виведення
;;; ---------------------------------------------------------

;; Правило 1: Виведення відношення "The father of John is Tom"
(defrule infer-father-relation
   (declare (salience 50))
   (parent-of (parent ?p) (child ?c))
   (person-gender (name ?p) (gender male))
   (not (parent-child (parent ?p) (child ?c) (relationship father)))
=>
   (assert (parent-child (parent ?p) (child ?c) (relationship father)))
   (printout t "[Правило infer-father-relation] Виведено: " ?p " є батьком (father) для " ?c crlf)
)

;; Правило 2: Виведення відношення "The mother of John is Susan"
(defrule infer-mother-relation
   (declare (salience 50))
   (parent-of (parent ?p) (child ?c))
   (person-gender (name ?p) (gender female))
   (not (parent-child (parent ?p) (child ?c) (relationship mother)))
=>
   (assert (parent-child (parent ?p) (child ?c) (relationship mother)))
   (printout t "[Правило infer-mother-relation] Виведено: " ?p " є матір'ю (mother) для " ?c crlf)
)

;; Правило 3: Виведення статусу/ролі "Tom is a father"
(defrule infer-father-role
   (declare (salience 40))
   (parent-child (parent ?p) (relationship father))
   (not (family-role (name ?p) (role father)))
=>
   (assert (family-role (name ?p) (role father)))
   (printout t "[Правило infer-father-role] Виведено: " ?p " набуває ролі father" crlf)
)

;; Правило 4: Виведення статусу/ролі "Susan is a mother"
(defrule infer-mother-role
   (declare (salience 40))
   (parent-child (parent ?p) (relationship mother))
   (not (family-role (name ?p) (role mother)))
=>
   (assert (family-role (name ?p) (role mother)))
   (printout t "[Правило infer-mother-role] Виведено: " ?p " набуває ролі mother" crlf)
)

;; Правило 5: Виведення статусу/ролі "John is a son"
(defrule infer-son-role
   (declare (salience 40))
   (parent-of (parent ?p) (child ?c))
   (person-gender (name ?c) (gender male))
   (not (family-role (name ?c) (role son)))
=>
   (assert (family-role (name ?c) (role son)))
   (printout t "[Правило infer-son-role] Виведено: " ?c " набуває ролі son" crlf)
)

;; Правило 6: Виведення спільного батьківства "The parents of John are Tom and Susan"
(defrule infer-joint-parents
   (declare (salience 30))
   (parent-child (parent ?f) (child ?c) (relationship father))
   (parent-child (parent ?m) (child ?c) (relationship mother))
   (not (parents-of (child ?c)))
=>
   (assert (parents-of (child ?c) (parents ?f ?m)))
   (printout t "[Правило infer-joint-parents] Виведено: Батьками (parents) для " ?c " є " ?f " та " ?m crlf)
)

;; Правило 7: Підсумкова верифікація виведених знань
(defrule verify-all-nine-statements
   (declare (salience 10))
   (person-gender (name Tom) (gender male))
   (person-gender (name Susan) (gender female))
   (person-gender (name John) (gender male))
   (parent-child (parent Tom) (child John) (relationship father))
   (parent-child (parent Susan) (child John) (relationship mother))
   (family-role (name Tom) (role father))
   (family-role (name Susan) (role mother))
   (family-role (name John) (role son))
   (parents-of (child John) (parents Tom Susan))
=>
   (printout t "==================================================" crlf)
   (printout t " ВЕРИФІКАЦІЯ: Усі 9 тверджень про родину успішно  " crlf)
   (printout t " виведені за допомогою продукційних правил CLIPS! " crlf)
   (printout t "==================================================" crlf)
)
