;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №2
;;; Завдання 1: Представлення родинних зв'язків у вигляді фактів
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; Група 1: Біологічна стать осіб (Tom, Susan, John)
;;; "Tom is a male.", "Susan is a female.", "John is a male."
(deftemplate person-gender
   (slot name (type SYMBOL))
   (slot gender (type SYMBOL) (allowed-values male female))
)

;;; Група 2: Сімейний статус та соціальні ролі в родині
;;; "Tom is a father.", "Susan is a mother.", "John is a son."
(deftemplate family-role
   (slot name (type SYMBOL))
   (slot role (type SYMBOL) (allowed-values father mother son daughter parent child))
)

;;; Група 3: Батьківські відносини між парою "батько/мати - дитина"
;;; "The father of John is Tom.", "The mother of John is Susan."
(deftemplate parent-child
   (slot parent (type SYMBOL))
   (slot child (type SYMBOL))
   (slot relationship (type SYMBOL) (allowed-values father mother))
)

;;; Група 4: Спільне батьківство над дитиною
;;; "The parents of John are Tom and Susan."
(deftemplate parents-of
   (slot child (type SYMBOL))
   (multislot parents (type SYMBOL))
)

;;; Ініціалізація початкових фактів для Завдання 1
(deffacts family-facts
   ;; Факти групи 1: стать
   (person-gender (name Tom) (gender male))
   (person-gender (name Susan) (gender female))
   (person-gender (name John) (gender male))

   ;; Факти групи 2: ролі
   (family-role (name Tom) (role father))
   (family-role (name Susan) (role mother))
   (family-role (name John) (role son))

   ;; Факти групи 3: індивідуальні батьківські зв'язки
   (parent-child (parent Tom) (child John) (relationship father))
   (parent-child (parent Susan) (child John) (relationship mother))

   ;; Факти групи 4: спільні батьки
   (parents-of (child John) (parents Tom Susan))
)

;;; Правила для виведення та верифікації фактів
(defrule print-parents-info
   (parents-of (child ?c) (parents $?p))
=>
   (printout t "[Завдання 1: Батьки] Батьками " ?c " є: " (implode$ ?p) crlf)
)

(defrule print-parent-child-info
   (parent-child (parent ?p) (child ?c) (relationship ?rel))
=>
   (printout t "[Завдання 1: Родина] " ?rel " дитини " ?c " -> " ?p crlf)
)

(defrule print-person-role-and-gender
   (family-role (name ?n) (role ?r))
   (person-gender (name ?n) (gender ?g))
=>
   (printout t "[Завдання 1: Особа] " ?n ": роль = " ?r ", стать = " ?g crlf)
)
