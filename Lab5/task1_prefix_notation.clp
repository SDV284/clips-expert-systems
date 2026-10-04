;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №5
;;; Загальне завдання: перетворення інфіксних виразів у префіксні
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

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
   (declare (salience 10))
   ?e <- (expression (id ?id) (infix ?inf) (status new))
   =>
   (bind ?pref (infix-to-prefix ?inf))
   (modify ?e (auto-prefix ?pref) (status converted)))

;; Правило 2: обчислення обох префіксних записів та порівняння результатів
(defrule evaluate-and-verify
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
