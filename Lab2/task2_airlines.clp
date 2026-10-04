;;; =========================================================
;;; Дисципліна: Інтелектуальні інформаційні технології
;;; Лабораторна робота №2
;;; Завдання 2 (Варіант 3): Мережа авіаліній (рис. 1)
;;; Студент: Святішенко, група ІН-32-2
;;; =========================================================

;;; Єдина конструкція deftemplate для опису графа маршрутів авіаліній
(deftemplate flight
   (slot from (type SYMBOL))
   (slot to (type SYMBOL))
)

;;; Ініціалізація орієнтованого графа рейсів (рис. 1)
(deffacts airline-network
   ;; Маршрути з міста Сан-Франциско
   (flight (from San-Francisco) (to Chicago))
   (flight (from San-Francisco) (to Los-Angeles))

   ;; Маршрути з міста Чикаго
   (flight (from Chicago) (to Indianapolis))

   ;; Маршрути з міста Індіанаполіс
   (flight (from Indianapolis) (to Houston))

   ;; Маршрути з міста Лос-Анджелес
   (flight (from Los-Angeles) (to Houston))

   ;; Маршрути з міста Х'юстон
   (flight (from Houston) (to San-Francisco))
   (flight (from Houston) (to New-York))

   ;; Маршрути з міста Нью-Йорк
   (flight (from New-York) (to Chicago))
)
