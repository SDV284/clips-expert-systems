;;; Завдання 4 - впорядковані факти; deftemplate, slots
(deftemplate person
   (slot name)
   (slot age)
   (slot gender)
)

(deffacts people
   (person (name John) (age 25) (gender male))
   (person (name Mary) (age 22) (gender female))
   (person (name Bob) (age 30) (gender male))
)
