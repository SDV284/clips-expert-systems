;;; Завдання 4* - впорядковані факти; deftemplate з multislot, type, allowed-values
(deftemplate person
   (multislot name)
   (slot age (type INTEGER))
   (slot gender (allowed-values male female))
)

(deffacts people
   (person (name John Smith) (age 25) (gender male))
   (person (name Mary Ann Clark) (age 22) (gender female))
   (person (name Bob Brown) (age 30) (gender male))
)
