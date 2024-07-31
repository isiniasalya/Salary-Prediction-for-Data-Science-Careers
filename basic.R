data <- read.csv("D:/UOC/Z. Academic/3rd year/semester 2/ST 3082/a. Projects/project 3/datasets/salaries.csv")
sum(is.na(data))
sum(duplicated(data))


#VIF---------------------------------------------------------------------------------------
lm1<-lm(PremiumPrice~Age+Diabetes+BloodPressureProblems+AnyTransplants +AnyChronicDiseases+BMI+
          KnownAllergies+HistoryOfCancerInFamily+NumberOfMajorSurgeries,data=data)
car::vif(lm1)