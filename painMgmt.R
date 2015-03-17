install.packages("gdata") #reading the .xlsx format
install.packages("RODBC") #reading the entire excel workbook
install.packages("devtools")
library(gdata)
library(RODBC)
getwd()
setwd("~/Desktop/Development/Spring 2015/")
data <- read.xls("data.xlsx")

#converting the xlsx file to CSV as there are more functionality in CSV

#reading Patients
patients <- read.csv("Patients.csv",TRUE, ",")
class(patients)
head(patients)

#reading pains  
pains <- read.csv("Pain.csv",TRUE, ",")
class(pains)
head(pains)

#reading medications
medications <- read.csv("Medications.csv",TRUE, ",")
class(patients)
head(patients)

#painMedication.df = data.frame(pains, medications) #Doesn't work

painMedication = merge(pains, medications)
class(painMedication)
head(painMedication)
view(painMedication)


