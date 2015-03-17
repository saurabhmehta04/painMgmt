install.packages("gdata") #reading the .xlsx format
install.packages("RODBC") #reading the entire excel workbook
install.packages("devtools")
library(gdata)
library(RODBC)
getwd()
setwd("~/Desktop/Development/Spring 2015/painMgmt")
data <- read.xls("data.xlsx")

#converting the xlsx file to CSV as there are more functionality in CSV

#reading Patients
patients <- read.csv("Patients.csv",TRUE, ",")
class(patients)
head(patients)

#reading pains  
pains <- read.csv("Pains.csv",TRUE, ",")
class(pains)
head(pains)

#reading medications
medications <- read.csv("Medications.csv",TRUE, ",")
class(patients)
head(patients)

#painMedication.df = data.frame(pains, medications) #Doesn't work

#using Merge function
painMedication = merge(pains, medications)
class(painMedication)
head(painMedication)

#Calculating average pain of particular patients
painAvg_150094 <- subset(pains, pains$Log.id == "150094")
dim(painAvg_150094)
head(painAvg_150094)
avg = mean(painAvg_150094$Pain.Score)
avg

#Making a function to automate the system
patientAvgPain <- function(id) {
  patient <-subset(pains, pains$Log.id == id)
  avg = mean(patient$Pain.Score)
  return(avg)
}

#Calculating average of 10 patients
avg = patientAvgPain("150094")
avg2 = patientAvgPain("150440")
avg3 = patientAvgPain("150934")
avg4 = patientAvgPain("151221")
avg5 = patientAvgPain("152066")
avg6 = patientAvgPain("152097")
avg7 = patientAvgPain("152680")
avg8 = patientAvgPain("153788")
avg9 = patientAvgPain("154545")
avg10 = patientAvgPain("154677")

#adding avg to the dataFrame
patients$avgPain[patients$Log.id == "150094"] <- avg
patients$avgPain[patients$Log.id == "150440"] <- avg2
patients$avgPain[patients$Log.id == "150934"] <- avg3
patients$avgPain[patients$Log.id == "151221"] <- avg4
patients$avgPain[patients$Log.id == "152066"] <- avg5
patients$avgPain[patients$Log.id == "152097"] <- avg6
patients$avgPain[patients$Log.id == "152680"] <- avg7
patients$avgPain[patients$Log.id == "153788"] <- avg8
patients$avgPain[patients$Log.id == "154545"] <- avg9
patients$avgPain[patients$Log.id == "154677"] <- avg10

#Setting Patients.name to null
patients$Patient.name <- NULL
patients
dim(patients)

#Retrieving patients with average Pain score 
head(patients)
painAvgPatients <- subset(patients, patients$avgPain != "NA")
painAvgPatients
attach(painAvgPatients)

#Histogram 
hist(avgPain)

#ScatterPlot 
par(mfrow=c(2,2))
plot(avgPain, Wgt)
plot(avgPain, Surgeon)
plot(avgPain, LOS)
plot(avgPain, BMI)
plot(avgPain, Age)

abline(lm(avgPain~Age, data=patients))



#Linear Model
summary(fit)
plot(fit)
