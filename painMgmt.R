setwd("~/Desktop/Development/Spring 2015/painMgmt")

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

attach(patients)
attach(pains)
attach(medications)
nrow(patients)
nrow(pains)
nrow(medications)


##################### Test Case  ############################
id = 151221
patient <-subset(pains, pains$Log.id == id)
patient
avg <- mean(patient$Pain.Score, na.rm=TRUE)
avg
patients$avgPain[patients$Log.id == id] <- avg
View(patients)


##################### Subset of the patient to calcluate pain ##################
patientAvgPain <- function(id) {
  patient <-subset(pains, pains$Log.id == id)
  avgPain <- mean(patient$Pain.Score, na.rm=TRUE)
  return(avgPain)
}


##################### AVERAGE pain of patients ############################
# Start the clock!
#ptm <- proc.time()

for( i in 1:nrow(patients)) {
  id <- patients[i, "Log.id"]
  avg <- patientAvgPain(id) #getting the subset using the function
  patients$avgPain[patients$Log.id == id] <- avg
}

View(patients)

# Stop the clock
#proc.time() - ptm

##################### Correlation   ############################
attach(patients)
View(patients)
patients["Patient.name"] <- NULL
patients["X"] <- NULL

par(mfrow=c(2,2))
plot(Surgeon, avgPain, xlab = "Surgeon", ylab = "avgPain")
plot(LOS, avgPain, xlab = "LOS", ylab = "avgPain")
plot(BMI, avgPain, xlab = "BMI", ylab = "avgPain")
plot(Wgt, avgPain, xlab = "Wgt", ylab = "avgPain")
plot(Gender, avgPain, xlab = "Gender", ylab = "avgPain")

par(mfrow=c(2,2))
pairs(patients)

# Pains plot
View(pains)
View(medications)
View(patients)

lm.fit = lm(patients$avgPain~Age+Gender+Wgt+BMI+LOS+Attnd.Class+Anesthesia+Hip.Knee+Procedure.Name+Surgeon, data=patients)
summary(lm.fit)


lm.fit1 = lm(patients$avgPain~Age+Wgt+BMI+LOS+Attnd.Class+Anesthesia+Hip.Knee+Procedure.Name+Surgeon, data=patients)
summary(lm.fit1)

################################### Significant model #######################################
lm.fit2 = lm(patients$avgPain~Age+LOS, data=patients,na.action="na.exclude") #p-value = 0.000939
summary(lm.fit2)
#############################################################################################

lm.fit2 = lm(patients$avgPain~Age+Surgeon, data=patients,na.action="na.exclude") #p-value = 0.002
summary(lm.fit2)

lm.fit2 = lm(patients$avgPain~LOS+Surgeon, data=patients,na.action="na.exclude") #p-value = 0.002
summary(lm.fit2)


########################  Performing cross validation ##############################
#install.packages('DAAG')
library(DAAG)

na.omit(patients)
View(patients)
cv.lm(df=patients, lm.fit1, m=7)


############## Writing the patients to xlsx file ###################
install.packages("xlsx")
library(xlsx)
write.xlsx(patients, file = "patientsWithAvgPain.xlsx")

