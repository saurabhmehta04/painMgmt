setwd("~/Desktop/Development/Spring 2015/painMgmt")

#reading Patients
patients <- read.csv("Patients.csv",TRUE, ",")
#class(patients)
#head(patients)

pains <- read.csv("Pains.csv",TRUE, ",")

medications <- read.csv("Medications.csv",TRUE, ",")

attach(patients)
attach(pains)
attach(medications)
nrow(patients)
nrow(pains)
nrow(medications)


##################### Test Case  ############################
# id = 151221
# patient <-subset(pains, pains$Log.id == id)
# patient
# avg
# avg <- mean(patient$Pain.Score, na.rm=TRUE)
# patients$avgPain[patients$Log.id == id] <- avg
# View(patients)


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


# Stop the clock
#proc.time() - ptm

##################### Correlation   ############################
attach(patients)
patients["Patient.name"] <- NULL
patients["X"] <- NULL
patients[patients=="N/A"] <- NA
patients[patients==""] <- NA
patients <- na.omit(patients)

par(mfrow=c(2,2))
#pairs(patients)


#Fitting the model
#lm.fit1 = lm(patients$avgPain~Age+Wgt+BMI+LOS+Attnd.Class+Anesthesia+Hip.Knee+Procedure.Name+Surgeon, data=patients)
#summary(lm.fit1)

###### Significant model #######

#lm.fit2 = lm(patients$avgPain~Age+Surgeon, data=patients,na.action="na.exclude") #p-value = 0.000939
#summary(lm.fit2)
#pairs(patients)

################################### New Significant model #######################################
lm.fit = lm(patients$avgPain~Age+LOS+Anesthesia, data=patients)
summary(lm.fit)
#############################################################################################


#lm.fit2 = lm(patients$avgPain~Age+Surgeon, data=patients,na.action="na.exclude") #p-value = 0.002
#summary(lm.fit2)

#lm.fit2 = lm(patients$avgPain~LOS+Surgeon, data=patients,na.action="na.exclude") #p-value = 0.002
#summary(lm.fit2)


############## Writing the patients to xlsx file ###################
#install.packages("xlsx")
#library(xlsx)
#write.xlsx(patients, file = "patientsWithAvgPain.xlsx")

####### Best subset #############

#patients = newPatientsDS;

#patients10 <- data.frame(patients)
#patients <- head(patients10,10) ## default is 6

# library(leaps)
# set.seed(1)
# library(MASS)
# k=10
# p = 10
# folds = sample(rep(1:k, length = nrow(patients)))
# cv.error = matrix(NA, k, p)
# 
# for(i in 1:k){
#   best.subset.fit = regsubsets(avgPain~Age+Gender+Wgt+BMI+LOS+Attnd.Class+Anesthesia+Hip.Knee+Procedure.Name+Surgeon, patients[folds != i,])
#   for(j in 1:p){
#     mat = model.matrix(avgPain~Age+Gender+Wgt+BMI+LOS+Attnd.Class+Anesthesia+Hip.Knee+Procedure.Name+Surgeon , patients[folds == i,])
#     coefi = coef(best.subset.fit, id = j)
#     pred = mat[, names(coefi)] %*% coefi
#     cv.error[i,j] = mean((avgPain[folds == i] - pred)^2)
#   }
# }
# 
# best.subset.fit = regsubsets(avgPain~Age+Gender+Wgt+BMI+LOS+Attnd.Class+Anesthesia+Hip.Knee+Procedure.Name+Surgeon, data = patients, method='forward')
# 
# best.subset.fit

# TODO:
# summary.out <- summary(best.subset.fit)
# as.data.frame(summary.out$outmat)
# square of varaibles, 
# non-linear regression in R


#lm.fit = lm(patients$avgPain~Age+LOS+Anesthesia, data=patients)
#summary(lm.fit)

#lm.fit10 <- lm(formula = avgPain ~ Age + I(Age^2) + I(Age^3))
#summary(lm.fit10)

#lm.fit11 <- lm(formula = avgPain ~ Age + LOS + Anesthesia+ I(Age^2) + I(Age^3))
#summary(lm.fit11)


# TODO: 
# Compute the p-value using python
# predict function because we dont know how well the model fits the data.
# error rate or Residue sum of error. 
# plotting the graph of medication and pain 


#Dividing dataset with only 10 rows for testing
patients_backup <- patients
dim(patients_backup)
patients <- head(patients,10) ## default is 6
View(patients)
# patients <- patients_backup 

#Cross-validation
library(DAAG)
lm.fit_cv = lm(avgPain~Age+Anesthesia, data=patients)
#lm.fit_cv$xlevels[["Surgeon"]] <- union(lm.fit_cv$xlevels[["Surgeon"]], levels(patients[test,]$Surgeon))
cv.lm(df=patients, lm.fit_cv, m=10)


#Validation set apporach
set.seed(5)
train = sample(1:nrow(patients), size=0.7*nrow(patients))
test = (-train)
data_train = patients[train,]
dim(data_train)
data_test = (patients[test,])
dim(data_test)
lm.fit = lm(avgPain~Age+LOS+Anesthesia, data=patients[train,])
lm.pred = predict(lm.fit,newdata = patients[test,])
mean((lm.pred - patients$avgPain[test])^2)

