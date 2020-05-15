## ----load_libraries, include=FALSE---------------------------------------
#library(knitr)
library(ggplot2)
library(dplyr)
install.packages("modeest") 
library(modeest)


#DESCRIPCIÓ del DATASET:
# read data 
titanic_train <- read.csv("../data/train.csv")
titanic_test <- read.csv("../data/test.csv")

#INTEGRACIÓ dades en un únic dataset
titanic_gender_submission <- read.csv("../data/gender_submission.csv")
titanic_test <- merge(titanic_test, titanic_gender_submission, by="PassengerId")
titanic_data <- rbind(titanic_train, titanic_test)

##Estructura dades:
str(titanic_data)
dim(titanic_data)
summary(titanic_data)

##Atributs amb valors buits: 
colSums(is.na(titanic_data))
colSums(titanic_data== "")

##Tractament valors buits variable "Embarked": 
##Ens basarem en usar una mesura de tendència central,en aquest cas al ser una variable categòrica usarem la moda 
mlv(titanic_data$Embarked, method = "mfv") 
#El ser S la moda: prenem el valor "S" per els valors buits de la variable.
titanic_data$Embarked[titanic_data$Embarked==""]="C"

#Comprovació 
colSums(titanic_data== "")


#Opció1:Edat
# Prenem la mitjana per a valors buits de la variable "Age" 
#titanic_data$Age[is.na(titanic_data$Age)] <- mean(titanic_data$Age,na.rm=T)


