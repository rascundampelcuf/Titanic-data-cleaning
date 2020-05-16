## ----load_libraries, include=FALSE---------------------------------------
#library(knitr)
library(ggplot2)
library(dplyr)
install.packages("modeest") 
library(modeest)


#1. DESCRIPCIÓ del DATASET:
## Read data 
titanic_train <- read.csv("../data/train.csv")
titanic_test <- read.csv("../data/test.csv")

#2. INTEGRACIÓ dades
##La base de dades està dividia en tres parts, la part de test té 418 registres i 11 variables, mentre que la de train té 891 observacions i 12 variables, la variable que no té el dataset test, és la variable Survived.
##A continuació hem integrat les tres parts, en un sol dataset. 
dim(titanic_test)
dim(titanic_train)
titanic_gender_submission <- read.csv("../data/gender_submission.csv")
titanic_test <- merge(titanic_test, titanic_gender_submission, by="PassengerId")
titanic_data <- rbind(titanic_train, titanic_test)

##Estructura dades:
str(titanic_data)
#El dataset final està format per 1309 observacions i 12 variables 
dim(titanic_data)
summary(titanic_data)

#3. NETEJA DE DADES:
##Atributs amb valors buits: 
colSums(is.na(titanic_data))
colSums(titanic_data== "")

##Tractament valors buits variable "Embarked": 
##Ens basarem en usar una mesura de tendència central,en aquest cas al ser una variable categòrica usarem la moda 
mlv(titanic_data$Embarked, method = "mfv") 
#El ser S la moda: prenem el valor "S" per els valors buits de la variable.
titanic_data$Embarked[titanic_data$Embarked==""]="C"

#Variable Cabin: la suprimirem**

#Comprovació 
colSums(titanic_data== "")


#Opció1:Edat
# Prenem la mitjana per a valors buits de la variable "Age" 
#titanic_data$Age[is.na(titanic_data$Age)] <- mean(titanic_data$Age,na.rm=T)


#ANÀLISIS RELACIONS VARIABLES

##¿Quants passatgers van sobreviure?
table(titanic_data$Survived)
prop.table(table(titanic_data$Survived))
## Una mica més d'un terç dels passatgers van sobreviure.

##¿Hi ha diferència entre la proporció d'home si dones que van sobreviure?
table(titanic_data$Sex, titanic_data$Survived)
prop.table(table(titanic_data$Sex, titanic_data$Survived), margin=1)
##La majoria de les dones van sobreviure, per contra els homes no. 

# Visualitzem la relació entre les variables "sex" i "survival":
ggplot(data=titanic_data[1:filas,],aes(x=Sex,fill=Survived))+geom_bar()

