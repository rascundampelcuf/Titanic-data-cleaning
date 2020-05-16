---
title: 'Respostes: PRAC2 - Tipologia i Cicle de Vida de les Dades'
author: "Autor: GABRIEL IZQUIERDO I MIREIA OLIVELLA"
date: "Maig 2020"
---
## ----load_libraries, include=FALSE---------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
install.packages("modeest") 
library(modeest)
if(!require(ggplot2)){
  install.packages('ggplot2', repos='http://cran.us.r-project.org')
  library(ggplot2)
}
if(!require(grid)){
  install.packages('grid', repos='http://cran.us.r-project.org')
  library(grid)
}
if(!require(gridExtra)){
  install.packages('gridExtra', repos='http://cran.us.r-project.org')
  library(gridExtra)
}

##----1. DESCRIPCIÓ del DATASET-------------------------------------------

## ---- echo=TRUE----------------------------------------------------------
# read data
titanic_train <- read.csv("../data/train.csv")
titanic_test <- read.csv("../data/test.csv")
#El dataset escollit recollir informació dels passatgers del titanic, en els que es pot analitzar la superviència i les característiques d'aquests. 
#Les dades del titanic contenen una barreja de variables textuals, booleanes, continues i categòriques. El dataset compte amb valors perduts, valors extrems i altres carectreístiques interessants que caldrà tractar. 

##----2. INTEGRACIÓ -----------------------------------------------------
##La base de dades està dividia en tres parts, la part de test té 418 registres i 11 variables, mentre que la de train té 891 observacions i 12 variables, la variable que no té el dataset test, és la variable Survived, que tenim en el fitxre anomenat gender_submission.
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

##----3. NETEJA DE DADES------------------------------------------------
# 3.1 Atributs amb valors buits: 
colSums(is.na(titanic_data))
colSums(titanic_data== "")

#Tractament valors buits variable "Embarked": 
#Ens basarem en usar una mesura de tendència central,en aquest cas al ser una variable categòrica usarem la moda 
mlv(titanic_data$Embarked, method = "mfv") 
##El ser S la moda: prenem el valor "S" per els valors buits de la variable.
titanic_data$Embarked[titanic_data$Embarked==""]="C"

#Tractament del valor Fare, mitjançant la mediana:
titanic_data[!complete.cases(titanic_data$Fare),]
titanic_data$Fare[1044] <- median(titanic_data$Fare, na.rm = TRUE)
#Variable Cabin: la suprimirem**


#Opció1:Edat
# Prenem la mitjana per a valors buits de la variable "Age" 
#titanic_data$Age[is.na(titanic_data$Age)] <- mean(titanic_data$Age,na.rm=T)

#3.2Valors Extrems


##4----AN�LISIS RELACIONS VARIABLES--------------------------------------
## �Quants passatgers van sobreviure?
table(titanic_data$Survived)
prop.table(table(titanic_data$Survived))
## Una mica m�s d'un ter� dels passatgers van sobreviure.

## �Hi ha difer�ncia entre la proporci� d'home si dones que van sobreviure?
table(titanic_data$Sex, titanic_data$Survived)
prop.table(table(titanic_data$Sex, titanic_data$Survived), margin=1)
##La majoria de les dones van sobreviure, per contra els homes no. 

# Visualitzem la relaci� entre les variables "sex" i "survival":
ggplot(data=titanic_data,aes(x=Sex,fill=Survived), colour="red")+geom_bar()

# Un altre punt de vista. Survival com a funci� de Embarked:
ggplot(data = titanic_data,aes(x=Embarked,fill=Survived))+geom_bar(position="fill")+ylab("Frequència")

## ---- echo=TRUE----------------------------------------------------------

female_people = titanic_data[titanic_data$Sex=="male",]
male_people = titanic_data[titanic_data$Sex=="female",]

# Visualitzem la relaci� entre les variables "Age" i "Pclass":
par(mfrow=c(1,2))
boxplot(female_people$Age~female_people$Pclass, main="Pclass by age (female)", xlab="Pclass", ylab="Age")
boxplot(male_people$Age~male_people$Pclass, main="Pclass by age (male)", xlab="Pclass", ylab="Age")

# Age missing values
age_mean <- function(age) {
  round(summary(age)['Mean'])
}

female_mean_ages = tapply(female_people$Age, female_people$Pclass, age_mean)
male_mean_ages = tapply(male_people$Age, male_people$Pclass, age_mean)

AgeImpute <- function(row) {
  sex <- row['Sex']
  age <- row['Age']
  pclass <- row['Pclass']
  value <- age
  if (is.na(age)) {
    if (sex == "female") {
      value <- female_mean_ages[pclass]
    } else {
      value <- male_mean_ages[pclass]
    }
  }
  return(value)
}
  
titanic_data$Age <- apply(titanic_data[, c("Sex", "Age", "Pclass")], 1, AgeImpute)