## ----load_libraries, include=FALSE---------------------------------------
library(knitr)
library(ggplot2)
library(dplyr)
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
library(car)
library(normtest)
library(nortest)
## ---- echo=TRUE----------------------------------------------------------

##----1. DESCRIPCIÓ del DATASET-------------------------------------------
#El dataset escollit recull informació dels passatgers del titanic, en els que es pot analitzar la superviència i les característiques d'aquests. 
#Les dades del titanic contenen una barreja de variables textuals, booleanes, continues i categòriques. El dataset compte amb valors perduts, valors extrems i altres carectreístiques interessants que caldrà tractar. 

#read Data
titanic_train <- read.csv("../data/train.csv")
titanic_test <- read.csv("../data/test.csv")
## ---- echo=TRUE----------------------------------------------------------

##----2. INTEGRACIÓ -----------------------------------------------------
##La base de dades està dividia en tres parts, la part de test té 418 registres i 11 variables, mentre que la de train té 891 observacions i 12 variables, la variable que no té el dataset test, és la variable Survived, que tenim en el fitxer anomenat gender_submission.
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

## ---- echo=TRUE----------------------------------------------------------


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
titanic_data$Fare[1044] <- mean(titanic_data$Fare, na.rm = TRUE)

# Age missing values
age_mean <- function(age) {
  round(summary(age)['Mean'])
}
female_people = titanic_data[titanic_data$Sex=="male",]
male_people = titanic_data[titanic_data$Sex=="female",]

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
  return(as.numeric(value))
}

titanic_data$Age <- apply(titanic_data[, c("Sex", "Age", "Pclass")], 1, AgeImpute)

#3.2Valors Extrems
##Els valors extrems o outliers són registres que destacant per ser molt distants al valor central del conjunt. 
##Generalment es considera un outlier quan el seu valor es troba allunyat 3 desviacions estàndars respecte la mitjana, un instrument gràfic que ens permet visualitzar ràpidament aquests valors són els diagrames de caixes. 
##Una altre forma de detectar-los a R, es mitjançant la funció boxplot.stats()
fare.bp<-boxplot(titanic_data$Fare, main="Fare", col="darkgreen")
Age.bp<-boxplot(titanic_data$Age, main="Age", col="darkgreen")

boxplot.stats(titanic_data$Age)$out
boxplot.stats(titanic_data$Fare)$out

#Si ens fixem en els valors extrems resultants, en el cas d'Age, són valors que poden donar-se perfectament, ja que podem tenir persones de 80 anys.
#En el cas de Fare, són valors que es poden haver donat també, ja que el preu que hagi pugut pagar cada passatger pot tenir una gran oscil.lació, i es poden donar valors de 0 a 500 perfectament.
#Es per això, que tot i haver-los detectat, hem decidit no tractar-los de manera diferent a com han estat recollits.

## ---- echo=TRUE----------------------------------------------------------


##4----ANÀLISI DE LES DADES------------------------------------------------

#4.1 Selecció de dades: 
##Totes les variables que tenim en el dataset fan referència a característiques dels passatgers del titanic.
##Tot i això, podem precindir de la columna CABIN, ja que per l'anàlis que durem a terme, no serà necessari la precisió tència del número de cabina.
titanic_data1<- select(titanic_data, -Cabin)
summary(titanic_data1)

#4.2 Normalitat i homogeneïtat de la variància
#Per comporbar si segueix una distribució normal, podem tenir una aproximació amb la funció qqnorm, on veiem que hi ha força desviaciço en alguns trams, i per tant, possibles evidencies de que no segueix una distribució normal.
#VARIABLE FARE
summary(titanic_data1$Fare)
#Representació de la distribució de la variable Fare mitjançant un histograma: 
hist(x=titanic_data1$Fare, main="Histograma Fare", xlab="Fare", ylab="Frecuencia", col = "purple", ylim=c(0,1200), xlim = c(0,600))

#A continuació compararem els quartils de la distribució observada amb els quartils teòrics d'una distribució normal, com més s'aproximen a les dades d'una normal, més alineats estan els punts al voltant de la recta.
qqnorm(titanic_data1$Fare) 
qqline(titanic_data1$Fare, col="red")
ggplot(titanic_data1,aes(Fare)) + geom_density(size=1, alpha= 0.6)+ ylab("DENSIDAD")

#Mètode analític per contrastar la Normalitat
##Hipòtesis nul.la: les dades procedeixen d'una distribució normal. 
##Hipòtesis alterantiva: no procedeixen d'una distribució normal. 

#TEST DE SHAPIRO-WILK 
shapiro.test(x=titanic_data1$Fare)
##rebutjem hipotesis nul.la, la diferència és estadísticament significativa. (mostres menor de 50)

#TEST Lilliefors
#Asumeix que la media y varianza poblacional són desconegudes. 
library("nortest")
lillie.test(x=titanic_data1$Fare)
##rebutjem hipotesis nul.la, la diferència és estadísticament significativa.
##Problemes de la manca de normalitat; estimadors mínim-quadrats no són eficients y els intervals de confiança són aproximats no exactes. 


#VARIABLE AGE
summary(titanic_data1$Age)
#Representació de la distribució de la variable Fare mitjançant un histograma: 
hist(x=titanic_data1$Age, main="Histograma Age", xlab="Age", ylab="Frecuencia", col = "green yellow", ylim=c(0,200), xlim = c(0,100))

#A continuació compararem els quartils de la distribució observada amb els quartils teòrics d'una distribució normal, com més s'aproximen a les dades d'una normal, més alineats estan els punts al voltant de la recta.
qqnorm(titanic_data1$Age) 
qqline(titanic_data1$Age, col="red")
ggplot(titanic_data1,aes(Age)) + geom_density(size=1, alpha= 0.6)+ ylab("DENSIDAD")

#Mètode analític per contrastar la Normalitat
##Hipòtesis nul.la: les dades procedeixen d'una distribució normal. 
##Hipòtesis alterantiva: no procedeixen d'una distribució normal. 

#TEST Lilliefors
#Asumeix que la media y varianza poblacional són desconegudes. 
library("nortest")
lillie.test(x=titanic_data1$Age)
##rebutjem hipotesis nul.la, la diferència és estadísticament significativa.
##Problemes de la manca de normalitat; estimadors mínim-quadrats no són eficients y els intervals de confiança són aproximats no exactes.

pairs(titanic_data1[, c(6,10)])
View(titanic_data1)


#Comprovació de la HOMOGENEITAT DE LA VARIANCIA
##Finalment comprovarem l'homoscedasticitat de les dades, és a dir, la igualtat de variàncies per Fare i Age.
aggregate(Fare~Survived, data = titanic_data1, FUN = var)
aggregate(Age~Survived, data = titanic_data1, FUN = var)
##Ja que no tenim seguretat que provinguin d'una població normal, hem utilitzat el test de Levene amb la mediana com a mesura de centralitat, juntament amb el test no paramètric  Fligner-Killeen que també es basa en la mediana. 
##Levene TEST
levene <- filter(.data = titanic_data1, Survived %in% c("0", "1"))
leveneTest(y = levene$Fare, group = levene$Survived, center = "median")
leveneTest(y = levene$Age, group = levene$Survived, center = "median")
##Fligner-Killeen
fligner.test(Fare ~ Survived, data=titanic_data1)
fligner.test(Age ~ Survived, data=titanic_data1)

#En ambdos test, podem veure com en el cas de Fare, es rebutja la hipòtesis nula i per tant, la variança no és constant, en canvi, pel que fa l'edat, amb un nivell de confiança del 95%, podem concloure que en ambdos grups la variança no varia, és a dir, no s'ha trobat diferencia significativa entre la variança d'aquests. 


## 4.3 Aplicació proves estadístiques
#Quants passatgers van sobreviure?
table(titanic_data1$Survived)
prop.table(table(titanic_data1$Survived))
## Una mica m?s d'un ter? dels passatgers van sobreviure.

## Hi ha diferencia entre la proporci? d'home si dones que van sobreviure?
table(titanic_data1$Sex, titanic_data1$Survived)
prop.table(table(titanic_data1$Sex, titanic_data1$Survived), margin=1)
##La majoria de les dones van sobreviure, per contra els homes no. 

# Visualitzem la relació entre les variables "sex" i "survival":
ggplot(data=titanic_data1,aes(x=Sex,fill=Survived), colour="red")+geom_bar()

# Visualitzem la relació entre les variables "Age" i "Pclass":
#par(mfrow=c(1,2))
#boxplot(female_people$Age~female_people$Pclass, main="Pclass by age (female)", xlab="Pclass", ylab="Age")
#boxplot(male_people$Age~male_people$Pclass, main="Pclass by age (male)", xlab="Pclass", ylab="Age")

## ---- echo=TRUE----------------------------------------------------------


#bibliografia
#https://rpubs.com/Joaquin_AR/218465
#http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/


