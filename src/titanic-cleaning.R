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
if(!require(dummies)){
  install.packages("dummies")
  library(dummies)
}
if(!require(e1071)){
  install.packages("e1071")
  library(e1071)
}
if(!require(ROCR)){
  install.packages("ROCR")
  library(ROCR)
}
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
titanic_data$Fare[titanic_data$Fare > 400]
# 4 valors extrems però s'intueix que són a propòsit al ser els 4 exactament iguals.

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
ggplot(titanic_data1,aes(Fare)) + geom_density(size=1, alpha= 0.6)+ ylab("DENSITAT")

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
hist(x=titanic_data1$Age, main="Histograma Age", xlab="Age", ylab="Frecuencia", col = "green yellow", ylim=c(0,350), xlim = c(0,100))

#A continuació compararem els quartils de la distribució observada amb els quartils teòrics d'una distribució normal, com més s'aproximen a les dades d'una normal, més alineats estan els punts al voltant de la recta.
qqnorm(titanic_data1$Age) 
qqline(titanic_data1$Age, col="red")
ggplot(titanic_data1,aes(Age)) + geom_density(size=1, alpha= 0.6)+ ylab("DENSITAT")

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

#En ambdos test, podem veure com en les dues variablescas de Fare, es rebutja la hipòtesis nula i per tant, la variança no és constant, en canvi, pel que fa l'edat, amb un nivell de confiança del 95%, podem concloure que en ambdos grups la variança no varia, és a dir, no s'ha trobat diferencia significativa entre la variança d'aquests. 



## 4.3 Aplicació proves estadístiques: 

###Ens interessa descriure la relació entre la supervivència i les variables edat, classe i gènere. 
###Per a això, en primer lloc hem dut a terme un gràfic mitjançant diagrames de barres amb la quantitat de morts i supervivents segons la classe a la que viatjaven, l'edat o el sexe.
###D'altra banda, per a obtenir les dades que estem veient utilitzarem la comanda table per a dues variables que ens proporciona una taula de contingència.

plotbyClass<-ggplot(titanic_data1,aes(Pclass,fill=Survived))+geom_bar() +labs(x="Class", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("darksalmon","darkseagreen4"))+ggtitle("Survived by Class")
plotbyAge<-ggplot(titanic_data1,aes(Age,fill=Survived))+geom_bar() +labs(x="Age", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("darksalmon","darkseagreen4"))+ggtitle("Survived by Age")
plotbySex<-ggplot(titanic_data1,aes(Sex,fill=Survived))+geom_bar() +labs(x="Sex", y="Passengers")+ guides(fill=guide_legend(title=""))+ scale_fill_manual(values=c("darksalmon","darkseagreen4"))+ggtitle("Survived by Sex")
grid.arrange(plotbyClass,plotbyAge,plotbySex,ncol=2)



##CRITERIS D'ÈXIT:
## Van sobreviure més del 50% dels passatgers? Existeix diferència significativa per un nivell de significació del 5%?
#Per poder contrastar la hipòtesis, utilitzarem el test binominal exacte. 
#H0: la proporció és major del 50%. 
#H1: la proporció no és major. 
tableSurvived<-table(titanic_data1$Survived)
prop.table(table(titanic_data1$Survived))
binom.test(x = c(494, 815), alternative = "greater", conf.level = 0.95)
#Amb un nivell de confiança del 95% podem concloure que no va sobreviure més del 50% dels passatgers. 

##Hi ha diferència entre la proporció d'homes i dones que van sobreviure?
table_Sex<-table(titanic_data1$Sex, titanic_data1$Survived)
prop.table(table(titanic_data1$Sex, titanic_data1$Survived), margin=1)
##Per esbrinar si hi ha diferència, hem executat el test de fisher, el qual ens permet estudiar si existeix asociació entre dues variables qualitatives.
#H0: Les variables són independents, és a dir, el fet de sobreviure no varia pel fet de ser un home o una dona, H1: Lles variables són dependents, el gènere si té relació amb el fet de sobreviure o no. 
fisher.test(table_Sex, alternative = "two.sided")
#Si fem el test X^2 també és significatiu.
chisq.test(x = table_Sex)
chisq.test(x = table_Sex)$residuals
#Amb un 95% de confiança podem rebutjar el test, i per tant, afirmar que les dues variables estàn relacionades. 
#S'esperava 11.7% més d'homes que sobrevisques i un 15.8% de dones menys.

## Hi ha diferències en la supervivencia segons la classe en la que viatjaven?
table_Class<-table(titanic_data1$Pclass, titanic_data1$Survived)
prop.table(table(titanic_data1$Pclass, titanic_data1$Survived), margin=1)
#H0: Les variables són independents, és a dir, el fet de sobreviure no varia segons la classe, H1: Lles variables són dependents, la classe si té relació amb el fet de sobreviure o no. 
fisher.test(table_Class, alternative = "two.sided")
#Si fem el test X^2 també és significatiu.
chisq.test(x = table_Class)
chisq.test(x = table_Class)$residuals
#Podem afirmar novament amb un 95% de confiança que hi ha relació entre ambdues variables.
#S'esperava un 4.7% més de supervivents de la classe 3, en canvi de la classe 1 s'esperava un 5.8% menys. 


# A continuació discretitzarem la variable edat. El nombre d'intervals escollits=3, utilitzarem el mètode d'igual freqüència per tal de mantenir la mateixa freqüència. 
dis1<-table(discretize(x=titanic_data1$Age, method = "frequency", breaks =4, include.lowest = TRUE))
dis1
titanic_data1$AgeD[titanic_data1$Age <21] <- "Menors de 21 anys"
titanic_data1$AgeD[titanic_data1$Age >= 21 & titanic_data1$Age < 28] <- "Entre 21 i 28 anys"
titanic_data1$AgeD[titanic_data1$Age >= 28 & titanic_data1$Age < 39] <- "Entre 28 i 39 anys"
titanic_data1$AgeD[titanic_data1$Age>= 39] <- "Majors de 39"

# Tot seguit fem de la nova variable un factor
titanic_data1$AgeD <-
  factor(
    titanic_data1$AgeD,
    ordered = FALSE,
    levels = c(
      "Menors de 21 anys",
      "Entre 21 i 28 anys",
      "Entre 28 i 39 anys",
      "Majors de 39"
    )
  )

summary(titanic_data1$AgeD)

## Hi ha diferències en la supervivència segons l'edat?
##cal fer discretització d'edat (en rangs)
table_AgeD<-table(titanic_data1$AgeD, titanic_data1$Survived)
prop.table(table(titanic_data1$AgeD, titanic_data1$Survived), margin=1)

#H0: Les variables són independents, és a dir, el fet de sobreviure no varia segons l'edat, H1: Lles variables són dependents, l'edat si té relació amb el fet de sobreviure o no. 
fisher.test(table_AgeD, alternative = "two.sided")
#Si fem el test X^2 també és significatiu.
chisq.test(x = table_AgeD)
chisq.test(x = table_AgeD)$residuals
chisq.test(x = table_AgeD)$stdres
#Podem afirmar novament amb un 95% de confiança que hi ha relació entre ambdues variables.
#S'esperava un 2,3% més de supervivents en la franja d'edat 21-28 anys, en canvi, s'esperava un 2% menys en la de menors de 21.



##A més a més també ens interesava conèixer si la probabilitat de sobreviure tenia alguna relació amb el tamany de la família?
#Creació nova variable: 
titanic_data1$FamilySize <- titanic_data1$SibSp + titanic_data1$Parch +1;
hist(titanic_data1$FamilySize)
boxplot.stats(titanic_data1$FamilySize)$out
fligner.test(Age ~ Survived, data=titanic_data1)
lillie.test(x=titanic_data1$FamilySize)

summary(titanic_data1$FamilySize)
#Farem serà discretitzar també la variable Família Size. 
titanic_data1$FamilySizeD[titanic_data1$FamilySize <2] <- "Adult sol"
titanic_data1$FamilySizeD[titanic_data1$FamilySize >= 2 & titanic_data1$FamilySize < 5] <- "Famílies de dos a 4 membres"
titanic_data1$FamilySizeD[titanic_data1$FamilySize>=5] <- "Famílies amb més de 4 membres"

# Tot seguit fem de la nova variable un factor
titanic_data1$FamilySizeD<-
  factor(
    titanic_data1$FamilySizeD,
    ordered = FALSE,
    levels = c(
      "Adult sol",
      "Famílies de dos a 4 membres",
      "Famílies amb més de 4 membres"
    )
  )

summary(titanic_data1$FamilySizeD)

table_Family<-table(titanic_data1$FamilySizeD, titanic_data1$Survived)
prop.table(table(titanic_data1$FamilySizeD, titanic_data1$Survived), margin=1)
#Per veure si la diferència es significativa ho comprobarem estadísticament.
#H0: Les variables són independents, és a dir, el fet de sobreviure no varia segons el tamany de la unitat familiar, H1: Lles variables són dependents.
fisher.test(table_Family, alternative = "two.sided")
#Si fem el test X^2 també és significatiu.
chisq.test(x = table_Family)
chisq.test(x = table_Family)$residuals
chisq.test(x = table_Family)$stdres
#Podem veure amb un nivell de significació del 5%, com el tamany de la unita familiar tmabé va influir, van sobreviure un 9.7% més de famílies de 2-4 membres del que s'esperava, per contra, s'esperava que un 7.8% més d'adults sols sobrevisques.


##Mitjançant els gràfics de barres, les taules de contingencia i els tests realitzats podem concloure: 
##La proporció d'homes i dones que van sobreviure és força diferent, homes: 109, dones: 385, si ens fixem en el % respecte el seu gènere, en les dones és del 83% mentre que pels homes és del 23%.
##Referent a la classe en la que viatjaven, si ens fixem en el gràfic, el nombre de personas que més van sobreviure són els que viatjaven en 3 classe, cal dir, però que, el nombre de passatgers d'aquesta classe és molt major. Si ens fixem en el % dins de cada classe, són els de primera classe els que tenen una ràtio més alta de supervivència.
##Cal destacar també que la proporció d'adults sols és més del 50%, i que la franja on hi trobem més viatjants és la franja d'edat entre 21 i 28 anys.
##Després de realitzar els 4 test, podem veure que les diferències són significatives, i que tan l'edat, la classe en la que viatjaven, el gènere com el tamany de la unitat familiar van ser significatius per la superviència.
par(mfrow=c(2,2))
plot(table_Class, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Class")
plot(table_Sex, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Sex")
plot(table_AgeD, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Age")
plot(table_Family, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Family Size")



# Correlació:
aux_data <- titanic_data[, c("Age", "SibSp", "Parch", "Fare", "Survived")]
aux_data$Fare <- round(aux_data$Fare)
corr_matrix <- matrix(nc = 2, nr = 0)
colnames(corr_matrix) <- c("estimate", "p-value")
for (i in 1:(ncol(aux_data) - 1)) {
  spearman_test = cor.test(aux_data[,i],
                           aux_data[,length(aux_data)],
                           method = "spearman")
  corr_coef = spearman_test$estimate
  p_val = spearman_test$p.value
  
  pair = matrix(ncol = 2, nrow = 1)
  pair[1][1] = corr_coef
  pair[2][1] = p_val
  corr_matrix <- rbind(corr_matrix, pair)
  rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(aux_data)[i]
}

print(corr_matrix)


# Regressió logística
## Check number of uniques values for each of the column to find out columns which we can convert to factors
sapply(titanic_data, function(x) length(unique(x)))

## Removing Cabin as it has very high missing values, passengerId, Ticket and Name are not required
titanic_data2 <- titanic_data %>% select(-c(Cabin, PassengerId, Ticket, Name))

## Converting "Survived","Pclass","Sex","Embarked" to factors
for (i in c("Survived","Pclass","Sex","Embarked")){
  titanic_data2[,i]=as.factor(titanic_data2[,i])
}

## Create dummy variables for categorical variables
titanic_data2 <- dummy.data.frame(titanic_data2, names=c("Pclass","Sex","Embarked"), sep="_")

## Splitting training and test data
train <- titanic_data2[1:667,]
test <- titanic_data2[668:889,]

## Model Creation
model <- glm(Survived ~.,family=binomial(link='logit'),data=train)

## Model Summary
summary(model)

## Using anova() to analyze the table of devaiance
anova(model, test="Chisq")

## Predicting Test Data
result <- predict(model,newdata=test,type='response')
result <- ifelse(result > 0.5,1,0)

## Confusion matrix and statistics
confusionMatrix(table(result, test$Survived))

## ROC Curve and calculating the area under the curve(AUC)
predictions <- predict(model, newdata=test, type="response")
ROCRpred <- prediction(predictions, test$Survived)
ROCRperf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")

plot(ROCRperf, colorize = TRUE, text.adj = c(-0.2,1.5), print.cutoffs.at = seq(0,1,0.1))

auc <- performance(ROCRpred, measure = "auc")
auc <- auc@y.values[[1]]
auc
## ---- echo=TRUE---------------------------------------------------------------------------------


##5----REPRESENTACIÓ DELS RESULTATS--------------------------------------------------------------
par(mfrow=c(2,2))
plot(table_Class, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Class")
plot(table_Sex, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Sex")
plot(table_AgeD, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Age")
plot(table_Family, col = c("darksalmon","darkseagreen4"), main = "Survived vs. Family Size")
#+ gràfic correlació + regressió 

## ---- echo=TRUE-------------------------------------------------------------------------------



##6----CONCLUSIÓ-------------------------------------------------------------------------------


##Mitjançant els gràfics de barres, les taules de contingencia i els tests realitzats podem concloure: 
#Que els passatgers que van sobreviure és menys del 50%, concretament només representen un 37% del total. 
##La proporció d'homes i dones que van sobreviure és força diferent, homes: 109, dones: 385, si ens fixem en el % respecte el seu gènere, en les dones és del 83% mentre que pels homes és del 23%.
##Referent a la classe en la que viatjaven, el nombre de personas que més van sobreviure són els que viatjaven en 3 classe, cal dir, però que, el nombre de passatgers d'aquesta classe és molt major. Si ens fixem en el % dins de cada classe, són els de primera classe els que tenen una ràtio més alta de supervivència.
##Cal destacar també que la proporció d'adults sols és més del 50%, i que la franja on hi trobem més viatjants és la franja d'edat entre 21 i 28 anys.
##Després de realitzar els 4 test d'independència, podem veure que les diferències són significatives, i que tan l'edat, la classe en la que viatjaven, el gènere com el tamany de la unitat familiar van ser rellevants per la superviència.
#Van sobreviure més dones que homes, la classe amb majors supervivents és la Classe 1, pel que fa l'edat s'esperava més supervivents entre la franja dels 21-28 d'anys de la que es va produir, i finalment, pel que fa el tamany familiar també va tenir un paper important, on destaquen les famílies de 2-4 membres destacan com a supervivents, m'entre que pel que fa els adults que viatjeven sols s'esperava una ràtio de supervients major.


#FALTA CONCLUSIÓ CORRELACIÓ + REGRESSIÓ



## ---- echo=TRUE-------------------------------------------------------------------------------
#bibliografia
#https://rpubs.com/Joaquin_AR/218465
#http://www.cookbook-r.com/Statistical_analysis/Homogeneity_of_variance/
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/aggregate
#https://rpubs.com/ovolchenko/chisq2
#https://www.rdocumentation.org/packages/stats/versions/3.6.2/topics/fisher.test
#https://rpubs.com/Joaquin_AR/220579
