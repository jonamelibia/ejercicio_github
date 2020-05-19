#MODELOS MATEMÁTICOS

#base ris
#regresión lineal múltiple con todas las variables
base_ris1<-base_ris[,-c(1,2,3,6)]
library(caret)
trainIndex <- createDataPartition(base_ris1$grupo_ris, p=0.80, list=FALSE)
dataTrain <- base_ris1[ trainIndex,]
dataTest <- base_ris1[-trainIndex,]
dataTrain
dataTest

z<-lm(dataTrain$ris~.,data=dataTrain[,-c(2)])
z 
cor(dataTrain$ris,fitted.values(z))
summary(z)

predictions <- predict(z, dataTest[,3:19])
predictions
library(Metrics)
metrica_mae_regresion<-mae(predictions,dataTest$ris)
metrica_mae_regresion

#modelo5<-update(modelo4,.~.-la variable)


#J48   grupo ris ~todas las variables obligatorias
set.seed(7)
base_ris1<-base_ris[,-c(1,2,3,6)]
library(caret)
trainIndex <- createDataPartition(base_ris1$grupo_ris, p=0.80, list=FALSE)
dataTrain <- base_ris1[ trainIndex,]
dataTest <- base_ris1[-trainIndex,]
dataTrain
dataTest

modeloj48_obl<- train(grupo_ris~., data=dataTrain[,-1], 
                      method="J48",
                      trControl=trainControl(
                        method="cv",number=10))
modeloj48_obl$finalModel
dim(dataTest)
dim(dataTest)
predictions <- predict(modeloj48_obl, dataTest[,3:19])
predictions

confusionMatrix(table(predictions,dataTest$grupo_ris))
metrica_accuracy_j48_obl<-0.4651

modeloj48_obl
library(partykit)
plot(modeloj48_obl$finalModel)


summary(modeloj48_obl)
summary(predictions)

#J48  grupo ris ~ ris
modelo<- train(grupo_ris~., data=dataTrain[,1:2], 
               method="J48",
               trControl=trainControl(
                 method="cv",number=10))
modelo$finalModel
dim(dataTest)
predictions<-predict(modelo,dataTest[,1:2])
predictions

confusionMatrix(table(predictions,dataTest$grupo_ris))
metrica_accuracy_j48_gruporis<-0.9302 

if(require("partykit", quietly = TRUE)) plot(modelo$finalModel)

summary(modelo)


#M5P ris~varibles obligatorias
set.seed(7)
base_ris1
library(caret)
trainIndex <- createDataPartition(base_ris1$ris, p=0.80, list=FALSE)
dataTrain <- base_ris1[ trainIndex,]
dataTest <- base_ris1[-trainIndex,]
dataTrain
dataTest

library(RWeka)
library(caret)
modelo1<-M5P(dataTrain$ris~., data=dataTrain[,-(2)])
modelo1

predictions<-predict(modelo1,dataTest[,-c(1,2)])
predictions

library(Metrics)
metrica_mae_M5P_obl<-mae(predictions,dataTest$ris)
metrica_mae_M5P_obl

if(require("partykit", quietly = TRUE)) plot(modelo1)

summary(modelo1)


#base_ampliada 

#quitar las variables obligatoras
set.seed(7)
base_ampliada1<-base_ampliada[,-c(1:5,7:23)]
base_ampliada1

library(caret)
trainIndex1 <- createDataPartition(base_ampliada1$grupo_ris, p=0.80, list=FALSE)
dataTrain1 <- base_ampliada1[ trainIndex1,]
dataTest1 <- base_ampliada1[-trainIndex1,]
dataTrain1
dataTest1

#modeloj48: variables base ampliada
modeloj48<- train(grupo_ris~., data=dataTrain1, 
                  method="J48",
                  trControl=trainControl(
                    method="cv",number=10))
modeloj48$finalModel
dim(dataTest1)
predictions <- predict(modeloj48, dataTest1[,2:81])
predictions
summary(modeloj48)
confusionMatrix(table(predictions, dataTest1$grupo_ris))
metrica_accuracy_j48_ampli<-0.4651

#M5P
#eliminar las variables no numericas
#ademas se eliminan las variables obligatorias
base_ampliada_solo_M5P <- base_ampliada[,-c(1:4,7:23)]
base_ampliada_solo_M5P <- base_ampliada_solo_M5P[,-2]

#modelo para predecir el ris en funcion de las variables ampliadas
set.seed(7)
library(caret)
trainIndex <- createDataPartition(base_ampliada_solo_M5P$ris, p=0.80, list=FALSE)
dataTrain <- base_ampliada_solo_M5P[ trainIndex,]
dataTest <- base_ampliada_solo_M5P[-trainIndex,]
dataTrain
dataTest
library(RWeka)
M5P_ampliadas_solo <- M5P(dataTrain$ris~., data = dataTrain)
print(M5P_ampliadas_solo)
plot(M5P_ampliadas_solo)

summary(M5P_ampliadas_solo)
dim(dataTest)
predictions <- predict(M5P_ampliadas_solo, dataTest[,2:81])
predictions

library(Metrics)
metrica_mae_M5P_ampl<-mae(predictions,dataTest[,1])
metrica_mae_M5P_ampl

set.seed(7)
base_ampliada1<-base_ampliada[,-c(1:5)]
base_ampliada1

library(caret)
trainIndex <- createDataPartition(base_ampliada1$grupo_ris, p=0.80, list=FALSE)
dataTrain <- base_ampliada1[ trainIndex,]
dataTest <- base_ampliada1[-trainIndex,]
dataTrain
dataTest

#modeloj48: variables base ampliada, explicativas + ampliadas
modeloj48<- train(grupo_ris~., data=dataTrain, 
                  method="J48",
                  trControl=trainControl(
                    method="cv",number=10))
modeloj48$finalModel
dim(dataTest)
predictions <- predict(modeloj48, dataTest[,2:98])
predictions
summary(modeloj48)
confusionMatrix(table(predictions, dataTest$grupo_ris))

metrica_accuracy_j48_todo<-0.4186


#modelos con variables elegidas segun la correlacion

#dataTrain y dataTest
set.seed(7)
library(caret)
trainIndex <- createDataPartition(tabla4$grupo_ris, p=0.80, list=FALSE)
dataTrain <- tabla4[ trainIndex,]
dataTest <- tabla4[-trainIndex,]
dataTrain
dataTest

#J48
nuevo1<- train(grupo_ris~., data=dataTrain[,-1], 
               method="J48",
               trControl=trainControl(
                 method="cv",number=10))
nuevo1$finalModel
dim(dataTest)
predictions <- predict(nuevo1, dataTest[,2:17])
predictions
summary(nuevo1)
confusionMatrix(table(predictions,dataTest$grupo_ris))

metrica_accuracy_j48_cor<-0.3721


#M5P               
set.seed(7)
library(caret)
trainIndex <- createDataPartition(tabla4$ris, p=0.80, list=FALSE)
dataTrain <- tabla4[ trainIndex,]
dataTest <- tabla4[-trainIndex,]
dataTrain
dataTest
library(RWeka)
nuevo2<-M5P(dataTrain$ris~.,data=dataTrain[,-2])
nuevo2
summary(nuevo2)

predictions<-predict(nuevo2,dataTest[,3:17])
predictions
library(Metrics)
metrica_mae_M5P_cor<-mae(predictions, dataTest$ris)
metrica_mae_M5P_cor





#tabla seleccionada con variables económicas
tabla5<-tabla3[,c(1,2,5,8,10,11,12,24,25,26,28,30,32,34,35,46,51)]
tabla5<-tabla5[,-15]

#J48 variables economicas


set.seed(7)
library(caret)
trainIndex <- createDataPartition(tabla5$grupo_ris, p=0.80, list=FALSE)
dataTrain <- tabla5[ trainIndex,]
dataTest <- tabla5[-trainIndex,]
dataTrain
dataTest

nuevo3<- train(grupo_ris~., data=dataTrain[,-1], 
               method="J48",
               trControl=trainControl(
                 method="cv",number=10))
nuevo3$finalModel
dim(dataTest)
predictions <- predict(nuevo3, dataTest[,2:16])
predictions
summary(nuevo3)
confusionMatrix(table(predictions,dataTest$grupo_ris))

metrica_accuracy_j48_econo<-0.4186


#M5P variables económicas             
set.seed(7)
library(caret)
trainIndex <- createDataPartition(tabla5$ris, p=0.80, list=FALSE)
dataTrain <- tabla5[ trainIndex,]
dataTest <- tabla5[-trainIndex,]
dataTrain
dataTest
library(RWeka)
nuevo4<-M5P(dataTrain$ris~.,data=dataTrain[,-2],control = Weka_control(M=15))
nuevo4
summary(nuevo4)

predictions<-predict(nuevo4,dataTest[,3:16])
predictions
library(Metrics)
metrica_mae_M5P_econo<-mae(predictions, dataTest$ris)
metrica_mae_M5P_econo


#regresión lineal múltiple con variables economicas

set.seed(7)
library(caret)
trainIndex <- createDataPartition(tabla5$ris, p=0.80, list=FALSE)
dataTrain <- tabla5[ trainIndex,]
dataTest <- tabla5[-trainIndex,]
dataTrain
dataTest
z<-lm(dataTrain$ris~.,data=dataTrain[,-c(2)])
z 
cor(dataTrain$ris,fitted.values(z))
summary(z)

predictions <- predict(z, dataTest[,3:16])
predictions
library(Metrics)
metrica_mae_regresion<-mae(predictions,dataTest$ris)
metrica_mae_regresion



#árbol J48 variables seleccionadas por sus correlaciones y por criterio económico
#es decir, las variables que se repiten en la tabla4 y en la tabla5

set.seed(7)
tabla7<-tabla5[,c(1,2,3,4,5,6,9,10,14)]
tabla7
library(caret)
trainIndex <- createDataPartition(tabla7$grupo_ris, p=0.80, list=FALSE)
dataTrain <- tabla7[ trainIndex,]
dataTest <- tabla7[-trainIndex,]
dataTrain
dataTest
nuevo5<- train(grupo_ris~., data=dataTrain[,-1], 
               method="J48",
               trControl=trainControl(
                 method="cv",number=10))
nuevo5$finalModel
dim(dataTest)
predictions1 <- predict(nuevo5, dataTest[,3:9])
predictions1
summary(nuevo5)
confusionMatrix(table(predictions1,dataTest$grupo_ris))

metrica_accuracy_cor_econ<-0.4651

