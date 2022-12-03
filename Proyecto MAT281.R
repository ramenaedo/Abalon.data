library(tidyverse)
library(tidytext)
library(tm)
library(caret)
library(car)
library(ggplot2)
library(class)
library(RColorBrewer)
library(caTools)
library(pROC)
library(rpart)
library(rpart.plot)

download.file(url = "https://raw.githubusercontent.com/ramenaedo/Abalon.data/main/abalone.data", destfile = "abalon.data")
abalon_data<-read_csv("abalon.data")
abalon_data$Sexo = as.factor(abalon_data$Sexo)
abalon_data$Sexo = as.numeric(abalon_data$Sexo)

#Veamos la cantidad de muestras respecto a la cantidad de anillos
table(abalon_data$Anillos)
abalon_data$Clasificacion<-recode(abalon_data$Anillos, "1:10=0 ; 11:29=1")
abalon_data$Clasificacion <- factor(abalon_data$Clasificacion, levels = c("0", "1"), labels = c("Jovenes", "Adulto"))
#Se ve mas bonito lo anterior 
#Realizamos el primer scatterplots
attach(abalon_data)
names(abalon_data)
grafica1=ggplot(abalon_data,aes(Longitud,Diametro,color=Anillos))
grafica1+geom_point() + scale_color_gradient(low = "blue", high="red")

#Realizamos 2do scatterplots
grafica2=ggplot(abalon_data,aes(Diametro,PesoEntero,color=Anillos))
grafica2+geom_point()+ scale_color_gradient(low = "blue", high="red")

#Realizamos 3er scatterplots
grafica3=ggplot(abalon_data,aes(Longitud,PesoConcha,color=Anillos))
grafica3+geom_point()+ scale_color_gradient(low = "blue", high="red")

#Realizamos 4to scatterplots
grafica4=ggplot(abalon_data,aes(PesoConcha,Diametro,color=Anillos))
grafica4+geom_point()+ scale_color_gradient(low = "blue", high="red")

#Realizamos 5to scatterlplot
grafica5=ggplot(abalon_data,aes(Sexo,Anillos,color=Anillos))
grafica5+geom_point()+ scale_color_gradient(low = "blue", high="red")

ggplot(data = abalon_data, aes(x=Clasificacion, y=PesoEntero, group=Clasificacion, color=Clasificacion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

ggplot(data = abalon_data, aes(x=Clasificacion, y=PesoDesbullado, group=Clasificacion, color=Clasificacion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

ggplot(data = abalon_data, aes(x=Clasificacion, y=PesoConcha, group=Clasificacion, color=Clasificacion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

ggplot(data = abalon_data, aes(x=Clasificacion, y=PesoVisceras, group=Clasificacion, color=Clasificacion)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1) +
  theme_bw() +
  theme(legend.position = "null")

#Regresion Logistica
attach(abalon_data)
modelo<-glm(Clasificacion ~Longitud+Diametro+PesoEntero+PesoDesbullado+PesoConcha+PesoVisceras, family = binomial(link = "logit"),data=abalon_data)
summary(modelo)

#Realizamos una validacion cruzada con nuestros datos
set.seed(12345)
split <- sample.split(abalon_data$Clasificacion, SplitRatio = 0.80)
training_set <- subset(abalon_data, split == TRUE)
test_set <- subset(abalon_data, split == FALSE)
table(training_set$Clasificacion)
table(test_set$Clasificacion)
#Son proporcionales
#Validacion Cruzada
folds <- createFolds(training_set$Clasificacion, k = 10)
# Regresion Logistica
cvRegresionLogistica <- lapply(folds, function(x){
  training_fold <- training_set[-x, ]
  test_fold <- test_set[x, ]
  clasificador <- glm(Clasificacion ~ Longitud+Diametro+PesoEntero+PesoDesbullado+PesoConcha+PesoVisceras, family = binomial(link = "logit"), data = training_fold)
  y_pred <- predict(clasificador, type = 'response', newdata = test_fold)
  y_pred <- ifelse(y_pred > 0.5, 1, 0)
  y_pred <- factor(y_pred, levels = c("0", "1"), labels = c("Jovenes", "Adulto"))
  cm <- table(test_fold$Clasificacion, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionRegresionLogistica <- mean(as.numeric(cvRegresionLogistica))
precisionRegresionLogistica

#CurvaROCValidacion
aucs = c()
for (kk in 1:100)
{
  train_index <- createDataPartition(abalon_data$Clasificacion, p = 0.8, list = FALSE, times = 1)
  train = abalon_data[train_index,]
  test = abalon_data[-train_index,]
  model<-glm(Clasificacion ~Longitud+Diametro+PesoEntero+PesoDesbullado+PesoConcha+PesoVisceras, family = binomial(link = "logit"),data=train)
  prob <- predict(model, type='response', newdata=test)
  roc <- roc( test $ Clasificacion ~ prob , plot = FALSE, print.auc = FALSE )
  aucs[kk]<-cbind(roc$auc)
}
plot( x = 1:100, y = aucs,  main = "Valor AUC para 100 iteraciones", xlab = "Iteración", ylab = "Valor AUC", col = "black", type = "p", pch = 1, xlim = c(0,100), ylim = c(0,1))
print(mean(aucs))

#Curva ROC representativa
attach(training_set)
modelo1<-glm(Clasificacion ~ Longitud+Diametro+PesoEntero+PesoDesbullado+PesoConcha+PesoVisceras, family = binomial(link = "logit"))
p=predict(modelo1, newdata =test_set, type='response')
roc=roc(test_set$Clasificacion~p,plot=TRUE, print.auc=TRUE)

#ARBOLES DE DECISION BINARIO

abalon_data$Clasificacion<-as.factor(abalon_data$Clasificacion)
attach(abalon_data)
modelo<-rpart(Clasificacion~Longitud+Diametro+PesoEntero+PesoDesbullado+PesoVisceras+PesoConcha,data=abalon_data)
rpart.plot(modelo) 

#CV Decision Tree
cvDecisionTree <- lapply(folds, function(x){
  training_fold <- training_set[-x, ]
  test_fold <- test_set[x, ]
  clasificador <- rpart(Clasificacion ~ Sexo+Diametro+Longitud+Altura+PesoEntero+PesoDesbullado+PesoVisceras+PesoConcha, data = training_fold)
  y_pred <- predict(clasificador, newdata = test_fold, type = 'class')
  cm <- table(test_fold$Clasificacion, y_pred)
  precision <- (cm[1,1] + cm[2,2]) / (cm[1,1] + cm[2,2] +cm[1,2] + cm[2,1])
  return(precision)
})
precisionDecisionTree <- mean(as.numeric(cvDecisionTree))
precisionDecisionTree

#CurvaROCValidacion
aucs = c()
for (kk in 1:100)
{
  train_index <- createDataPartition(abalon_data$Clasificacion, p = 0.8, list = FALSE, times = 1)
  train = abalon_data[train_index,]
  test = abalon_data[-train_index,]
  model <- rpart(Clasificacion ~ Sexo+Diametro+Longitud+Altura+PesoEntero+PesoDesbullado+PesoVisceras+PesoConcha, data = train)
  prob <- predict(model, newdata = test, type = 'vector')
  roc <- roc( test $ Clasificacion ~ prob , plot = FALSE, print.auc = FALSE )
  aucs[kk]<-cbind(roc$auc)
}
plot( x = 1:100, y = aucs,  main = "Valor AUC para 100 Iteraciones", xlab = "iteración", ylab = "Valor AUC", col = "black", type = "p", pch = 1, xlim = c(0,100), ylim = c(0,1))
print(mean(aucs))

#Curva ROC represetativa
attach(training_set)
modelo1<-rpart(Clasificacion~Sexo+Diametro+Longitud+Altura+PesoEntero+PesoDesbullado+PesoVisceras+PesoConcha,data=training_set)
p1=predict(modelo1, newdata =test_set, type='vector')
roc=roc(test_set$Clasificacion~p1,plot=TRUE, print.auc=TRUE)

