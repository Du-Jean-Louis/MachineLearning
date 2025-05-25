getwd()
setwd("M5S2/Machine_Learning/DM")

ozone<-read.table("depSeuil.dat", header=TRUE, sep=",", stringsAsFactors=FALSE)

sum(complete.cases(ozone))
n <- nrow(ozone)  # Nombre d'observations

# Les données sont complètes

ozone$JOUR <- factor(ozone$JOUR, levels = c(0, 1), labels = c("Pas férié", "Férié"))
ozone$STATION <- as.factor(ozone$STATION)

str(ozone)

# Maintenant qu'on a prétraiter les données, on peut commencer l'analyse

library(lattice)
library(ggplot2)

# Convertir la variable binaire en facteur pour le boxplot

ggplot(ozone, aes(x = STATION, y = O3obs)) +
  geom_boxplot(fill = "purple") +
  labs(title = "Boxplot de la concentration d'ozone par station",
       x = "Station étudiée",
       y = "Concentration d'ozone") +
  theme_minimal()

ggplot(ozone, aes(x = O3obs, fill = JOUR)) +
  geom_histogram(position = "dodge", bins = 30) +
  labs(title = "Histogramme de la concentration d'ozone selon si le jour est férié",
       x = "Concentration d'ozone",
       y = "Effectif") +
  scale_fill_manual(values = c("red", "blue"), labels = c("Férié", "Pas férié")) +
  theme_minimal()

ggplot(ozone, aes(x = TEMPE, y = O3obs)) +
  geom_point(color = "blue", alpha = 0.6) +  
  labs(title = "Concentration d'ozone en fonction de la température",
       x = "Température",
       y = "Concentration d'ozone") +
  theme_minimal()

ggplot(ozone, aes(x = RMH2O, y = O3obs)) +
  geom_point(color = "green", alpha = 0.6) +  
  labs(title = "Concentration d'ozone en fonction de l'humidité",
       x = "Humidité",
       y = "Concentration d'ozone") +
  theme_minimal()

ggplot(ozone, aes(x = NO2, y = O3obs)) +
  geom_point(color = "yellow", alpha = 0.6) +  
  labs(title = "Concentration d'ozone en fonction de la concentration en dioxyde d'azote",
       x = "Concentration de dioxyde d'azote NO2",
       y = "Concentration d'ozone") +
  theme_minimal()

ggplot(ozone, aes(x = NO, y = O3obs)) +
  geom_point(color = "orange", alpha = 0.6) +  
  labs(title = "Concentration d'ozone en fonction de la concentration de monoxyde d'azote",
       x = "Concentration de monoxyde d'azote",
       y = "Concentration d'ozone") +
  theme_minimal()

ggplot(ozone, aes(x = MOCAGE, y = O3obs)) +
  geom_point(color = "red", alpha = 0.6) + 
  labs(title = "Concentration d'ozone en fonction de la prédiction par N-S",
       x = "Prédiction faite par Navier-Stokes",
       y = "Concentration d'ozone") +
  theme_minimal()

ggplot(ozone, aes(x = VentMOD, y = O3obs)) +
  geom_point(color = "cyan", alpha = 0.6) +  
  labs(title = "Concentration d'ozone en fonction du module du vent",
       x = "Module du vent",
       y = "Concentration d'ozone") +
  theme_minimal()

ggplot(ozone, aes(x = VentANG, y = O3obs)) +
  geom_point(color = "pink", alpha = 0.6) + 
  labs(title = "Concentration d'ozone en fonction de l'angle du vent",
       x = "Angle du vent",
       y = "Concentration d'ozone") +
  theme_minimal()

# Classification supervisée

library(kernlab)
library(tidyverse)
library(caret)
library(nnet) 

# On commence par la régression multi-linéaire
# On prend toutes les variables explicatives
set.seed(20)
param_cv <- trainControl(method="cv",number=5)
fit_glm <- train(O3obs ~ ., data = ozone,
                 method="glm",
                 trControl=param_cv)
p <- length(fit_glm$finalModel$coefficients) - 1  # Nombre de variables explicatives
cat("R2 classique : ", fit_glm$results$Rsquared, "\t R2 ajusté : ",1-(1-fit_glm$results$Rsquared)*(n-1)/(n-p-1))

# Sélectionner les variables
# Seulement les 3 variables qu'on a remarqué lors de l'analyse descriptive
set.seed(20)
fit_glm1 <- train(O3obs ~ TEMPE + MOCAGE + STATION, data = ozone,
                  method="glm",
                  trControl=param_cv)
# C'est la variance expliqué par le modèle, on veut le maximiser
p <- length(fit_glm1$finalModel$coefficients) - 1  # Nombre de variables explicatives
cat("R2 classique : ", fit_glm1$results$Rsquared, "\t R2 ajusté : ",1-(1-fit_glm1$results$Rsquared)*(n-1)/(n-p-1))
# On voit que ce n'est pas suffisant pour bien expliqué notre problème

# On rajoute la variable JOUR
set.seed(20)
fit_glm2 <- train(O3obs ~ TEMPE + MOCAGE + STATION + JOUR, data = ozone,
                  method="glm",
                  trControl=param_cv)
# C'est la variance expliqué par le modèle, on veut le maximiser
p <- length(fit_glm2$finalModel$coefficients) - 1  # Nombre de variables explicatives
cat("R2 classique : ", fit_glm2$results$Rsquared, "\t R2 ajusté : ",1-(1-fit_glm2$results$Rsquared)*(n-1)/(n-p-1))
# On voit que cette variable fait diminuer le R2 ajusté, elle n'est donc pas explicative

# On enlèves JOUR, et on rajoute les mesures d'humidité et d'azote
set.seed(20)
fit_glm3 <- train(O3obs ~ TEMPE + MOCAGE + STATION + RMH2O + NO + NO2, data = ozone,
                  method="glm",
                  trControl=param_cv)
# C'est la variance expliqué par le modèle, on veut le maximiser
p <- length(fit_glm3$finalModel$coefficients) - 1  # Nombre de variables explicatives
cat("R2 classique : ", fit_glm3$results$Rsquared, "\t R2 ajusté : ",1-(1-fit_glm3$results$Rsquared)*(n-1)/(n-p-1))
# On améliore le R2 ajusté

# On ajoute maintenant les variables portant sur le vent
set.seed(20)
fit_glm4 <- train(O3obs ~ TEMPE + MOCAGE + STATION + RMH2O + NO + NO2 + VentMOD + VentANG, data = ozone,
                  method="glm",
                  trControl=param_cv)
# C'est la variance expliqué par le modèle, on veut le maximiser
p <- length(fit_glm4$finalModel$coefficients) - 1  # Nombre de variables explicatives
cat("R2 classique : ", fit_glm4$results$Rsquared, "\t R2 ajusté : ",1-(1-fit_glm4$results$Rsquared)*(n-1)/(n-p-1))
# On obtient un R2 ajusté légèrement meilleur que celui où l'on prend toutes les variables

# On obtient ainsi comme R2 optimale : 53.4%

# Test de RF
set.seed(20)
fit_rf<- train(O3obs ~.,
               data = ozone, method = "rf",
               tuneLength=5,trControl = param_cv)
max(fit_rf$results$Rsquared)

# R2 : 61.2%

# Boosting
set.seed(20)
fit_gbm <- train(O3obs ~ ., 
                 data = ozone, 
                 method = "gbm",  
                 trControl = param_cv,  # Cross-validation
                 tuneLength = 5,  
                 verbose = FALSE)  # Désactive l'affichage des logs
max(fit_gbm$results$Rsquared)

# R2 : 59.6%

# Réseaux de neuronnes

set.seed(20)
# Paramètre qu'il teste pour l'apprentissage
grid <- expand.grid(size = c(3, 5, 7), decay = c(0.01, 0.1, 1))
fit_rn <- train(O3obs ~ ., data = ozone, method = "nnet", 
                trControl = param_cv, linout = TRUE, maxit = 100, tuneGrid = grid)
max(fit_rn$results$Rsquared)

# R2 : 58.9%

# On va séparer les données en 2, un set d'entrainement et un set de test
# Puis on va calculer l'erreur empirique sur le set de test

set.seed(30)
indic <- createDataPartition(ozone$O3obs, p=0.9,list = FALSE)

#sous-echantillon d'apprentissage et de test
train <- ozone[indic,]
test <- ozone[-indic,]

set.seed(20)
fit_final<- train(O3obs ~.,
               data = train, method = "rf",
               tuneLength=5,trControl = param_cv)
max(fit_final$results$Rsquared)

pred_final<-predict(fit_final,test)

Erreur_empirique = mean((pred_final - test$O3obs)^2)
Erreur_empirique

# Observation de 3 individus dans l'échantillon de test
# On a choisi une seed pour qu'on ait "oui" et "non"
set.seed(20)
indic_bis <- sample(nrow(test), 3)  # Tire 3 lignes au hasard
print(indic_bis)  # Affiche les numéros de ligne
test[indic_bis, ]  # Sélectionne ces lignes

# Sur ces trois cas, on voit que notre prédicteur se rapproche des valeurs réelles
pred_final[indic_bis]
test[indic_bis, ]$O3obs
