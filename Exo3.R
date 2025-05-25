getwd()
setwd("M5S2/Machine_Learning/DM")

pokemon <- read.csv("pokemon_competitive_analysis.csv")

# On veut étudier si un pokemon est compétitif, pour cela on va juste se baser sur l'année 2024
# On enlèves donc les autres années

pokemon <- pokemon[,-c(1,18,19,20,21,22)]

# On va transformer cette variable en variable binaire
colnames(pokemon)[colnames(pokemon) == "Worlds_VGC_Usage_2024"] <- "Usage"
pokemon$Usage <- ifelse(pokemon$Usage == "NoUsage", "Pas compétitif", "Compétitif")
pokemon$Usage <- as.factor(pokemon$Usage)
table(pokemon$Usage)

pokemon$types <- ifelse(pokemon$type2 == "No_type", 
                               pokemon$type1, 
                               paste(pmin(pokemon$type1, pokemon$type2), 
                                     pmax(pokemon$type1, pokemon$type2), sep = "-"))
pokemon$types <- as.factor(pokemon$types)
# On enlèves type1 et type2
pokemon <- pokemon[,-c(2,3)]

# Deux cas pour que le pokemon ne soit pas utilisé, soit il est banni car trop puissant, soit il n'est pas assez fort pour être jouer

library(lattice)
library(ggplot2)

ggplot(pokemon, aes(x = Usage, y = attack, fill = Usage)) +
  geom_boxplot() +
  labs(title = "Comparaison de l'Attaque selon l'Usage", x = "Usage", y = "Attaque") +
  theme_minimal()

ggplot(pokemon, aes(x = Usage, y = defense, fill = Usage)) +
  geom_boxplot() +
  labs(title = "Comparaison de le Defense selon l'Usage", x = "Usage", y = "Attaque") +
  theme_minimal()

ggplot(pokemon, aes(x = Usage, y = sp_atk, fill = Usage)) +
  geom_boxplot() +
  labs(title = "Comparaison de l'Attaque spe selon l'Usage", x = "Usage", y = "Attaque") +
  theme_minimal()

ggplot(pokemon, aes(x = Usage, y = sp_def, fill = Usage)) +
  geom_boxplot() +
  labs(title = "Comparaison de la Defense spe selon l'Usage", x = "Usage", y = "Attaque") +
  theme_minimal()

ggplot(pokemon, aes(x = Usage, y = speed, fill = Usage)) +
  geom_boxplot() +
  labs(title = "Comparaison de la vitesse selon l'Usage", x = "Usage", y = "Vitesse") +
  theme_minimal()

ggplot(pokemon, aes(x = Usage, y = hp, fill = Usage)) +
  geom_boxplot() +
  labs(title = "Comparaison des PV selon l'Usage", x = "Usage", y = "PV") +
  theme_minimal()

ggplot(pokemon, aes(x = Usage, y = total_stats, fill = Usage)) +
  geom_boxplot() +
  labs(title = "Comparaison des statistiques totales selon l'Usage", x = "Usage", y = "Statistiques totales") +
  theme_minimal()

ggplot(pokemon, aes(x = generation, fill = Usage)) +
  geom_bar(position = "fill") +
  labs(title = "Nombre de Pokémon compétitifs par generation",
       x = "Type de Pokémon", y = "Nombre de Pokémon") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(pokemon, aes(x = types, fill = Usage)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion de Pokémon compétitifs par Type",
       x = "Type de Pokémon", y = "Nombre de Pokémon") +
  scale_x_discrete(breaks = NULL)


library(kernlab)
library(tidyverse)
library(caret)


# Test de la regression logistique
param_cv <- trainControl(method="cv",number=5)
set.seed(20)
fit_glm <- train(Usage ~ hp + attack + defense + sp_atk + sp_def + speed + total_stats + generation + legendary + mythical + types, data = pokemon,
                 method="glm",
                 family="binomial",
                 trControl=param_cv)
# C'est l'erreur moyen car on fait une cross validation en 5 folds
1-fit_glm$results[,"Accuracy"]

# 18% sans les talents

# Test des arbres
set.seed(20)
fit_cart <- train(Usage ~ ., data = pokemon,
                  method="rpart",
                  trControl=param_cv)
#fit_cart$results[,c("cp","Accuracy")]
#fit_cart$bestTune
1-max(fit_cart$results$Accuracy)

# Erreur de 14.4%

# Test de RF
set.seed(20)
fit_rf<- train(Usage ~ hp + attack + defense + sp_atk + sp_def + speed + total_stats + generation + legendary + mythical + types,
               data = pokemon, method = "rf",
               tuneLength=5,trControl = param_cv)
1- max(fit_rf$results$Accuracy)

# Erreur de 14.4%

# Test des k-plus proche voisins
set.seed(20)
fit_knn<- train(Usage ~ hp + attack + defense + sp_atk + sp_def + speed + total_stats + generation + legendary + mythical + types, data = pokemon,
                method = "knn",
                trControl = param_cv, tuneLength = 5)
fit_knn$results[,c("k","Accuracy")]
1- max(fit_knn$results$Accuracy)

# Erreur de 14.8%

set.seed(20)
fit_gbm <- train(Usage ~ hp + attack + defense + sp_atk + sp_def + speed + total_stats + generation + legendary + mythical + types, 
                 data = pokemon, 
                 method = "gbm",  
                 trControl = param_cv,  # Cross-validation
                 tuneLength = 5,  
                 verbose = FALSE)  
1- max(fit_gbm$results$Accuracy)

# Erreur 13.3%

# Réseaux de neuronnes
set.seed(20)
grid <- expand.grid(size = c(3, 5), decay = c(0.01, 0.1, 1))
fit_rn <- train(Usage ~ hp + attack + defense + sp_atk + sp_def + speed + total_stats + generation + legendary + mythical + types, data = pokemon, method = "nnet", 
                trControl = param_cv, linout = FALSE, maxit = 100, tuneGrid = grid)
1-max(fit_rn$results$Accuracy)


# On prend le modèle Gradient Boosting, il donne l'erreur la plus faible, autour de 13% 

# On va séparer les données en 2, un set d'entrainement et un set de test
# Puis on va calculer l'erreur empirique sur le set de test
set.seed(20)
indic <- createDataPartition(pokemon$Usage, p=0.9,list = FALSE)

#sous-echantillon d'apprentissage et de test
train <- pokemon[indic,]
test <- pokemon[-indic,]

train$types <- factor(train$types, levels = levels(pokemon$types))
test$types <- factor(test$types, levels = levels(pokemon$types))

set.seed(20)
fit_final<- train(Usage ~hp + attack + defense + sp_atk + sp_def + speed + total_stats + generation + legendary + mythical + types,
               data = train, method = "gbm",
               tuneLength=5,trControl = param_cv,verbose = FALSE)
1- max(fit_rf$results$Accuracy)


score <- predict(fit_final, test, type="prob")
prediction <- ifelse(score > 0.5, "Compétitif", "Pas compétitif")[,1]
conf = table(obs=test$Usage,pred=prediction)
conf

Erreur_empirique = (conf["Pas compétitif","Compétitif"]+conf["Compétitif","Pas compétitif"])/(sum(conf))
Erreur_empirique

# Vérifions que notre prédicteur est efficace peu importe le seuil
ROC <- pROC::roc(test$Usage=="Compétitif",score[,"Compétitif"])
plot(1-ROC$specificities,ROC$sensitivities,type="l")
abline(0,1)
ROC$auc

# Observation de 3 individus dans l'échantillon de test
set.seed(20)
indic_bis <- sample(nrow(test), 3)  # Tire 3 lignes au hasard
print(indic_bis)  # Affiche les numéros de ligne
test[indic_bis, ]  # Sélectionne ces lignes

score[indic_bis,]
prediction[indic_bis]
test[indic_bis, ]$Usage
