getwd()
setwd("M5S2/Machine_Learning/DM")

# On charge la base de données, on a 1417 observations pour 56 variables
visa_full <- read.table("visa_raw_R.dat", header=TRUE, sep="\t", stringsAsFactors=FALSE)

# On doit faire un prétraitement des données

# On enlève les variables GxxGxxS car elles sont majoritairement renseignées inc
visa_full <- subset(visa_full, select = -c(G25G26S, G03G04S, G29G30S, G35G36S,G37G38S,G45G46S,G47G48S))

# Avec ce qu'il reste on enlève les observations manquantes
visa_full[visa_full == "." | visa_full == "inc"] <- NA  
visa_full <- na.omit(visa_full)  
# Il nous reste 753 observations complètes

# On transforme en variables numériques et factor 

visa_full[, c(1,5,7,10:48)] <- lapply(visa_full[, c(1,5,7,10:48)], as.numeric)
visa_full[, c(2,3,6,8,49)] <- lapply(visa_full[, c(2,3,6,8,49)], as.factor)
visa_full[,c(4)] <- factor(visa_full[, c(4)], levels = c(1, 2), labels = c("Femme", "Homme"))


visa_full[,"familr"] <- as.numeric(visa_full[,"FAMIQ"])

visa_full[,"sexer"] <- 2 - as.numeric(visa_full[,"SEXEQ"]) # H:0 F:1
visa_full[,"kvunbq"] <- cut(visa_full$KVUNB, 
                            breaks=c(min(visa_full$KVUNB),1.5,max(visa_full$KVUNB)), 
                            labels = c("K0","K1"), include.lowest = TRUE)
visa_full[,"vienbq"] <- cut(visa_full$VIENB, 
                            breaks=c(min(visa_full$VIENB),0.5,max(visa_full$VIENB)), 
                            labels = c("V0","V1"), include.lowest = TRUE)
visa_full[,"uemnbq"] <- cut(visa_full$UEMNB, 
                            breaks=c(min(visa_full$UEMNB),0.5,1.5,max(visa_full$UEMNB)), 
                            labels = c("U0","U1","U2"), include.lowest = TRUE)
visa_full[,"xlgnbq"] <- cut(visa_full$XLGNB, 
                            breaks=c(min(visa_full$XLGNB),0.5,1.5,max(visa_full$XLGNB)), 
                            labels = c("X0","X1","X2"), include.lowest = TRUE)
visa_full[,"ylvnbq"] <- cut(visa_full$YLVNB, 
                            breaks=c(min(visa_full$YLVNB),0.5,1.5,max(visa_full$YLVNB)), 
                            labels = c("Y0","Y1","Y2"), include.lowest = TRUE)
visa_full[,"rocnbq"] <- cut(visa_full$ROCNB, 
                            breaks=c(min(visa_full$ROCNB),0.5,max(visa_full$ROCNB)), 
                            labels = c("R0","R1"), include.lowest = TRUE)
visa_full[,"nptagq"] <- cut(visa_full$NPTAG, 
                            breaks=c(min(visa_full$NPTAG),0.5,max(visa_full$NPTAG)), 
                            labels = c("N0","N1"), include.lowest = TRUE)
visa_full[,"endetq"] <- cut(ceiling(visa_full$ENDET), 
                            breaks=c(min(ceiling(visa_full$ENDET)),0.5,max(ceiling(visa_full$ENDET))), 
                            labels = c("E0","E1"), include.lowest = TRUE)
visa_full[,"gagetq"] <- cut(ceiling(visa_full$GAGET), 
                            breaks=c(min(ceiling(visa_full$GAGET)),0.5,max(ceiling(visa_full$GAGET))), 
                            labels = c("G0","G1"), include.lowest = TRUE)
visa_full[,"facanq"] <- cut(ceiling(visa_full$FACAN), 
                            breaks=c(min(ceiling(visa_full$FACAN)),0.5,max(ceiling(visa_full$FACAN))), 
                            labels = c("F0","F1"), include.lowest = TRUE)
visa_full[,"lgagtq"] <- cut(ceiling(visa_full$LGAGT), 
                            breaks=c(min(ceiling(visa_full$LGAGT)),0.5,max(ceiling(visa_full$LGAGT))), 
                            labels = c("L0","L1"), include.lowest = TRUE)
visa_full[,"havefq"] <- cut(ceiling(visa_full$HAVEF), 
                            breaks=c(min(ceiling(visa_full$HAVEF)),0.5,max(ceiling(visa_full$HAVEF))), 
                            labels = c("H0","H1"), include.lowest = TRUE)
visa_full[,"ageq"] <- cut(visa_full$AGER, breaks=quantile(visa_full[,"AGER"], probs = seq(0, 1, 1/3)),
                          labels = c("A0","A1","A2"), include.lowest = TRUE)
visa_full[,"relatq"] <- cut(visa_full$RELAT, breaks=quantile(visa_full[,"RELAT"], probs = seq(0, 1, 1/3)),
                            labels = c("R0","R1","R2"), include.lowest = TRUE)
visa_full[,"qsmoyq"] <- cut(visa_full$QSMOY, breaks=quantile(visa_full[,"QSMOY"], probs = seq(0, 1, 1/3)),
                            labels = c("Q0","Q1","Q2"), include.lowest = TRUE)
visa_full[,"opgnbq"] <- cut(visa_full$OPGNB,
                            breaks=c(0,0.0000001,2,16),
                            labels = c("O0","O1","O2"), include.lowest = TRUE)
visa_full[,"moyrvq"] <- cut(visa_full$MOYRV, breaks=quantile(visa_full[,"MOYRV"], probs = seq(0, 1, 1/3)),
                            labels = c("M0","M1","M2"), include.lowest = TRUE)
visa_full[,"tavepq"] <- cut(visa_full$TAVEP, breaks=quantile(visa_full[,"TAVEP"], probs = seq(0, 1, 1/3)),
                            labels = c("T0","T1","T2"), include.lowest = TRUE)
visa_full[,"dmvtpq"] <- cut(visa_full$DMVTP, breaks=quantile(visa_full[,"DMVTP"], probs = seq(0, 1, 1/3)),
                            labels = c("D0","D1","D2"), include.lowest = TRUE)
visa_full[,"boppnq"] <- cut(visa_full$BOPPN, breaks=quantile(visa_full[,"BOPPN"], probs = seq(0, 1, 1/3)),
                            labels = c("B0","B1","B2"), include.lowest = TRUE)
visa_full[,"jnbjdq1"] <- cut(visa_full$JNBJD1S,
                             breaks=c(0,0.0001,2.8,100),
                             labels = c("J0","J1","J2"), include.lowest = TRUE)
visa_full[,"jnbjdq2"] <- cut(visa_full$JNBJD2S,
                             breaks=c(0,0.0001,2.8,100),
                             labels = c("J0","J1","J2"), include.lowest = TRUE)
visa_full[,"jnbjdq3"] <- cut(visa_full$JNBJD3S,
                             breaks=c(0,0.0001,2.8,100),
                             labels = c("J0","J1","J2"), include.lowest = TRUE)
visa_full[,"itavcq"] <- cut(visa_full$ITAVC, breaks=quantile(visa_full[,"ITAVC"], probs = seq(0, 1, 1/3)),
                            labels = c("I0","I1","I2"), include.lowest = TRUE)

# Réorganisation de la table
# la table vispremv aura la forme suivante : 
# les variables quantitatives
# les variables qualitatives (d'origine et transformées)


visa_full<-visa_full[,c(names(visa_full)[c(1,5,7,50,51)],names(visa_full)[10:48],
                      names(visa_full)[2:4],names(visa_full)[c(6,8,9)],names(visa_full)[52:75],
                      names(visa_full)[49])]  

visa_num <- visa_full[,c(1:46,75)]   
visa_cat <- visa_full[,c(47:75)]  


# Maintenant qu'on a prétraiter les données, on peut commencer l'analyse

library(lattice)
library(ggplot2)
# Convertir la variable binaire en facteur pour le boxplot

# Tracer le barplot

ggplot(visa_full, aes(x = itavcq, fill = CARVP)) +
  geom_bar(position = "fill") +  
  labs(title = "Possession de la carte VP selon les avoirs sur tous les comptes", 
       x = "Avoir sur tous les comptes", 
       y = "Proportion", 
       fill = "Possède la carte VP") +
  scale_fill_manual(values = c("red", "blue"), labels = c("non", "oui")) +
  theme_minimal()

# On voit qu'en proportion, plus on est dans une catégorie haute, plus l'on a de chances de posséder la carte VP

ggplot(visa_full, aes(x = NTCAS, fill = CARVP)) +
  geom_bar(position = "dodge") +  
  labs(title = "Possession de la carte VP en fonction du nombre de cartes possédé", 
       x = "Nombre de carte possédé", 
       y = "Effectif", 
       fill = "Possède la carte VP") +
  scale_fill_manual(values = c("red", "blue"), labels = c("non", "oui")) +
  theme_minimal()

# On remarque qu'en effectif il y a plus de gens possédant un nombre de cartes moins important
# En proportion, on voit que plus le nombre de cartes possédés est élevés plus la proportion de gens possédant la carte VP est élevée aussi

ggplot(visa_full, aes(x = CARVP, y = RELAT, fill = CARVP)) +
  geom_boxplot() +
  labs(title = "Possession de la carte VP selon la durée de la relation client-banque", 
       x = "Possède la carte VP", 
       y = "Durée de la relation banque-client", 
       fill = "Possession de la carte VP") +
  scale_fill_manual(values = c("red", "blue"), labels = c("non", "oui")) +
  theme_minimal()

# On s'attend à trouver un lien entre la longévité de la relation entre le client et sa banque et la possession de carte VP
# Mais on remarque que cette relation n'existe pas où est très faible

ggplot(visa_full, aes(x = FAMIQ, fill = CARVP)) +
  geom_bar(position = "fill") +  
  labs(title = "Possession de la carte VP selon la situation familiale", 
       x = "Situation familliale", 
       y = "Proportion", 
       fill = "Possède la carte VP") +
  scale_fill_manual(values = c("red", "blue"), labels = c("non", "oui")) +
  theme_minimal()

# On voit des légères tendances, les personnes déclarant être en couple ont légèrement plus de chance d'en posséder une

ggplot(visa_full, aes(x = SEXEQ, fill = CARVP)) +
  geom_bar(position = "fill") +  
  labs(title = "Possession de la carte VP selon le sexe", 
       x = "Sexe", 
       y = "Proportion", 
       fill = "Possède la carte VP") +
  scale_fill_manual(values = c("red", "blue"), labels = c("non", "oui")) +
  theme_minimal()

# On remarque que les femmes ont plus tendances à posséder une carte VP

ggplot(visa_full, aes(x = opgnbq, fill = CARVP)) +
  geom_bar(position = "fill") +  
  labs(title = "Possession de carte VP selon le nombre d'opération par mois", 
       x = "Opération par mois (catégorie)", 
       y = "Proportion", 
       fill = "Possède la carte VP") +
  scale_fill_manual(values = c("red", "blue"), labels = c("non", "oui")) +
  theme_minimal()

# On observe un lien entre opération par mois et possession de la carte VP

ggplot(visa_full, aes(x = AGER, fill = CARVP)) +
  geom_bar(position = "dodge") +  
  labs(title = "Possession de la carte VP en fonction du nombre de cartes possédé", 
       x = "Nombre de carte possédé", 
       y = "Effectif", 
       fill = "Possède la carte VP") +
  scale_fill_manual(values = c("red", "blue"), labels = c("non", "oui")) +
  theme_minimal()

# On ne remarque pas de lien évident entre l'âge et la possession de carte VP
# Néanmoins on voit que les personnes très jeunes et très âgées n'en possèdent pas
# Lors de la phase de traitement des données, on aurait pu écarter ces populations comme cela a été fait dans l'étude


# Classification supervisée
library(kernlab)
library(tidyverse)
library(caret)
library(nnet) 
library(pROC)

# Test de la regression logistique
param_cv <- trainControl(method="cv",number=5)

set.seed(20)
fit_glm <- train(CARVP ~ ., data = visa_full,
                 method="glm",
                 family="binomial",
                 trControl=param_cv)
# C'est l'erreur moyen car on fait une cross validation en 10 folds
1-fit_glm$results[,"Accuracy"]

# Version seulement variables quantitatives
set.seed(20)
fit_glm1 <- train(CARVP ~ ., data = visa_num,
                 method="glm",
                 family="binomial",
                 trControl=param_cv)
# C'est l'erreur moyen car on fait une cross validation en 10 folds
1-fit_glm1$results[,"Accuracy"]

# Version seulement variables qualitatives
set.seed(20)
fit_glm2 <- train(CARVP ~ ., data = visa_cat,
                 method="glm",
                 family="binomial",
                 trControl=param_cv)
# C'est l'erreur moyen car on fait une cross validation en 10 folds
1-fit_glm2$results[,"Accuracy"]

# On voit que cette méthode fonctionne mieux sur seulement les variables numériques
# Environ les mêmes performances sur le reste
# Erreur de 18.1%

# Test des arbres
set.seed(20)
fit_cart <- train(CARVP ~ ., data = visa_num,
                  method="rpart",
                  trControl=param_cv)
1-max(fit_cart$results$Accuracy)

# Cette méthode a l'air de mieux fonctionner sur les variables numériques aussi
# Elle est beaucoup moins efficace sur seulement les variables qualitatives
# Erreur de 11.7%

# Test de RF
set.seed(20)
fit_rf<- train(CARVP ~.,
               data = visa_full, method = "rf",
               tuneLength=5,trControl = param_cv)
1- max(fit_rf$results$Accuracy)

# Cette méthode fonctionne mieux sur toutes les variables
# Et beaucoup moins bien sur seulement les variables qualitatives
# Erreur de 8.9% 

# Test des k-plus proche voisins
set.seed(20)
fit_knn<- train(CARVP ~ ., data = visa_cat,
                method = "knn",
                trControl = param_cv, tuneLength = 5)
fit_knn$results[,c("k","Accuracy")]
1- max(fit_knn$results$Accuracy)

# Contrairement aux autres, elle fonctionne mieux sur seulement les variables catégorielles

set.seed(20)
fit_gbm <- train(CARVP ~ ., 
                 data = visa_full, 
                 method = "gbm",  
                 trControl = param_cv,  # Cross-validation
                 tuneLength = 5,  
                 verbose = FALSE)  
1- max(fit_gbm$results$Accuracy)

# La méthode de Gradient Boosting fonctionne le mieux sur toutes les données
# Erreur 7.1%

# Réseaux de neuronnes
set.seed(20)
grid <- expand.grid(size = c(3), decay = c(0.01, 0.1, 1))
fit_rn <- train(CARVP ~ ., data = visa_cat, method = "nnet", 
                trControl = param_cv, linout = FALSE, maxit = 100, tuneGrid = grid)
1-max(fit_rn$results$Accuracy)

# La meilleure méthode est celle avec seulement les variables qualitatives
# Erreur de 13%



# On prend le modèle de boosting, il donne l'erreur la plus faible, autour de 9% contre 25% pour les plus proches voisins et la régression logistiques

# On va séparer les données en 2, un set d'entrainement et un set de test
# Puis on va calculer l'erreur empirique sur le set de test

set.seed(20)
indic <- createDataPartition(visa_full$CARVP, p=0.9,list = FALSE)

#sous-echantillon d'apprentissage et de test
train <- visa_full[indic,]
test <- visa_full[-indic,]

set.seed(20)
param_cv2 <- trainControl(method="cv",number=5)
fit_gbm <- train(CARVP ~ ., 
                 data = train, 
                 method = "gbm",  
                 trControl = param_cv,  # Cross-validation
                 tuneLength = 5,  
                 verbose = FALSE)  
1- max(fit_gbm$results$Accuracy)

# Erreur sur les données d'entrainement 7.9%

# Faisons l'erreur empirique sur les données de tests
score <- predict(fit_gbm, test, type="prob")
prediction <- predict(fit_gbm, test)
conf = table(obs=test$CARVP,pred=prediction)
conf

Erreur_empirique = (conf["non","oui"]+conf["oui","non"])/(sum(conf))
Erreur_empirique

# Vérifions que notre prédicteur est efficace peu importe le seuil
ROC <- pROC::roc(test$CARVP=="oui",score[,"oui"])
plot(1-ROC$specificities,ROC$sensitivities,type="l")
abline(0,1)
ROC$auc


# Observation de 3 individus dans l'échantillon de test
# On a choisi une seed pour qu'on ait "oui" et "non"
set.seed(20)
indic_bis <- sample(nrow(test), 3)  # Tire 3 lignes au hasard
print(indic_bis)  # Affiche les numéros de ligne
test[indic_bis, ]  # Sélectionne ces lignes

# On regarde le score, si la proba de "oui" est supérieur à 0.5 on prédit "oui"
# Sur ces trois cas, on voit que notre prédicteur ne s'est pas trompé avec pour seuil 0.5
# Si on prenait un seuil de 95%, on aura deviner non pour tout et l'on se serait trompé par exemple
score[indic_bis,]
prediction[indic_bis]
test[indic_bis, ]$CARVP

