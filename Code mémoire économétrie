#############################################################
##  Seance:  Projet microéconométrie                       ##
##  Date  : 07/04/2024                                     ##
#############################################################

# #################
# 1.INTRODUCTION
# #################

## 1.1 Chargement des librairies
## -----------------------------
library(gmodels) # Cross Tables [CrossTable()]
library(ggmosaic) # Mosaic plot with ggplot [geom_mosaic()]
library(corrplot) # Correlation plot [corrplot()]
library(ggpubr) # Arranging ggplots together [ggarrange()]
library(cowplot) # Arranging ggplots together [plot_grid()]
library(caret) # ML [train(), confusionMatrix(), createDataPartition(), varImp(), trainControl()]
library(tidyverse) # Data manipulation
library(Matrix) # Sparse and Dense Matrix Classes and Methods
library(ROCR) # 
library(readxl)
library(stats)
library(factoextra)
library(FactoMineR)
library(cluster)
library(ggplot2)
library(NbClust)
library(ggplot2)
library(pscl)
library(margins)
library(skimr)
library(broom)
library(caret)
library(ROSE)
library(gridExtra)
library(openxlsx)
library(dplyr)
library(pROC)
library(DMwR)
library(zoo)
library(aod)
library(xts)
library(quantmod)
library(randomForest)
library(skimr)
library(gridExtra)
# Package SMOTE: il ne reste plus que des archives, il faut l'installer manuellement



## 1.2 Import du fichier .xlsx
## ---------------------------


train_data <- read_excel("Diagnostic_train.xlsx", sheet = 1, col_names = TRUE)


## 1.3 Description de la base
## --------------------------
dim(train_data)
names(train_data)
train_data <- train_data %>%
  rename(Last_treatment_PR_immobilized = `Last treatment PR immobilized`)

# création de la variable
train_data <- train_data %>%
  mutate(y = ifelse(is.na(Last_treatment_PR_immobilized) | Last_treatment_PR_immobilized != 2023, 0, 1))

summary(train_data)

CrossTable(train_data$Last_treatment_PR_immobilized)
CrossTable(train_data$Year_helicopter_flight)

head(train_data)
sum(is.na(train_data))
colSums(is.na(train_data))
# beaucoup de valeur manquante sur les variables

train_data$Nb_of_incident[is.na(train_data$Nb_of_incident)] <- 0
train_data$Nb_of_anomaly[is.na(train_data$Nb_of_anomaly)] <- 0

# On modifie la colonne date de service pour plus d'interprétabilité
train_data$Service_date <- as.numeric(substr(train_data$Service_date, 1, 4))
train_data$Service_date <- 2023-train_data$Service_date

#Pareil pour la colonne year_helicopter flight
train_data$Year_helicopter_flight = 2023 - train_data$Year_helicopter_flight



# On supprime les colonnes ne servant plus à rien
train_data <- subset(train_data, select = -ID_t)
train_data <- subset(train_data, select = -Last_treatment_PR_immobilized)


## ##################################################
## 2. ANALYSE EXPLORATOIRE ET TRAITEMENT DES DONNEES
## ##################################################

## Theme par defaut pour ggplot
## ----------------------------
theme_set(theme_bw())

## Parametres par defaut pour les graphes en mosaic
## ------------------------------------------------
mosaic_theme = theme(axis.text.x = element_text(angle = 90,
                                                hjust = 1,
                                                vjust = 0.5),
                     axis.text.y = element_blank(),
                     axis.ticks.y = element_blank())

# création de classe "(04) > 67 ans","(03) 56-67 ans","(02) 31-55 ans", "(01) <= 30 ans"
# Uniquement pour l'analyse exploratoire, pas pour la modélisation, penser à l'enlever lors de la modélisation.

train_dataacp <- train_data %>%
  mutate(cl_Duree = case_when(
    Service_date > 67 ~ 04,
    Service_date > 55 ~ 03,
    Service_date > 30 ~ 02,
    Service_date <= 30 ~ 01
  ))





### 2.1 ANALYSE UNIVARIEE
### ---------------------
skim(train_data)

# Visualisation des distributions
train_data %>%
  ggplot(aes(x = Service_date)) +  # Utilisation de la colonne Duree_annees
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +  # Réglage de la largeur des bacs et des couleurs
  scale_x_continuous(breaks = seq(0, 90, 5)) +  # Réglage des étiquettes de l'axe x
  labs(title = "Distribution de l'âge des lignes")  # Ajout d'un titre



histogram1 <- ggplot(train_data, aes(x = Electrical_length)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de la Longueur Électrique")

histogram2 <- ggplot(train_data, aes(x = Nb_of_incident)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution du nombre d'incident")

histogram3 <- ggplot(train_data, aes(x = Length_climate_hazard_plan)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de Length_climate_hazard_plan")

histogram4 <- ggplot(train_data, aes(x = Nb_of_anomaly)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de Nb_of_anomaly")

histogram5 <- ggplot(train_data, aes(x = Length_fragile_section)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de Length_fragile_section")

histogram6 <- ggplot(train_data, aes(x = Year_helicopter_flight)) +
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de l'année de la différence de l'année de passage de l'hélicoptère")

grid.arrange(histogram1, histogram2, histogram3, histogram4, histogram5, histogram6,ncol = 2) 




### 2.2 ANALYSE BIVARIEE
### --------------------

ggplot(train_data, aes(x = Electrical_length, y = Nb_of_incident)) +
  geom_point() +
  labs(title = "Relation entre la longueur du tronçon et nombre d'accident")

ggplot(train_data, aes(x = Nb_of_anomaly, y = Nb_of_incident)) +
  geom_point() +
  labs(title = "Relation entre nombre d'anomalie et le nombre d'accident")

ggplot(train_data, aes(x = Length_climate_hazard_plan, y = Nb_of_incident)) +
  geom_point() +
  labs(title = "Relation entre Longueur en plan aléa climatique et le nombre d'accident")

ggplot(train_data, aes(x = Length_fragile_section, y = Nb_of_incident)) +
  geom_point() +
  labs(title = "Relation entre la Longueur section fragile et le nombre d'accident")

# Calculer la matrice de corrélation

corr=cor(train_data)
corrplot(corr, method = "circle")

#sauvegarde des données
data_num = train_data %>% select(Nb_of_incident, Electrical_length , Length_climate_hazard_plan , Length_fragile_section, Nb_of_anomaly, Year_helicopter_flight, Service_date, y)

# Spécifiez le nom du fichier Excel de sortie
nom_fichier <- "data_train.xlsx"

# Exportez le dataframe vers le fichier Excel
write.xlsx(data_num, nom_fichier)



# EN CONCLUSION ----------------------------------------------------------------------
# Variables dichotomisees : service_date
# Variables discretisees  : aucune
# Variables retraitees    : Nb_of_incident, Nb_of_anomaly
# ------------------------------------------------------------------------------------



# visiualisation de cluster
library(FactoMineR)
library(factoextra)
fin2_data_num2 = train_data %>% select(Nb_of_incident, Electrical_length , Length_climate_hazard_plan , Length_fragile_section, Nb_of_anomaly, Year_helicopter_flight, cl_Duree, y)
# Effectuer la PCA
fin2_data_scaled <- scale(fin2_data_num2)

pca_result2 <- PCA(fin2_data_scaled, graph = TRUE)


# Utilisez la méthode du coude pour trouver le nombre optimal de clusters
wss <- numeric(10)  # Créez un vecteur pour stocker les valeurs de l'inertie


for (i in 1:25) {
  kmeans_result <- kmeans(fin2_data_scaled, centers = i)
  wss[i] <- sum(kmeans_result$tot.withinss)
}

# Tracez un graphique de l'inertie en fonction du nombre de clusters
plot(1:25, wss, type = "b", xlab = "Nombre de clusters (K)", ylab = "Inertie")

#k=10
K <- 10
kmeans_result <- kmeans(fin2_data_scaled, centers = K)


# Visualisation des clusters
fviz_cluster(kmeans_result, data = fin2_data_scaled)

#test indépendance, au vu de notre ACP.
cor_test_AB <- cor.test(train_data$y, train_data$Electrical_length, method = "pearson")
print(cor_test_AB)

cor_test_AB <- cor.test(train_data$y, train_data$cl_Duree , method = "pearson")
print(cor_test_AB)



## ##################################################
## 3. Modélisation Logit
## ##################################################

### 3.1 - Subdivision en train-test

set.seed(0)
trainIndex <- createDataPartition(train_data$y, p = .75, 
                                  list = FALSE, 
                                  times = 1)
train <- train_data[trainIndex,]


test <- train_data[-trainIndex,]


### 3.2 - Modélisation logit

logit <- glm(y ~ ., data = train, family = "binomial")
summary(logit)
predictions <- predict(logit, newdata = test, type = "response")
predictedClass <- ifelse(predictions > 0.5, 1, 0)
confusionMatrix <- confusionMatrix(as.factor(predictedClass), as.factor(test$y), mode = "everything")
print(confusionMatrix)
# Gros problème concernant l'équilibrage de classe, on ne prédit aucun vrai positif.
# 98% de précision
# kappa = 0 : pas de différence par rapport à une séléction de classe aléatoire


### 3.3 - Rééquilibrage des classes - Méthode SMOTE

train$y = as.factor(train$y)
train = as.data.frame(train)
train <- SMOTE(y ~ ., train,perc.over=100, perc.under=200)
table(train$y)


### 3.4 - Modèle Logit

logit2 <- glm(y ~ ., data = train, family = binomial(link=logit))
predictions2 <- predict(logit2, newdata = test, type="response")
preductedClass2 <- ifelse(predictions2 > 0.5,1,0)
table(preductedClass2)
confusionMatrix2 <- confusionMatrix(as.factor(preductedClass2), as.factor(test$y), mode = "everything")
print(confusionMatrix2)

#Odd ratio :

tidy_model <- broom::tidy(logit2, exponentiate = TRUE, conf.int = TRUE)
print(tidy_model)


### 3.5 Modèle probit

probit_model <- glm(y ~ ., data = train, family = "binomial"(link = probit))
summary(probit_model)
predictionsprobit <- predict(probit_model, newdata = test, type = "response")
predictedClassprobit <- ifelse(predictionsprobit > 0.5, 1, 0)
table(predictedClassprobit)
confusionMatrixprobit <- confusionMatrix(as.factor(predictedClassprobit), as.factor(test$y), mode = "everything")
print(confusionMatrixprobit)

### 3.6 Coefficients

summary(logit)
summary(logit2)
summary(probit_model)


## ##################################################
## 4. Evaluation des modèles
## ##################################################

### 4.1 - Résidus

# Obtenir les résidus du modèle
residuals1 <- residuals(logit)
residuals2 <- residuals(logit2)
hist(residuals2)

residuals_matrix <- cbind(residuals1,residuals2)
par(mfrow = c(2, 2))  # Diviser la fenêtre graphique en 2 lignes et 2 colonnes
for (i in 1:3) {
  hist(residuals_matrix[, i], main = paste("Modèle", i), xlab = "Résidus")
}
par(mfrow = c(1, 1))
plot(train$Nb_of_incident, residues)

### 4.2 - Courbe ROC et AUC
roc1 <- roc(test$y, predictedClass)
auc1 <- auc(roc1)
roc2 <- roc(test$y, preductedClass2)
auc2 <- auc(roc2)
roc3 <- roc(test$y, predictedClassprobit)
auc3 <- auc(roc3)


par(mfrow = c(2, 2))
plot(roc1, col = "blue", main = paste("AUC=",round(auc1,2)), xlab = "Taux de faux positifs", ylab = "Taux de vrais positifs")
plot(roc2, col = "red", main = paste("AUC=",round(auc2,2)), xlab = "Taux de faux positifs", ylab = "Taux de vrais positifs")
plot(roc3, col = "red", main = paste("AUC=",round(auc3,2)), xlab = "Taux de faux positifs", ylab = "Taux de vrais positifs")
par(mfrow = c(1, 1))

### 4.3 - Effets marginaux
margins1 <- margins(logit)
margins2 <- margins(logit2)
print(margins1)
print(margins2)




### 4.4 Peudo R2

pR2(logit)
pR2(logit2)
pR2(probit_model)

## ##################################################
## 4. Random forest
## ##################################################


tuneGrid <- expand.grid(.mtry = 2)
trainControl <- trainControl(method = "cv", number = 5)

rf_model <- train(y ~ ., data = train, 
                  method = "rf", 
                  tuneGrid = tuneGrid, 
                  trControl = trainControl,
                  ntree = 500) 
print(rf_model)
predictions <- predict(rf_model, newdata = test)
confusion_matrix <- table(predictions, test$y)
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print(paste("Accuracy:", accuracy))
plot(varImp(rf_model))





