#############################################################
##  Seance:  Projet data science                           ##
##                                                         ##
#############################################################

# #################
# 1.INTRODUCTION  #
# #################


rm(list=ls())

## 1.1 Chargement des librairies
## -----------------------------

library(kableExtra)
library(broom)
library(knitr)
library(corrplot)
library(GGally)
library(dplyr)
library(ggplot2)
library(reshape2)
library(caret)
library(readxl)
library(xgboost)
library(FactoMineR)
library(gmodels)
library(skimr)
library(ggmosaic)
library(ggpubr)
library(cowplot)
library(tidyverse)
library(gmodels)
library(ggpubr) 
library(Matrix) 
library(ROCR) 
library(xlsx)
library(stats)
library(factoextra)
library(cluster)
library(NbClust)
library(gridExtra)
library(openxlsx)


## 1.2 Import des fichiers
## ---------------------------

X_train = read.csv('X_train_new_data.csv')
Y_train = read.csv('y_train_saegPGl_new_data.csv')
X_test = read.csv('X_test_new_data.csv')
externe = read_excel('externe.xls')


## 1.3 Description de la base
## --------------------------
str(X_train)

id_train <- X_train$Identifiant
id_test <- X_test$Identifiant
X_train_test <- rbind(X_train, X_test)
dim(X_train_test)
names(X_train_test)
summary(X_train_test)
head(X_train_test)
sum(is.na(X_train_test))
str(X_train_test)
X_train_test$EXPO <- as.numeric(gsub(",", ".", X_train_test$EXPO))


## 1.4 One-hot-encoding
## --------------------------


#On recupère les colonnes catégorielles et on les transforme en numérique
colonnes_categ <- names(X_train_test)[sapply(X_train_test, function(x) is.character(x) | is.factor(x))]
colonnes_categ <- colonnes_categ[colonnes_categ != "Insee"]
X_train_test[colonnes_categ] <- lapply(X_train_test[colonnes_categ], as.factor)
dummy_model <- dummyVars(~ . - Insee, data=X_train_test)
data_transformed <- predict(dummy_model, newdata = X_train_test)
data_transformed <- as.data.frame(data_transformed)
X_train_test <- X_train_test[, setdiff(names(X_train_test), colonnes_categ)]
X_train_test_final <- cbind(X_train_test, data_transformed)
#On s'occupe des colonnes dupliqués, des noms à changer...
names(X_train_test_final) <- make.unique(names(X_train_test_final))
X_train_test_final =X_train_test_final %>% select(-X.1)
X_train_test_final =X_train_test_final %>% select(-Identifiant.1)
X_train_test_final =X_train_test_final %>% select(-EXPO.1)
X_train_test_final =X_train_test_final %>% select(-superficief.1)
#On sépare en Train, Test
X_train <- X_train_test_final[X_train_test_final$Identifiant %in% id_train, ]
X_test <- X_train_test_final[X_train_test_final$Identifiant %in% id_test, ]
Train <- merge(X_train, Y_train, by = "Identifiant")
#On règle les petits problèmes (noms, duplication,-1)
colnames(Train)[colnames(Train) == 'X.x'] <- 'X' 
Train <- Train[, !(names(Train) %in% c("X.y"))]
Train <- Train[, !(names(Train) %in% c("ft_24_categ.   ."))]
names(Train) <- gsub(" +", " ", names(Train)) 
names(Train) <- gsub(" ", "_", names(Train)) 
names(Train) <- gsub("ft_24_categ.>=10", "ft_24_categ_gte_10", names(Train))
names(X_test) <- gsub(" +", " ", names(X_test)) 
names(X_test) <- gsub(" ", "_", names(X_test)) 
names(X_test) <- gsub("ft_24_categ.>=10", "ft_24_categ_gte_10", names(X_test))
mean_expo <- mean(Train$EXPO[Train$EXPO != -1])
mean_expo <- mean(X_test$EXPO[X_test$EXPO != -1])
Train$EXPO[Train$EXPO == -1] <- mean_expo
Train$EXPO[X_test$EXPO == -1] <- mean_expo


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



### 2.1 ANALYSE UNIVARIEE
### ---------------------
skim(X_train_test)

# Visualisation des distributions

Train %>%
  ggplot(aes(x = target)) +  # Utilisation de la colonne Duree_annees
  geom_histogram(binwidth = 1, fill = "blue", color = "black") +  # Réglage de la largeur des bacs et des couleurs
  scale_x_continuous(breaks = seq(0, 90, 5)) +  # Réglage des étiquettes de l'axe x
  labs(title = "Distribution de target")  # Ajout d'un titre


histogram1 <- ggplot(Train, aes(x = superficief)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de la superficie")

histogram2 <- ggplot(Train, aes(x = ft_22_categ)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de ft_22_categ")

histogram3 <- ggplot(Train, aes(x = fuites)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de fuites")

histogram4 <- ggplot(Train, aes(x = chomage)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution du chomage")



histogram6 <- ggplot(Train, aes(x = ft_19_categ)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de ft_19_categ")

histogram7 <- ggplot(Train, aes(x = ft_4_categ)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de ft_4_categ")

histogram8 <- ggplot(Train, aes(x = criminalite)) +
  geom_histogram(bins = 30, fill = "blue", color = "black") +
  theme_minimal() +
  labs(title = "Distribution de la criminalite")

grid.arrange(histogram1, histogram2, histogram3, histogram4,ncol = 2) 
grid.arrange( histogram6, histogram7, histogram8,ncol = 2) 


### 2.2 ANALYSE BIVARIEE
### --------------------

ggplot(Train, aes(x = superficief, y = target)) +
  geom_point() +
  labs(title = "Relation entre la superficie et target")

ggplot(Train, aes(x = EXPO, y = target)) +
  geom_point() +
  labs(title = "Relation entre EXPO et target")

ggplot(Train, aes(x = ft_22_categ, y = target)) +
  geom_point() +
  labs(title = "Relation entre ft_22_categ et target")


# Calculer la matrice de corrélation
correlation_matrix <- cor(Train[, c("target", "EXPO", "fuites","ft_22_categ", "chomage", "pluie", "catnat")])

# Créer un graphique de corrélation
corrplot(correlation_matrix, method = "circle")

# visiualisation de cluster
clu = Train %>% select(target, EXPO , catnat , pluie, chomage, ft_22_categ, superficief, criminalite, pprn, rev_med, log_sociaux, entreprises)

# Effectuer la PCA
cluscale <- scale(clu)

pca_result2 <- PCA(cluscale, graph = TRUE)



# Utilisez la méthode du coude pour trouver le nombre optimal de clusters
wss <- numeric(10)  # Créez un vecteur pour stocker les valeurs de l'inertie

for (i in 1:25) {
  kmeans_result <- kmeans(cluscale, centers = i)
  wss[i] <- sum(kmeans_result$tot.withinss)
}

# Tracez un graphique de l'inertie en fonction du nombre de clusters
plot(1:25, wss, type = "b", xlab = "Nombre de clusters (K)", ylab = "Inertie")

#k=9
K <- 9
kmeans_result <- kmeans(cluscale, centers = K)


# Visualisation des clusters
fviz_cluster(kmeans_result, data = cluscale)




## #######################################################
## 3. Modélisation sans ajout de grosse données externes #
## #######################################################



### 3.1 Regression logistique
### -----------------------------------

Train_tout <- subset(Train, select = -c(Insee,Identifiant,X))
control <- trainControl(method="cv", number=10) # 5 plis de validation croisée 
model <- train(target~., data=Train_tout, method="glm", family="binomial", trControl = control)
summary(model)
# On refait un modèle avec nos variables les plus significatives, ainsi de suite et on arrive à:
model1 <- train(target~EXPO+ft_19_categ+superficief+ft_21_categ+criminalite+rev_med+ft_7_categ.2, data=Train_tout, method="glm", family="binomial", trControl = control)
summary(model1)
#0,4245 sur le site
predictions <- predict(model1, X_test)
X_test$target <- predictions
Y_testrf <- X_test[, c("X","Identifiant", "target")]
write.csv(Y_testrf, file = "nouveau.csv", row.names = FALSE)

### 3.2 XGBOOST
### -----------


#On applique XGBOOST à tout notre dataframe dans un premier temps, puis je séléctionne les variables qui ont le plus d'importance et on arrive à:
XGTRAIN = Train[,c("EXPO","superficief","ft_19_categ","ft_21_categ","criminalite","rev_med","ft_7_categ.2","target")]
XGY= XGTRAIN$target
XGTRAIN = subset(XGTRAIN,select=-target)
XGTEST= X_test[,c("EXPO","superficief","ft_19_categ","ft_21_categ","criminalite","rev_med","ft_7_categ.2")]
xgb_trcontrol = trainControl(method = "cv", number = 5, allowParallel = TRUE, verboseIter = FALSE, returnData = FALSE)             
xgbGrid <- expand.grid(nrounds = 100,  
                       max_depth = 3,
                       colsample_bytree = 0.9,
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1)
xgb_model = train(XGTRAIN, XGY, trControl = xgb_trcontrol, tuneGrid = xgbGrid, 
                  method = "xgbTree")

imp= xgb_model$finalModel
imp= xgb.importance(model=imp)
print(imp)
xgb.plot.importance(imp)

XGPRED = predict(xgb_model, XGTEST)
X_test$target <- XGPRED
rendu <- X_test[, c("X","Identifiant", "target")]
write.csv(rendu, file = "rendu1.csv", row.names = FALSE)
#0.4055


## #######################################################
## 4. Modélisation avec ajout de grosse données externes #
## #######################################################


### 4.1 Ajout du fichier externe
### ----------------------------


names(externe)[1] <- "Insee"
Train = merge(Train, externe, by = "Insee")
Test = merge(X_test,externe, by="Insee", all.x = TRUE)
Train = merge(Train,Y_train[,c("Identifiant","target")], by="Identifiant")
colSums(is.na(Train))
names(Train) <- gsub(" ", "_", names(Train))
names(Test) <- gsub(" ", "_", names(Test))
Train = subset(Train,select=-target.x)
names(Train)
names(Test)
colSums(is.na(Test))
colSums(is.na(Train))
getMode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
df <- lapply(Test, function(x) {
  if(is.numeric(x)) {
    # Pour les variables numériques, remplace NA par la moyenne
    x[is.na(x)] <- mean(x, na.rm = TRUE)
  } else if(is.character(x)) {
    # Pour les variables de type caractère, remplace NA par le mode
    modeValue <- getMode(x[!is.na(x)]) # Calcule le mode des valeurs non-NA
    x[is.na(x)] <- modeValue
  }
  return(x)
})

Test <- as.data.frame(df)
colSums(is.na(Test))
colnames(Train)[colnames(Train)=="Evolution_Pop_%"] <- "Evolution_Pop_."



### 4.2 Regression logistique
### -------------------------

vars_categ <- sapply(Train, function(x) is.factor(x) | is.character(x))
TrainSansCateg <- Train %>% select(which(!vars_categ))
TrainSansCateg <- subset(TrainSansCateg, select = -c(Identifiant,X))

#TrainSansCateg$superficief = log10(TrainSansCateg$superficief)

control <- trainControl(method="cv", number=10) # 5 plis de validation croisée 
model2 <- train(target.y~., data=TrainSansCateg, method="glm", family="binomial", trControl = control)
summary(model2)



# On sélectionne les variables les plus significatives encore une fois, ainsi de suite et on arrive à:
model3 <- train(target.y~superficief+EXPO+criminalite+ft_21_categ+Nb_Atifs+Fidélité+Moyenne_Revenus_Fiscaux_Départementaux+Moyenne_Revenus_Fiscaux_Régionaux+Dep_Moyenne_Salaires_Employé_Horaires+Reg_Moyenne_Salaires_Horaires+Reg_Moyenne_Salaires_Prof_Intermédiaire_Horaires+Reg_Moyenne_Salaires_Employé_Horaires+Dynamique_Entrepreneuriale_Service_et_Commerce+Valeur_ajoutée_régionale+ PIB_Régionnal+Score_PIB+Dynamique_Entrepreneuriale+ft_19_categ+ft_7_categ.2+Nb_de_Commerce, data=Train, method="glm", family="binomial", trControl = control)
summary(model3)

predictions <- predict(model3, Test)
Test$target <- predictions
Y_testrf <- Test[, c("X","Identifiant", "target")]
write.csv(Y_testrf, file = "nouveau.csv", row.names = FALSE)







