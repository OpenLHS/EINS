######################################################
################## 29 mai 2025 EINS ##################
######################################################

# Chargement des librairies nécessaires
library(dplyr)
library(randomForest)
library(nnet)
library(caret)
library(rpart)
library(rpart.plot)
library(haven)
library(maptree)
library(NeuralNetTools)
??maptree


################## PARTIE 1: MANIPULATION ################## 
# Chemin du dossier
setwd('C:\\Votre\\Répertoire\\Des\\Fichiers')

# Exploration de quelques BD
admis <- read_sas('admis_assur_medic_p1.sas7bdat')
head(as.data.frame(admis))
sej_diag <- read_sas('sej_diag_p1.sas7bdat')
head(as.data.frame(sej_diag))
sej_interv <- read_sas('sej_interv_p1.sas7bdat')
head(as.data.frame(sej_interv))
pharma <- read_sas('serv_pharma1996_2016p1.sas7bdat')
head(as.data.frame(pharma))

# Lecture de la BD finale
# Ne pas faire: data <- read.csv('Torsade_fictives_EINS.csv')
# data est un mot réservé
torsade <- read.csv('Torsade_fictives_EINS.csv')

# A-t-on bien ouvert et lu la BD?
# Différentes caractéristiques
summary(torsade)
str(torsade)
dim(torsade)

#  À quoi ressemblent les premières lignes?
head(torsade)

# Combien de personnes sont décédées?
sum(!is.na(torsade$date_deces_srap))

# Quelle est la moyenne d'âge?
mean(torsade$age_interv_escc)

# Création de nouvelles variables
torsade$DC <-ifelse(!is.na(torsade$date_deces_srap), 1, 0)
summary(torsade$DC)

# Grande utilisation des services d'urgence
torsade$GU <-ifelse(torsade$NB_UR_POST_1 > 4, 1, 0)
summary(torsade$GU)
# Transformation de numérique à facteur pour la classification
torsade$GU <- as.factor(torsade$GU)

# Y a-t-il une différence dans le summary maintenant en facteur?
summary(torsade$GU)

# Transformation en facteur ou en code numérique du sexe
torsade$sexe_srap <- as.factor(torsade$sexe_srap)
torsade$sexe_srap <- as.numeric(torsade$sexe_srap)

# Deux façons d'examiner des variables spécifiques
# Façon dinosaure « old school »
summary(torsade$sexe_srap)
summary(torsade$age_interv_escc)
summary(torsade$CombCI_CH_CIM10_5ans)
summary(torsade$NB_OMNI_PRE_1)
summary(torsade$NB_SPEC_PRE_1)
summary(torsade$GENDHDI)

# Façon tidyverse new age
torsade %>% select(sexe_srap, age_interv_escc, CombCI_CH_CIM10_5ans, NB_OMNI_PRE_1,
                   NB_SPEC_PRE_1, GENDHDI) %>% summary

# Une régression logistique
mod <- glm(DC ~ sexe_srap + age_interv_escc + CombCI_CH_CIM10_5ans + NB_OMNI_PRE_1 +
             NB_SPEC_PRE_1 + GENDHDI, family = 'binomial', data = torsade)

# Résultats des OR avec IC95
summary(mod)
exp(cbind(OR = coef(mod), confint(mod)))




################## PARTIE 2: MACHINE LEARNING ################## 

# Division en sets d'entraînement et de test
# Seed pour la reproductibilité
set.seed(29052025)
train <- torsade %>% dplyr::sample_frac(.75)
set.seed(29052025)
test  <- dplyr::anti_join(torsade, train, by = 'noindiv_srap')

# Fonction pour calculer le score de Brier
brier <- function(x, y) mean((x - y)^2)

# Un arbre de décision
dt <- rpart(GU ~ sexe_srap + age_interv_escc + CombCI_CH_CIM10_5ans + NB_OMNI_PRE_1 +
              NB_SPEC_PRE_1 + GENDHDI + HUPDPAD, data = train)
rpart.plot(dt)

# Beaucoup d'arbre de décision (random forest) + mesure du temps
start.time <- Sys.time()
torsade_rf <- randomForest(GU ~ sexe_srap + age_interv_escc + CombCI_CH_CIM10_5ans +
                          NB_OMNI_PRE_1 + NB_SPEC_PRE_1 + GENDHDI + HUPDPAD,
                          data = train, importance = TRUE, ntree = 500)
end.time <- Sys.time()
end.time - start.time

# Importance des variables
varImp(torsade_rf)


# Réseau de neurones
torsade_nn <- nnet(GU ~ sexe_srap + age_interv_escc + CombCI_CH_CIM10_5ans + NB_OMNI_PRE_1 +
              NB_SPEC_PRE_1 + GENDHDI + HUPDPAD,
              data = train, size=6, linout=FALSE)

summary(torsade_nn)
plotnet(torsade_nn)

# Importance des variables
varImp(torsade_nn)

# D'autres façon de mesurer l'importance (propres aux réseaux de neurones)
# Plus la mesure est élevée, plus l'importance est grande
garson(torsade_nn)
olden(torsade_nn)

# Evaluer les prédictions du réseau de neurones avec la matrice de confusion et Brier
predictions <- predict(torsade_nn, test[, -ncol(test)], type="class")
confusion_matrix <- table(predictions, test$GU)
print(confusion_matrix)

brier(as.numeric(as.character(predictions)), as.numeric(as.character(test$GU)))
