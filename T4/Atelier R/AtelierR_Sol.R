###################################################################################################################################################

# PROJECT: Atelier d'introduction à R (Solutionnaire)
# DOC:     
# BY:      JPM, FCL
# DATE:    Mai 2025
# UPDATE:  --  

####################################################################################################################################################

# Cette commande retire les objets et les données qui sont présentement gardés en mémoire.
rm(list=ls())

#-------------------------------------------------------------------------------
# À nous de joueR! (0)
#-------------------------------------------------------------------------------

#####
# Fonctionnement de R en tant qu'outil de calcul
#####

# Opérations mathématiques
1 + 1
2 * 3
3^2
exp(1)
log(10) # correspond à ln, le logarithme naturel (ou logarithme en base e)
log(10, base = 10)

# Opérations booléennes
1 < 2
!(1 < 2)
(1 < 2) & FALSE
(1 < 2) | FALSE
1 == 2
1 != 2
"Hello World!" == "Hello World!"
"Hello World!" == "hello world!"

# Assignation
# Avec R, on privilégie l'utilisation de l'opérateur " <- " pour faire l'assignation à des variables ou à des objets.
x <- 34
x <- x + 1

mot_de_bienvenue <- "Hello World!"

fonction_bienvenue <- function(nombre){
  
  if(nombre<0){
    stop("Le nombre de points d'exclamation ne peut pas être négatif.")
  }
  
  point_exclamation <- rep("!", nombre)
  point_exclamation <- paste(point_exclamation, collapse = "")
  return(paste0("Hello World", point_exclamation, collapse = ""))
}
fonction_bienvenue(3)

# Vecteurs
# Avec R, on dispose d'un opérateur spécifiquement pour créer des vecteurs: "c".
c("Pêche", "Pomme", "Poire", "Abricot")
vecteur1 <- c(10, 20, 30)
vecteur2 <- c(2, 3, 4)

# On peut également faire des opérations arithmétiques sur ces vecteurs.
3*vecteur1
vecteur1*vecteur2

# La plupart des fonctions peuvent être appliquées à la fois sur un objet ou sur un vecteur.
sqrt(x = 4)
sqrt(x = c(4, 9, 16))

#####
# Aidez-moi!
#####

# Les commandes "?fonction" et "??fonction" effectuent des recherches parmi les fonctions R qui sont disponibles et
# affichent les résultats dans l'onglet d'aide de RStudio.

# La commande "?fonction" vous apporte directement à la page de cette fonction, si elle est disponible
?sqrt
?log
?glimpse

# La commande "??fonction" cherche plutôt dans l'ensemble des pages et retourne des résultats de recherches 
# qui pourraient être intéressants
??glimpse

#####
# Chargement de librairies essentielles
#####

# Au besoin, on installe les librairies
# Note: Comme il n'est pas nécessaire d'installer plusieurs fois une même librairie, je vous conseille de commenter
# ces lignes une fois l'installation faite.
install.packages("gtsummary")
install.packages("tidyverse")

# On charge les librairies
library(gtsummary) 
library(tidyverse)

#####
# Utilisation de R dans un contexte d'analyses ou d'exploration de données
#####

# Prérequis: la gestion des répertoires
# Pour voir le contenu du répertoire dans lequel on travaille présentement.
dir()

# Pour montrer le répertoire dans lequel se trouve ce code.
working_directory <- dirname(rstudioapi::getSourceEditorContext()$path)

# Pour changer le répertoire de travail.
setwd(working_directory)

# Chargement d'un fichier de données
# (Changer le nom du fichier manuellement, au besoin)
filename <- "Donnees_glucose.csv"

# On charge le jeu de données en mémoire.
df_glucose <- read.csv(filename)
df_glucose 

# Alternative pour charger les données en mémoire. Format un peu plus lisible.
df_glucose <- read_csv(filename) 
df_glucose

# Remarque: L'objectif de l'atelier n'est pas de partir d'un jeu de données brute,
# de bien le charger, de le nettoyer puis de s'en servir pour faire des analyses.
# Plutôt, on considère que l'on dispose déjà d'un jeu de données nettoyées et on 
# aimerait apprendre à utiliser R pour répondre à différentes questions, tout en
# utilisant ce jeu de données. 
# Ainsi, il vous sera utile de jeter un coup d'oeil un peu plus approfondi
# à la commande de chargement d'un CSV pour votre pratique.
?read.csv

#-------------------------------------------------------------------------------
# À vous de joueR! (1)
#-------------------------------------------------------------------------------

# Exploration des commandes en lien avec les jeux de données

# Que font les commandes suivantes?
head(df_glucose)                    # Affiche les premières lignes du jeu
tail(df_glucose)                    # Affiche les dernières lignes du jeu
glimpse(df_glucose)                 # Transposition de la fonction head
view(df_glucose)                    # Ouvre le jeu de données dans R
df_glucose[1,]                      # Accède la ligne 1 du jeu
df_glucose[c(1,3,5),]               # Accède les lignes 1, 3 et 5 du jeu
df_glucose[,1]                      # Accède la colonne 1 du jeu
df_glucose[2,2]                     # Accèsde la ligne 2 et la colonne 2 du jeu (donc une seule cellule!)
df_glucose[df_glucose$Predia==1,]   # Accède les lignes où "Predia==1"
df_glucose$Age                      # Accède la colonne nommée "Age"

#-------------------------------------------------------------------------------
# Fin (1)
#-------------------------------------------------------------------------------

# Manipulation de données
# Les principales actions et principaux verbes à notre disposition sont les suivants:

#------------------------------------------------
# Action                          | Verbe       |
#------------------------------------------------
# Sélectionner des observations   | filter()    |   
# Sélectionner des variables      | select()    |
# Ordonner les lignes             | arrange()   |
# Créer de nouvelles variables    | mutate()    |
# Créer des résultats sommaires   | summarise() |
# Créer des groupes               | group_by()  |
#------------------------------------------------

# Ordonnez le jeu de données en fonction de l'IMC des patientes
df_glucose <- arrange(df_glucose, IMC)
min(df_glucose$IMC)
df_glucose

# Créez une nouvelle variable qui correspond à la cote z des patientes pour ce qui est de leur IMC
df_glucose <- mutate(df_glucose, IMCCoteZ = (IMC - mean(IMC))/sd(IMC))
df_glucose

# Pour chaque catégorie de la variable traitement, calculez la moyenne de l’IMC des patientes.
df_glucose_sans_traitement <- filter(df_glucose, Traitement==0)
mean(df_glucose_sans_traitement$IMC)
df_glucose_avec_traitement <- filter(df_glucose, Traitement==1)
mean(df_glucose_avec_traitement$IMC)

# Exploration de la commande %>% 
set.seed(seed = 34)
y <- rnorm(n = 100, mean = 3, sd = 4)
x <- runif(n = 100, min = -5, max = 5)

plot(x, y)
x %>% plot(y)

# Première comparaison avec l'opérateur %>% 
df_glucose <- arrange(df_glucose, IMC)
# vs
df_glucose <- df_glucose %>%
  arrange(IMC)

# Deuxième comparaison avec l'opérateur %>% 
df_glucose <- arrange(df_glucose, IMC)
df_glucose_avec_predia <- filter(df_glucose, Predia==1)
# vs 
df_glucose %>% 
  arrange(IMC) %>% 
  filter(Predia==1)

# Troisième comparaison avec l'opérateur %>% 
df_glucose_sans_traitement <- filter(df_glucose, Traitement==0)
mean(df_glucose_sans_traitement$IMC)
df_glucose_avec_traitement <- filter(df_glucose, Traitement==1)
mean(df_glucose_avec_traitement$IMC)
# vs
df_glucose %>% 
  group_by(Traitement) %>% 
  summarise(mean(IMC))

# Comment pourriez-vous calculer l'IMC moyen des patientes?
mean(df_glucose$IMC)
# Comment pourriez-vous calculer l'IMC moyen des patientes ayant 28 ans?
# Au besoin, explorer la commande "filter".
mean(filter(df_glucose, Age==28)$IMC)

# Supposons que l'on souhaite calculer l'IMC moyen des patientes ayant
# entre 25 et 30 ans, et ce en comparant les patientes qui ont reçu
# le traitement à celles n'ayant pas reçu le traitement.
df_glucose %>% 
  filter(Age>=25 & Age<=30) %>% 
  group_by(Traitement) %>% 
  summarise(mean(IMC))

#-------------------------------------------------------------------------------
# À vous de joueR! (2)
#-------------------------------------------------------------------------------

# Créez une variable qui correspond à l'augmentation de la mesure de glucose entre T1 et T2.
df_glucose <- df_glucose %>% 
  mutate(Gluc_Diff = Gluc_T2 - Gluc_T1)

# Explorez la commande count
?count

# Parmi les patientes ayant plus de 30 ans, combien y a-t-il de patientes qui ont reçu le traitement? 
# Qui ne l'ont pas reçu?
df_glucose %>% 
  filter(Age>30) %>% 
  count(Traitement)

# Créez un jeu de données secondaire où les seules variables sont les mesures de glucose, 
# c'est-à-dire Gluc_T1 et Gluc_T2.
df_glucose2 <- df_glucose %>% 
  select(c(Gluc_T1, Gluc_T2))

# Explorez la commande case_when.
?case_when

# Créez une variable (AgeCat) pour les catégories d'âge suivantes.
# Moins de 25 ans
# Au moins 25 ans, moins de 32 ans
# Au moins 32 ans
df_glucose <- df_glucose %>% 
  mutate(AgeCat = case_when(Age< 25 ~ "<25",
                             Age< 32 ~ ">=25, <32",
                             Age>=32 ~ ">=32"))

# On aimerait connaître l'âge moyen des patientes qui ont reçu le traitement, selon
# si elles font partie d'un groupe à risque ou non.
df_glucose %>% 
  filter(Traitement==1) %>% 
  group_by(Groupe_a_risque) %>% 
  summarise(mean(Age))

# Pour les patientes diagnostiquées avec un prédiabète et qui sont âgées de plus de 30 ans,
# combien d'entre elles font partie d'un groupe à risque?
df_glucose %>% 
  filter(Predia==1 & Age>30) %>% 
  group_by(Groupe_a_risque) %>% 
  count()

#-------------------------------------------------------------------------------
# Fin (2)
#-------------------------------------------------------------------------------

# La grammaire des graphiques via la librairie ggplot2
# Les éléments essentiels d'un graphique et les fonctions associées sont:

#----------------------------------------------
# Élément essentiel             | Fonction    |
#----------------------------------------------
# Choix du jeu de données       | ggplot()    |   
# Choix de la/les variable(s)   | aes()       |
# Choix de représentation       | geom_...()  |
#----------------------------------------------

# Supposons que l'on souhaite créer un histogramme de l'âge des patientes
# Nous allons créer le graphique couche par couche

# Couche 1: Le choix du jeu de données
ggplot(df_glucose)

# Couche 2: Le choix des variables
ggplot(df_glucose, aes(x=Age))

# Couche 3: Le choix de représentation 
ggplot(df_glucose, aes(x=Age)) + 
  geom_histogram()

# On peut également spécifier certains paramètres pour la représentation géométrique.
# Par exemple, la largeur des bandes.
ggplot(df_glucose, aes(x=Age)) + 
  geom_histogram(binwidth = 1)

# Couche 4? On ajoute par-dessus notre histogramme une autre représentation géométrique.
ggplot(df_glucose, aes(x=Age)) + 
  geom_histogram(binwidth = 1) + 
  geom_freqpoly(binwidth = 1)

# Évidemment, l'ordre dans lequel on met les couches est important. C'est justement le principe des couches!
ggplot(df_glucose, aes(x=Age)) +  
  geom_freqpoly(binwidth = 1) + # On a choisi de mettre en premier le polygone de fréquences
  geom_histogram(binwidth = 1)  # puis de mettre l'histogramme. L'histogramme est donc par-dessus le polygone!

# Enfin, on peut également faire ce qu'on appelle des choix de facettes ou des choix d'étiquettes

# Supposons que l'on souhaite construire un histogramme de l’âge des patientes 
# ayant reçu le traitement et un autre pour celles ne l’ayant pas reçu.
ggplot(df_glucose, aes(x=Age)) + 
  geom_histogram(binwidth = 1) + 
  facet_wrap(~Traitement)

# On aimerait faire la même chose, mais en distinguant les patientes ayant reçu ou non le traitement
ggplot(df_glucose, aes(x=Age, colour=Traitement)) +
  geom_histogram() 

# Traitement est une variable numérique, mais l'argument colour devrait être catégorique. On crée donc une autre
# variable dans notre jeu de données
df_glucose <- df_glucose %>% 
  mutate(TraitementCat = case_when(Traitement==0~"Controle",
                                   Traitement==1~"Intervention")
)
ggplot(df_glucose, aes(x=Age, colour=TraitementCat)) +
  geom_histogram() 

# On peut ajouter plusieurs options pour personnaliser ces graphiques
ggplot(df_glucose, aes(x=Age, colour=TraitementCat)) +
  geom_histogram() +
  labs(x = "Axe des X", 
       y = "Axes des Y",
       title = "Mon titre",
       caption = "Figure 1: Mon graphique.") + 
  scale_colour_manual(values = c('#6ea1c5', '#ef8354')) 

# Voici un autre exemple de personnalisation
ggplot(df_glucose, aes(x=Age, fill=TraitementCat)) +
  geom_histogram() +
  labs(x = "Axe des X", 
       y = "Axes des Y",
       title = "Mon titre",
       caption = "Figure 1: Mon graphique.") + 
  scale_fill_manual(values = c('#6ea1c5', '#ef8354')) 

#-------------------------------------------------------------------------------
# À vous de joueR! (3)
#-------------------------------------------------------------------------------

# Créez un "box plot" de l'IMC des patientes qui permet de comparer le groupe ayant reçu le traitement à celui qui
# ne l'a pas reçu.
ggplot(df_glucose, aes(x = TraitementCat, y = IMC)) + 
  geom_boxplot()

# Explorer la commande coord_flip
?coord_flip

# Que fait cette commande?
# Elle interchange les axes d'un graphique!
ggplot(df_glucose, aes(x = TraitementCat, y = IMC)) + 
  geom_boxplot() + 
  coord_flip()

# Créez un diagramme à bâtons qui illustre la répartition de la variable Traitement
ggplot(df_glucose, aes(x = TraitementCat)) + 
  geom_bar()

# Refaites le même graphique, mais on ne considérant que les patientes ayant au moins 30 ans.
ggplot(df_glucose %>% 
         filter(Age>30), aes(x = TraitementCat)) + 
  geom_bar()

# Refaites le même graphique, mais on ne considérant que les patientes ayant au moins 30 ans et en distinguant
# les patientes faisant partie d'un groupe à risque.
ggplot(df_glucose %>% 
         filter(Age>30), aes(x = TraitementCat, fill = RisqueCat)) + 
  geom_bar()

ggplot(df_glucose %>% 
         filter(Age>30), aes(x = TraitementCat)) + 
  geom_bar() + 
  facet_wrap(~RisqueCat)

# Créez un nuage de points pour les deux mesures de glucose
ggplot(df_glucose, aes(x=Gluc_T1, y = Gluc_T2)) + 
  geom_point()

# Pour ce même nuage de points, on aimerait colorier les points afin de différencier les patientes faisant partie d'un
# groupe à risque et celles qui n'en font pas partie
df_glucose <- df_glucose %>% 
  mutate(RisqueCat = case_when(Groupe_a_risque==0 ~ "Ne fait pas partie d'un groupe à risque",
                               Groupe_a_risque==1 ~ "Fait partie d'un groupe à risque"))
ggplot(df_glucose, aes(x=Gluc_T1, y = Gluc_T2, colour = RisqueCat)) + 
  geom_point()

# Explorez la commande geom_smooth
?geom_smooth

# Toujours sur ce même nuage de points, on aimerait ajouter une fonction de tendance avec un intervalle de confiance
ggplot(df_glucose, aes(x=Gluc_T1, y = Gluc_T2, colour = RisqueCat)) + 
  geom_point() + 
  geom_smooth()

# On fait ici la même chose, mais en spécifiant que l'on souhaite utiliser un modèle de régression linéaire
ggplot(df_glucose, aes(x=Gluc_T1, y = Gluc_T2, colour = RisqueCat)) + 
  geom_point() + 
  geom_smooth(method="lm")

# Que se passe-t-il si l'on déplace l'argument concernant la couleur dans la fonction geom_point() ?
ggplot(df_glucose, aes(x=Gluc_T1, y = Gluc_T2)) + 
  geom_point(aes(colour = RisqueCat)) + 
  geom_smooth()

# On fait ici la même chose, mais en spécifiant que l'on souhaite utiliser un modèle de régression linéaire
# On a également ajouter les étiquettes pour les axes, le titre et personnalisé les couleurs des groupes
ggplot(df_glucose, aes(x=Gluc_T1, y = Gluc_T2)) + 
  geom_point(aes(colour = RisqueCat)) + 
  geom_smooth(method="lm") + 
  labs(x = "Mesure du glucose au temps T1", 
       y = "Mesure du glucose au temps T2",
       title = "Étude de la relation entre les deux mesures du glucose \n en fonction de l'appartenance à un groupe à risque.") + 
  scale_colour_manual(values = c('#6ea1c5', '#ef8354')) 

#-------------------------------------------------------------------------------
# Fin (3)
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# À vous de joueR! (4)
#-------------------------------------------------------------------------------

# On vous demande de comparer les statistiques descriptives du groupe "intervention"
# et du groupe "controle" pour les variables suivantes:
# Age
# Groupe_a_risque
# IMC
# Predia

# Certaines librairies permettent de générer des sommaires de façon rapide.
# La libraire "gtsummary", chargée précédemment, est l'une de celles-ci.

stat.desc <- df_glucose %>% 
  mutate(PrediaCat = case_when(Predia==0 ~ "Non",
                               Predia==1 ~ "Oui")) %>% 
  select(c("TraitementCat", "Age", "Groupe_a_risque", "IMC", "PrediaCat")) %>% 
  tbl_summary(by="TraitementCat", 
              type = all_continuous() ~ "continuous2",
              statistic = list(
                all_continuous() ~ c(
                  "{N_nonmiss}/{N_obs} ({p_nonmiss}%)", "{mean} ({sd})",
                  "{median} ({p25}, {p75})",
                  "{min}, {max}"
                ),
                all_categorical() ~ c(
                  "{n} / {N} ({p}%)")
              ),
              missing="no",digits = all_continuous() ~ 1) %>%
  add_p( test = list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test"))
stat.desc 

#-------------------------------------------------------------------------------
# Fin (4)
#-------------------------------------------------------------------------------

#####
# Comparaison de deux proportions: tentative de répondre à la question de recherche
#####

# On mentionne que l'on s'intéresse à la proportion de patientes ayant un changement dans la mesure
# du test oral inférieur à 0.3mmol/L
seuil = 0.3 

# On crée une nouvelle variable binaire où
# 0: Le changement dans la mesure du test oral n'EST PAS inférieur à 0.3mmol/L 
# 1: Le changement dans la mesure du test oral EST inférieur à 0.3mmol/L
df_glucose <- df_glucose %>% 
  mutate(Gluc_Diff = Gluc_T2 - Gluc_T1) %>% 
  mutate(Amelioration = case_when(Gluc_Diff<seuil~1,
                                  Gluc_Diff>=seuil~0))

# On effectue ici les transformations nécessaires afin de faire un test de comparaison de deux proportions
df_glucose %>% 
  group_by(TraitementCat) %>% 
  count(Amelioration) %>% 
  pivot_wider(names_from = Amelioration, values_from = n) %>% 
  column_to_rownames("TraitementCat") %>%
  select(c("1", "0")) %>% 
  as.matrix() %>% 
  prop.test()

# De façon équivalente, on peut présenter le tout sous forme de tableau
prop.tbl <- df_glucose %>% 
  select(c("Amelioration", "TraitementCat")) %>% 
  tbl_summary(by="TraitementCat", 
              type = all_continuous() ~ "continuous2",
              statistic = list(
                all_categorical() ~ c(
                  "{n} / {N} ({p}%)")
              ),
              missing="no",digits = all_continuous() ~ 1) %>%
  add_p( test = list(all_continuous() ~ "t.test", all_categorical() ~ "fisher.test"))
prop.tbl 

#####
# Utilisation de R pour l'ajustement de données en régression linéaire
#####

# Création d'un modèle de régression linéaire
reglin <- lm(Gluc_T2 ~ Gluc_T1 + AgeCat, data = df_glucose)

# Création de la table permettant d'analyser le modèle de régression que l'on vient de créer
tbl_regression(reglin)

# Voici une façon alternative de produire des résultats similaires, mais directement dans la console
summary(reglin)

#####
# Utilisation de R pour l'ajustement de données en régression logistique
#####

# Représentation du nuage de points et de la droite de régression linéaire pour une seule covariable.
# On voit bien que le nuage n'est pas linéaire et qu'on aura besoin d'un autre outil afin de répondre à la question.
ggplot(df_glucose, aes(x = IMC, y = Amelioration)) + 
  geom_point() + 
  geom_smooth(method = "lm") + 
  labs(x = "IMC", y = "Différence d'au plus 0.3mmol/L entre les deux mesures \n (Oui = 1, Non = 0)")

# Création d'un modèle de régression linéaire
reglogis <- glm(Amelioration ~ TraitementCat + Age + IMC + Groupe_a_risque + Predia , data = df_glucose, family = "binomial") 

# Création de la table permettant d'analyser le modèle de régression que l'on vient de créer
tbl_regression(reglogis)

# L'argument "exponentiate = TRUE" nous permet d'afficher les odds ratios plutôt que les log(odds ratios).
# Ce n'est pas nécessaire, mais ces valeurs s'interprètent beaucoup plus facilement.
tbl_regression(reglogis, exponentiate = TRUE)

# Voici une façon alternative de produire des résultats similaires, mais directement dans la console
summary(reglogis)
