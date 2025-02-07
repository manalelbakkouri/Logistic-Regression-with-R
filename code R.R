
#partie 1: Descriptive et exploration des donnees

#Importation du dataset 'predictive.csv' et l'affichage des lignes ainsi que les statistiques des donnees
data=read.csv('predictive.csv')
head(data)
summary(data)

#suppression des variables non pertinentes 
library(dplyr)
data <- data %>% select(-c(`Product.ID`, `UDI` , `Failure.Type` , `Type` ))
head(data)

#verification des valeurs manquantes
any(is.na(data))
sum(is.na(data))

#verification des valeurs nulles
any(is.null(data))
sum(is.null(data))

#verification des valeurs dupliquees
duplicated_rows <- duplicated(data)
print(duplicated_rows)

# les valeurs aberrantes 
# Liste des variables à vérifier
variables_to_check <- c("Air.temperature..K.", "Process.temperature..K.", 
                        "Rotational.speed..rpm.", "Torque..Nm.", 
                        "Tool.wear..min.")

# Fonction pour détecter les valeurs aberrantes
detect_outliers <- function(df, column_name) {
  # Obtenir les valeurs aberrantes
  outliers <- boxplot(df[[column_name]], plot = FALSE)$out
  return(outliers)
}

# Afficher les valeurs aberrantes pour chaque variable
for (var in variables_to_check) {
  outliers <- detect_outliers(data, var)
  cat("Outliers pour", var, ":\n")
  print(outliers)
  cat("\n\n")
}
#traitement de ces valeurs 
# Fonction pour remplacer les outliers par la moyenne
replace_outliers_with_mean <- function(df, column_name) {
  # Récupérer les outliers
  outliers <- boxplot(df[[column_name]], plot = FALSE)$out
  
  # Calculer la moyenne de la colonne
  mean_value <- mean(df[[column_name]], na.rm = TRUE)
  
  # Remplacer les outliers par la moyenne
  df[[column_name]][df[[column_name]] %in% outliers] <- mean_value
  return(df)
}

# Appliquer cette fonction à chaque variable
for (var in variables_to_check) {
  data <- replace_outliers_with_mean(data, var)
}

#verification du nombre de lignes et colonnes
nombre_de_colonnes <- ncol(data)
nombre_de_lignes <- nrow(data)
print(paste("Le dataset contient", nombre_de_lignes, "lignes et", nombre_de_colonnes, "colonnes."))

# Matrice de corrélation entre les differents variables
cor(data)

#l'ensemble de donnees apres le traitement 
head(data)

#partie 2 : visualisation

#Histogramme pour visualiser la distribution de la variable cible
library(ggplot2)
ggplot(data, aes(x = Target)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution de la variable cible")

#Histogramme pour visualiser la distribution de la temperature de l'air en kelvin
library(ggplot2)
ggplot(data, aes(x = Air.temperature..K.)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Distribution de la temperature de l'air en kelvin")

#pairplot pour afficher la correlation de chaque variable aves les autres
install.packages("GGally")
library(GGally)
ggpairs(data)

#visualiser la relaion entre RPM et Target
library(ggplot2)
ggplot(data, aes(x = Rotational.speed..rpm., y = Target)) +
  geom_point(color = "red") +
  labs(title = "Relation entre la vitesse de rotation(RPM) et Target", x = "Rotational.speed..rpm.", y = "Target")


# partie 3 : Modelisation de la regression logistique

# la foncion glm : Régression logistique
model <- glm(Target ~ Process.temperature..K. + Air.temperature..K. + Rotational.speed..rpm. + Torque..Nm., Tool.wear..min.,
             data = data, family = binomial)

summary(model)


####
gradient_descent <- function(X, y, learning_rate = 0.01, iterations = 1000) {
  m <- nrow(X)  # Nombre d'observations
  n <- ncol(X)  # Nombre de variables
  theta <- rep(0, n)  # Initialisation des coefficients
  cost_history <- numeric(iterations)  # Historique des coûts (pour suivi)
  
  for (i in 1:iterations) {
    z <- X %*% theta  # Calcul de z = X * theta
    h <- 1 / (1 + exp(-z))  # Fonction sigmoïde (h = hypothèse)
    
    # Calcul de la fonction de coût (log-vraisemblance négative)
    cost <- -sum(y * log(h) + (1 - y) * log(1 - h)) / m
    cost_history[i] <- cost  # Sauvegarder le coût pour suivi
    
    # Calcul du gradient
    gradient <- t(X) %*% (h - y) / m
    
    # Mise à jour des paramètres
    theta <- theta - learning_rate * gradient
  }
  
  return(list(theta = theta, cost_history = cost_history))  # Retourner les coefficients et l'historique des coûts
}

# Préparation des données
X <- as.matrix(cbind(1, data[, c("Air.temperature..K.", "Process.temperature..K.", 
                                 "Rotational.speed..rpm.", "Torque..Nm.", "Tool.wear..min.")]))
y <- as.numeric(data$Target)

# Estimation des coefficients
result <- gradient_descent(X, y, learning_rate = 0.01, iterations = 1000)

# Extraction des résultats
theta <- result$theta
cost_history <- result$cost_history

# Afficher les coefficients estimés
print("Coefficients estimés :")
print(theta)

# Afficher l'évolution du coût
plot(cost_history, type = "l", col = "blue", lwd = 2, 
     xlab = "Itérations", ylab = "Coût", 
     main = "Évolution de la fonction de coût")


#####
# Résumé du modèle
summary(model)
#######

#### Test de normalite 

#  le package 'nortest' pour  tests
install.packages("nortest")
library(nortest)

# Test de Anderson-Darling
ad.test(residuals_std)

# Test de Lilliefors
lillie.test(residuals_std)
#visualisation
# Q-Q Plot pour évaluer la normalité
windows()
qqnorm(residuals_std)
qqline(residuals_std, col = "red")

# test de non-autocorrelation 
library(lmtest)
bgtest(model)


#Test d'homoscédasticité des résidus

# Test de Breusch-Pagan
bptest(model)

#fin
