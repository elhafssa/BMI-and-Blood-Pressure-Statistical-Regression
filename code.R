
set.seed(300)

# ============================================================
# 1. Chargement des packages
# ============================================================

# À exécuter une seule fois si les packages ne sont pas installés :
# install.packages("lmtest")
# install.packages("leaps")
# install.packages("glmnet")

library(lmtest)
library(leaps)
library(glmnet)

# ============================================================
# 2. Chargement du dataset
# ============================================================

load("BMI_Dataset.RData")

objets <- ls()

dataframes <- objets[
  sapply(objets, function(x) is.data.frame(get(x)))
]

datafile <- get(dataframes[1])

head(datafile)
dim(datafile)
names(datafile)
str(datafile)
summary(datafile)

# ============================================================
# 3. Renommer les variables
# ============================================================

# BMI   : indice de masse corporelle
# BP_Sy : pression systolique
# BP_Di : pression diastolique

datafile$bmi <- as.numeric(datafile$BMI)

datafile$pression_systolique <-
  as.numeric(as.character(datafile$BP_Sy))

datafile$pression_diastolique <-
  as.numeric(as.character(datafile$BP_Di))

datafile <- datafile[, c(
  "bmi",
  "pression_systolique",
  "pression_diastolique"
)]

head(datafile)
str(datafile)
summary(datafile)

# ============================================================
# 4. Nettoyage des données
# ============================================================

# Valeurs manquantes
sum(is.na(datafile))
colSums(is.na(datafile))

# Suppression des lignes avec valeurs manquantes
datafile <- na.omit(datafile)

# Doublons
sum(duplicated(datafile))

# Suppression des doublons
datafile <- datafile[!duplicated(datafile), ]

# Vérification finale
sum(is.na(datafile))
sum(duplicated(datafile))
dim(datafile)

# ============================================================
# 5. Création d'une variable qualitative pour l'ANOVA
# ============================================================

datafile$categorie_pression <- ifelse(
  datafile$pression_systolique < 120,
  "Normale",
  ifelse(
    datafile$pression_systolique < 140,
    "Elevee",
    "Hypertension"
  )
)

datafile$categorie_pression <-
  as.factor(datafile$categorie_pression)

table(datafile$categorie_pression)

# ============================================================
# 6. Analyse descriptive
# ============================================================

summary(datafile)

# Statistiques descriptives du BMI
mean(datafile$bmi)
median(datafile$bmi)
sd(datafile$bmi)
min(datafile$bmi)
max(datafile$bmi)

# Statistiques de la pression systolique
mean(datafile$pression_systolique)
sd(datafile$pression_systolique)

# Statistiques de la pression diastolique
mean(datafile$pression_diastolique)
sd(datafile$pression_diastolique)

# Histogramme du BMI
hist(
  datafile$bmi,
  main = "Distribution du BMI",
  xlab = "BMI"
)

# Histogramme de la pression systolique
hist(
  datafile$pression_systolique,
  main = "Distribution de la pression systolique",
  xlab = "Pression systolique"
)

# Histogramme de la pression diastolique
hist(
  datafile$pression_diastolique,
  main = "Distribution de la pression diastolique",
  xlab = "Pression diastolique"
)

# Boxplot du BMI selon la catégorie de pression
boxplot(
  bmi ~ categorie_pression,
  data = datafile,
  main = "BMI selon la catégorie de pression systolique",
  xlab = "Catégorie de pression",
  ylab = "BMI"
)

# ============================================================
# 7. Corrélations
# ============================================================

cor(
  datafile$bmi,
  datafile$pression_systolique
)

cor(
  datafile$bmi,
  datafile$pression_diastolique
)

cor(datafile[, c(
  "bmi",
  "pression_systolique",
  "pression_diastolique"
)])

# Relation BMI / pression systolique
plot(
  datafile$pression_systolique,
  datafile$bmi,
  main = "BMI en fonction de la pression systolique",
  xlab = "Pression systolique",
  ylab = "BMI",
  pch = 16
)

# Relation BMI / pression diastolique
plot(
  datafile$pression_diastolique,
  datafile$bmi,
  main = "BMI en fonction de la pression diastolique",
  xlab = "Pression diastolique",
  ylab = "BMI",
  pch = 16
)

# ============================================================
# 8. Régression linéaire simple
# ============================================================

model_simple <- lm(
  bmi ~ pression_systolique,
  data = datafile
)

summary(model_simple)

plot(
  datafile$pression_systolique,
  datafile$bmi,
  main = "Régression simple : BMI ~ pression systolique",
  xlab = "Pression systolique",
  ylab = "BMI",
  pch = 16
)

abline(model_simple, col = "red", lwd = 2)

# ============================================================
# 9. Régression linéaire multiple
# ============================================================

model_full <- lm(
  bmi ~ pression_systolique + pression_diastolique,
  data = datafile
)

summary(model_full)

# ============================================================
# 10. Comparaison des modèles
# ============================================================

model_sy <- lm(
  bmi ~ pression_systolique,
  data = datafile
)

model_di <- lm(
  bmi ~ pression_diastolique,
  data = datafile
)

model_sy_di <- lm(
  bmi ~ pression_systolique + pression_diastolique,
  data = datafile
)

comparaison <- data.frame(
  Modele = c(
    "Pression systolique",
    "Pression diastolique",
    "Pression systolique + diastolique"
  ),
  R2 = c(
    summary(model_sy)$r.squared,
    summary(model_di)$r.squared,
    summary(model_sy_di)$r.squared
  ),
  R2_ajuste = c(
    summary(model_sy)$adj.r.squared,
    summary(model_di)$adj.r.squared,
    summary(model_sy_di)$adj.r.squared
  ),
  AIC = c(
    AIC(model_sy),
    AIC(model_di),
    AIC(model_sy_di)
  ),
  BIC = c(
    BIC(model_sy),
    BIC(model_di),
    BIC(model_sy_di)
  )
)

print(comparaison)

# ============================================================
# 11. Sélection stepwise
# ============================================================

model_step <- step(
  model_full,
  direction = "both"
)

summary(model_step)

model_final <- model_step

# ============================================================
# 12. Sélection Lasso
# ============================================================

X <- model.matrix(
  bmi ~ pression_systolique + pression_diastolique,
  data = datafile
)[, -1]

Y <- datafile$bmi

lasso_model <- glmnet(
  X,
  Y,
  alpha = 1
)

plot(
  lasso_model,
  main = "Évolution des coefficients Lasso"
)

cv_lasso <- cv.glmnet(
  X,
  Y,
  alpha = 1
)

plot(
  cv_lasso,
  main = "Validation croisée du Lasso"
)

best_lambda <- cv_lasso$lambda.min
print(best_lambda)

coef(cv_lasso, s = "lambda.min")

# ============================================================
# 13. Cp de Mallows
# ============================================================

reg_subset <- regsubsets(
  bmi ~ pression_systolique + pression_diastolique,
  data = datafile,
  nvmax = 2
)

res_subset <- summary(reg_subset)

res_subset$cp
res_subset$adjr2
res_subset$outmat

plot(
  res_subset$cp,
  type = "b",
  main = "Cp de Mallows",
  xlab = "Nombre de variables",
  ylab = "Cp"
)

abline(0, 1, col = "red", lty = 2)

# ============================================================
# 14. Analyse de la variance (ANOVA)
# ============================================================

boxplot(
  bmi ~ categorie_pression,
  data = datafile,
  main = "BMI selon la catégorie de pression systolique",
  xlab = "Catégorie de pression",
  ylab = "BMI"
)

anova_pression <- aov(
  bmi ~ categorie_pression,
  data = datafile
)

summary(anova_pression)

TukeyHSD(anova_pression)

# ============================================================
# 15. Diagnostic du modèle final
# ============================================================

par(mfrow = c(2, 2))
plot(model_final)
par(mfrow = c(1, 1))

# Analyse des résidus
residus <- residuals(model_final)

hist(
  residus,
  main = "Distribution des résidus",
  xlab = "Résidus"
)

# Test de normalité de Shapiro-Wilk
set.seed(300)

residus_sample <- sample(
  residus,
  size = min(5000, length(residus))
)

shapiro.test(residus_sample)

# Test d'homoscédasticité de Breusch-Pagan
bptest(model_final)

# Test d'indépendance de Durbin-Watson
dwtest(model_final)

# ============================================================
# 16. Points influents
# ============================================================

hat_vals <- hatvalues(model_final)

seuil_hat <- 2 * length(coef(model_final)) /
  nrow(datafile)

which(hat_vals > seuil_hat)

# Distance de Cook
cook <- cooks.distance(model_final)

plot(
  cook,
  type = "h",
  main = "Distance de Cook",
  xlab = "Observation",
  ylab = "Distance de Cook"
)

abline(
  h = 4 / nrow(datafile),
  col = "red",
  lty = 2
)

which(cook > 4 / nrow(datafile))

# Résidus studentisés
rstud <- rstudent(model_final)

which(abs(rstud) > 2)

# ============================================================
# 17. Prédiction pour une nouvelle observation
# ============================================================

nouveau_patient <- data.frame(
  pression_systolique = 130,
  pression_diastolique = 85
)

prediction <- predict(
  model_final,
  newdata = nouveau_patient,
  interval = "prediction",
  level = 0.95
)

print(prediction)

bmi_predit <- prediction[1, "fit"]

print(bmi_predit)

# ============================================================
# 18. Catégorie du BMI prédit
# ============================================================

categorie_predite <- ifelse(
  bmi_predit < 18.5,
  "Insuffisance",
  ifelse(
    bmi_predit < 25,
    "Normal",
    ifelse(
      bmi_predit < 30,
      "Surpoids",
      "Obesite"
    )
  )
)

print(categorie_predite)

cat(
  "BMI prédit :",
  round(bmi_predit, 2),
  "\n"
)

cat(
  "Catégorie prédite :",
  categorie_predite,
  "\n"
)
