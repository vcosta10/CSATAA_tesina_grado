### Aplicación 1: Pistachos (Métricas) ###

### Lo hice "a mano" para poder poner los valores de redes

# Defino funciones para calcular cada métrica
accuracy <- function(TP, FN, FP, TN) {
  (TP + TN) / (TP + TN + FP + FN)
}

sensitivity <- function(TP, FN, FP, TN) {
  TP / (TP + FN)
}

specificity <- function(TP, FN, FP, TN) {
  TN / (TN + FP)
}

positive_predictive_value <- function(TP, FN, FP, TN) {
  TP / (TP + FP)
}

negative_predictive_value <- function(TP, FN, FP, TN) {
  TN / (TN + FN)
}

j_youden <- function(TP, FN, FP, TN) {
  sensitivity(TP, FN, FP, TN) + specificity(TP, FN, FP, TN) - 1
}

markedness <- function(TP, FN, FP, TN) {
  positive_predictive_value(TP, FN, FP, TN) + negative_predictive_value(TP, FN, FP, TN) - 1
}

f1_score <- function(TP, FN, FP, TN) {
  2 * TP / (2 * TP + FP + FN)
}

cohen_kappa <- function(TP, FN, FP, TN) {
  observed_agreement <- (TP + TN) / (TP + TN + FP + FN)
  chance_agreement <- ((TP + FP) * (TP + FN) + (TN + FP) * (TN + FN)) / (TP + TN + FP + FN)^2
  kappa <- (observed_agreement - chance_agreement) / (1 - chance_agreement)
  kappa
}

matthews_correlation <- function(TP, FN, FP, TN) {
  (TP * TN - FP * FN) / sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))
}


# Defino las matrices de confusión para cada modelo (Orden: TP, FN, FP, TN)
modelo1 <- c(211, 27, 36, 156)
modelo2 <- c(112, 2, 26, 75)
modelo3 <- c(111, 3, 13, 88)

# Calculo las métricas para cada modelo
metricas_modelo1 <- c(
  accuracy(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  sensitivity(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  specificity(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  positive_predictive_value(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  negative_predictive_value(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  j_youden(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  markedness(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  f1_score(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  cohen_kappa(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  matthews_correlation(modelo1[1], modelo1[2], modelo1[3], modelo1[4])
)

metricas_modelo2 <- c(
  accuracy(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  sensitivity(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  specificity(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  positive_predictive_value(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  negative_predictive_value(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  j_youden(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  markedness(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  f1_score(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  cohen_kappa(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  matthews_correlation(modelo2[1], modelo2[2], modelo2[3], modelo2[4])
)

metricas_modelo3 <- c(
  accuracy(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  sensitivity(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  specificity(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  positive_predictive_value(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  negative_predictive_value(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  j_youden(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  markedness(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  f1_score(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  cohen_kappa(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  matthews_correlation(modelo3[1], modelo3[2], modelo3[3], modelo3[4])
)

# Creo la tabla
knitr::kable(
  cbind(
    "Modelo" = c("AR", "R2", "R3"),
    "EXA" = round(c(metricas_modelo1[1], metricas_modelo2[1], metricas_modelo3[1]), 3) * 100,
    "SEN" = round(c(metricas_modelo1[2], metricas_modelo2[2], metricas_modelo3[2]), 3) * 100,
    "ESP" = round(c(metricas_modelo1[3], metricas_modelo2[3], metricas_modelo3[3]), 3) * 100,
    "VPP" = round(c(metricas_modelo1[4], metricas_modelo2[4], metricas_modelo3[4]), 3) * 100,
    "VPN" = round(c(metricas_modelo1[5], metricas_modelo2[5], metricas_modelo3[5]), 3) * 100,
    "J" = round(c(metricas_modelo1[6], metricas_modelo2[6], metricas_modelo3[6]), 3)* 100,
    "INT" = round(c(metricas_modelo1[7], metricas_modelo2[7], metricas_modelo3[7]), 3)* 100,
    "F1" = round(c(metricas_modelo1[8], metricas_modelo2[8], metricas_modelo3[8]), 3)* 100,
    "K" = round(c(metricas_modelo1[9], metricas_modelo2[9], metricas_modelo3[9]), 3)* 100,
    "MCC" = round(c(metricas_modelo1[10], metricas_modelo2[10], metricas_modelo3[10]), 3)* 100
  )
)

## Mismo análisis pero con IC para las primeras cinco métricas
library(DescTools)

### Lo hice "a mano" para poder poner los valores de redes

# Defino funciones para calcular cada métrica
accuracy <- function(TP, FN, FP, TN) {
  IC <- round(BinomCI(TP + TN, TP + TN + FP + FN, conf.level = .95, method = "clopper-pearson"), 3)*100
  paste0(IC[, 1], " [", IC[, 2], " ; ",IC[, 3], "]")
}

sensitivity <- function(TP, FN, FP, TN) {
  IC <- round(BinomCI(TP, TP + FN, conf.level = .95, method = "clopper-pearson"), 3)*100
  paste0(IC[, 1], " [", IC[, 2], " ; ",IC[, 3], "]")
}

specificity <- function(TP, FN, FP, TN) {
  IC <- round(BinomCI(TN, TN + FP, conf.level = .95, method = "clopper-pearson"), 3)*100
  paste0(IC[, 1], " [", IC[, 2], " ; ",IC[, 3], "]")
}

positive_predictive_value <- function(TP, FN, FP, TN) {
  IC <- round(BinomCI(TP, TP + FP, conf.level = .95, method = "clopper-pearson"), 3)*100
  paste0(IC[, 1], " [", IC[, 2], " ; ",IC[, 3], "]")
}

negative_predictive_value <- function(TP, FN, FP, TN) {
  IC <- round(BinomCI(TN, TN + FN, conf.level = .95, method = "clopper-pearson"), 3)*100
  paste0(IC[, 1], " [", IC[, 2], " ; ",IC[, 3], "]")
}


# Calculo las métricas para cada modelo
metricas_modelo1 <- c(
  accuracy(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  sensitivity(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  specificity(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  positive_predictive_value(modelo1[1], modelo1[2], modelo1[3], modelo1[4]),
  negative_predictive_value(modelo1[1], modelo1[2], modelo1[3], modelo1[4])
)

metricas_modelo2 <- c(
  accuracy(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  sensitivity(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  specificity(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  positive_predictive_value(modelo2[1], modelo2[2], modelo2[3], modelo2[4]),
  negative_predictive_value(modelo2[1], modelo2[2], modelo2[3], modelo2[4])
)

metricas_modelo3 <- c(
  accuracy(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  sensitivity(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  specificity(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  positive_predictive_value(modelo3[1], modelo3[2], modelo3[3], modelo3[4]),
  negative_predictive_value(modelo3[1], modelo3[2], modelo3[3], modelo3[4])
)

# Creo la tabla
knitr::kable(
  cbind(
    "Modelo" = c("AR", "R2", "R3"),
    "EXA" = c(metricas_modelo1[1], metricas_modelo2[1], metricas_modelo3[1]),
    "SEN" = c(metricas_modelo1[2], metricas_modelo2[2], metricas_modelo3[2]),
    "ESP" = c(metricas_modelo1[3], metricas_modelo2[3], metricas_modelo3[3]),
    "VPP" = c(metricas_modelo1[4], metricas_modelo2[4], metricas_modelo3[4]),
    "VPN" = c(metricas_modelo1[5], metricas_modelo2[5], metricas_modelo3[5])
  )
)

