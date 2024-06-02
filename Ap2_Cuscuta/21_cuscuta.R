## Librerías
library(readxl)
library(tidyverse)
library(DescTools)

## Resultados de REDES sin considerar la categoría CLUSTER
redes_sin_cluster <- read_excel("./Pruebas.xlsx", sheet = "Redes") %>% 
  mutate(
    TP = Cuscuta_bien, 
    TN = Class_E-(Contaminación-Cuscuta_bien), 
    FP = Class_C-Cuscuta_bien, 
    FN = Contaminación-Cuscuta_bien,
    ACC = (TP+TN)/(TP+FP+TN+FN),
    SEN = TP/(TP+FN),
    SPC = TN/(TN+FP),
    PPV = TP/(TP+FP),
    NPV = TN/(TN+FN),
    F1  = 2*PPV*SEN/(PPV+SEN)
  )

## Resultados de REDES considerando la categoría CLUSTER
redes_con_cluster <- read_excel("./Pruebas.xlsx", sheet = "Redes") %>% 
  mutate(
    TP = Cuscuta_bien+Cuscuta_en_Cluster, 
    TN = Class_E-(Contaminación-(Cuscuta_bien+Cuscuta_en_Cluster)), 
    FP = Class_C+Class_O-(Cuscuta_bien+Cuscuta_en_Cluster), 
    FN = Contaminación-(Cuscuta_bien+Cuscuta_en_Cluster),
    ACC = (TP+TN)/(TP+FP+TN+FN),
    SEN = TP/(TP+FN),
    SPC = TN/(TN+FP),
    PPV = TP/(TP+FP),
    NPV = TN/(TN+FN),
    F1  = 2*PPV*SEN/(PPV+SEN)
  )

## Resultados de ANALITICO sin considerar la categoría OTROS
analitico_sin_cluster <- read_excel("./Pruebas.xlsx", sheet = "Analítico") %>% 
  mutate(
    Cuscuta_bien = as.numeric(Cuscuta_bien),
    Cuscuta_en_Otros = as.numeric(Cuscuta_en_Otros),
    TP = Cuscuta_bien, 
    TN = Class_E-(Contaminación-Cuscuta_bien), 
    FP = Class_C-Cuscuta_bien, 
    FN = Contaminación-Cuscuta_bien,
    ACC = (TP+TN)/(TP+FP+TN+FN),
    SEN = TP/(TP+FN),
    SPC = TN/(TN+FP),
    PPV = TP/(TP+FP),
    NPV = TN/(TN+FN),
    F1  = 2*PPV*SEN/(PPV+SEN)
  )

## Resultados de ANALITICO considerando la categoría OTROS
analitico_con_cluster <- read_excel("./Pruebas.xlsx", sheet = "Analítico") %>% 
  mutate(
    Cuscuta_bien = as.numeric(Cuscuta_bien),
    Cuscuta_en_Otros = as.numeric(Cuscuta_en_Otros),
    TP = Cuscuta_bien+Cuscuta_en_Otros, 
    TN = Class_E-(Contaminación-(Cuscuta_bien+Cuscuta_en_Otros)), 
    FP = Class_C+Class_O-(Cuscuta_bien+Cuscuta_en_Otros), 
    FN = Contaminación-(Cuscuta_bien+Cuscuta_en_Otros),
    ACC = (TP+TN)/(TP+FP+TN+FN),
    SEN = TP/(TP+FN),
    SPC = TN/(TN+FP),
    PPV = TP/(TP+FP),
    NPV = TN/(TN+FN),
    F1  = 2*PPV*SEN/(PPV+SEN)
  )

## POR ESPECIE ##

## Metricas para REDES NEURONALES por especie (CLUSTER NO)
redes_sin_cluster_especie <- redes_sin_cluster %>%
  group_by(Especie) %>%
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(Especie, ACC, SEN, SPC, PPV, NPV, F1)

#redes_sin_cluster_especie

## Metricas para REDES NEURONALES por especie (CLUSTER SI)
redes_con_cluster_especie <- redes_con_cluster %>% 
  group_by(Especie) %>% 
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(Especie, ACC, SEN, SPC, PPV, NPV, F1)

#redes_con_cluster_especie

## Metricas para REDES NEURONALES por especie (CLUSTER NO)
analitico_sin_cluster_especie <- analitico_sin_cluster %>% 
  group_by(Especie) %>% 
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(Especie, ACC, SEN, SPC, PPV, NPV, F1)

#analitico_sin_cluster_especie

## Metricas para REDES NEURONALES por especie (CLUSTER SI)
analitico_con_cluster_especie <- analitico_con_cluster %>% 
  group_by(Especie) %>% 
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(Especie, ACC, SEN, SPC, PPV, NPV, F1)
  
#analitico_con_cluster_especie

cbind(
  "Especie" = redes_sin_cluster_especie[, 1],
  c("ANALITICO_SIN_C" = analitico_sin_cluster_especie[, 3]),
  c("REDES_SIN_C" = redes_sin_cluster_especie[, 3]),
  c("ANALITICO_CON_C" = analitico_con_cluster_especie[, 3]),
  c("REDES_CON_C" = redes_con_cluster_especie[, 3])
)


## GLOBAL ## 

## Metricas para REDES NEURONALES por especie (CLUSTER NO)
redes_sin_cluster_global <- redes_sin_cluster %>%
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(ACC, SEN, SPC, PPV, NPV, F1)

redes_sin_cluster_global

## Metricas para REDES NEURONALES por especie (CLUSTER SI)
redes_con_cluster_global <- redes_con_cluster %>% 
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(ACC, SEN, SPC, PPV, NPV, F1)

redes_con_cluster_global

## Metricas para REDES NEURONALES por especie (CLUSTER NO)
analitico_sin_cluster_global <- analitico_sin_cluster %>%
  drop_na(Cuscuta_bien) %>% 
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(ACC, SEN, SPC, PPV, NPV, F1)

## Metricas para REDES NEURONALES por especie (CLUSTER SI)
analitico_con_cluster_global <- analitico_con_cluster %>%
  drop_na(Cuscuta_bien) %>% 
  summarise(
    TP_ = sum(TP),
    TN_ = sum(TN),
    FP_ = sum(FP),
    FN_ = sum(FN),
    IC_ACC = round(BinomCI(TP_ + TN_, TP_ + TN_ + FP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SEN = round(BinomCI(TP_, TP_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_SPC = round(BinomCI(TN_, TN_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_PPV = round(BinomCI(TP_, TP_ + FP_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    IC_NPV = round(BinomCI(TN_, TN_ + FN_, conf.level = .95, method = "clopper-pearson"), 3)*100,
    ACC = paste0(IC_ACC[, 1], " [", IC_ACC[, 2], " ; ",IC_ACC[, 3], "]"),
    SEN = paste0(IC_SEN[, 1], " [", IC_SEN[, 2], " ; ",IC_SEN[, 3], "]"),
    SPC = paste0(IC_SPC[, 1], " [", IC_SPC[, 2], " ; ",IC_SPC[, 3], "]"),
    PPV = paste0(IC_PPV[, 1], " [", IC_PPV[, 2], " ; ",IC_PPV[, 3], "]"),
    NPV = paste0(IC_NPV[, 1], " [", IC_NPV[, 2], " ; ",IC_NPV[, 3], "]"),
    F1  = round(mean(F1, na.rm = T), 3)*100
  ) %>%
  select(ACC, SEN, SPC, PPV, NPV, F1)


cbind(
  c(
    "ANALITICO_SIN_C",
    "REDES_SIN_C",
    "ANALITICO_CON_C",
    "REDES_CON_C"),
  c(
    analitico_sin_cluster_global[, 2], 
    redes_sin_cluster_global[, 2], 
    analitico_con_cluster_global[, 2],
    redes_con_cluster_global[, 2])
)



## Descriptivo de las pruebas
analitico <- read_excel("./Pruebas.xlsx", sheet = "Analítico")
analitico$Método <- "Analítico" 
redes <- read_excel("./Pruebas.xlsx", sheet = "Redes")
redes$Método <- "Redes"

pruebas <- rbind(analitico[, c(2:6, 13:14)], redes[, c(2:6, 13:14)])

# Tiempo
pruebas %>% 
  group_by(Especie) %>% 
  summarise(
    Tiempo = mean(Segundos)/60
  )

pruebas %>% 
  group_by(Método, Especie) %>% 
  summarise(
    Clase_Especie = round(mean(Class_E), 0),
    Clase_Cuscuta = round(mean(Class_C), 0),
    Clase_Otros   = round(mean(Class_O), 0),
    Cuscuta_Otros = Clase_Cuscuta + Clase_Otros,
    Total = Clase_Especie + Clase_Cuscuta + Clase_Otros
  )