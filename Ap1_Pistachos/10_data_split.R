### División del dataset ###

## Librerías
library(tidyverse) # Manipulación de datos
library(readxl)    # Importar archivo .xlsx

## Importación del dataset
pist <- read_xlsx("./Pistachio_28_Features_Dataset.xlsx")

## Creación de columna id
pist$id <- 1:nrow(pist)

## Seteo de semilla para obtener resultados reproducibles
set.seed(7777777)

## División de datos en train y test para árbol de decisión (misma división que en Tree.R)
samp.80.20 <- sample(1 : nrow(pist), size = round(0.8 * nrow(pist)))

## División de datos en test y val para redes
pist.test.20  <- pist[-samp.80.20, ] 
samp.10.10 <- sort(sample(1 : nrow(pist.test.20), size = round(0.5 * nrow(pist.test.20))))

pist.test <- pist.test.20[samp.10.10, ]
pist.val  <- pist.test.20[-samp.10.10, ]

pist_id_test <- pist.test %>%
  select(id)

pist_id_val <- pist.val %>% 
  select(id)

## Archivo con los id a usar en python para separar las imágenes en train, test y val
write.table(cbind(pist_id_test, pist_id_val), file = "pist_id.txt", sep = "/t",
            row.names = FALSE, col.names = c("test", "val"))