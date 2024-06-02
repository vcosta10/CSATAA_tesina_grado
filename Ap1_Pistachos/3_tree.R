### Aplicación 1: Pistachos (Árbol de decisión) ###

## Librerías
library(tidyverse)  # Manipular datos
library(readxl)     # Importar archivo .xlsx
library(rpart)      # Aplicar árboles de decisión
library(rpart.plot) # Graficar árboles de decisión
library(caret)      # Usar función train()


## Importación del dataset
setwd("C:/Users/Victorio/Desktop/Viko/Academia/UNR - Licenciatura en Estadistica/Tesina/Ap 1 - Pistachos/Pistachio_Image_Dataset")

pist <- read_xlsx("Pistachio_28_Features_Dataset.xlsx") %>% 
  mutate(Class = case_when(
    Class == "Kirmizi_Pistachio" ~ "Kirmizi",
    Class == "Siirt_Pistachio" ~ "Siirt"
    )
)

## Seteo de semilla para obtener resultados reproducibles
set.seed(7777777)

## División de datos en train y test para árbol de decisión (misma división que en Tree.R)
samp.80.20 <- sample(1 : nrow(pist), size = round(0.8 * nrow(pist)))

## División de datos en test y val para redes
pist.train <- pist[samp.80.20, ]
pist.test  <- pist[-samp.80.20, ] 

dim(pist.train)
dim(pist.test)

## Ajuste y gráfica del árbol (modelo maximal)
pist.cart <- rpart(
  as.factor(Class) ~ .,     # Incluyo todas las variables que tengo
  control = rpart.control(
    cp = 0,                 # Parámetro de complejidad (en 0 es el modelo maximal)
    minbucket = 25          # Número mínimo de observaciones que puede quedar en cada nodo terminal
  ),
  data = pist.train
)

rpart.plot(pist.cart,
           extra = 100,
           digits = 3,
           tweak = 1.5,
           box.palette = "Gn",
           shadow.col = "darkgray")


## Búsqueda del valor óptimo para el parámetro de complejidad

# Opción 1: se imprimen los cp para ver cuál usar
printcp(pist.cart, digits = 3)
plotcp(pist.cart, upper = "splits")

# Opción 2: validación cruzada para encontrar el óptimo
model2 <- train(
  Class ~., data = pist.train, method = "rpart",
  trControl = trainControl("cv", number = 10),
  tuneLength = 10
)
plot(model2, xlim = c(0, 0.05))
model2$bestTune

## Se ajusta el modelo final
pist.cart.cp.best <- rpart(
  as.factor(Class) ~ ., 
  control = rpart.control(
    cp = model2$bestTune, 
    minbucket = 25
  ),  
  data = pist.train
)
rpart.plot(pist.cart.cp.best,
           extra = 106,
           digits = 3,
           tweak = 1.2,
           box.palette = "Gn",
           shadow.col = "darkgray")


# Evaluación del modelo
predichos.cart.class.best <- predict(
  pist.cart.cp.best, 
  pist.test, 
  type="class"
)
matriz.confusion.best <- confusionMatrix(
  predichos.cart.class.best, 
  as.factor(pist.test$Class), 
  positive = "Kirmizi"
)
matriz.confusion.best$table
