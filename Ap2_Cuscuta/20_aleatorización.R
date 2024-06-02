library(permute)
library(xlsx)

treat <- rep(c("Alfalfa0", "Alfalfa1", "Alfalfa2", "Alfalfa3", "Alfalfa4", "Alfalfa5",
               "Lotus0", "Lotus1", "Lotus2", "Lotus3", "Lotus4", "Lotus5",
               "Trebol0", "Trebol1", "Trebol2", "Trebol3", "Trebol4", "Trebol5"), each = 3)

set.seed(21)
treat <- as.data.frame(treat[shuffle(1:54)])
treat$`Número_de_cuscutas_encontradas` <- rep(as.numeric(c("")), 54)

write.xlsx(treat, "Aleatorización.xlsx", sheetName = "Randomization", 
           col.names = TRUE, row.names = TRUE, append = FALSE)